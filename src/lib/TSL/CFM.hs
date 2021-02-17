-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.CFM
-- Maintainer  :  Felix Klein
--
-- Control flow model representation.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

-----------------------------------------------------------------------------

module TSL.CFM
  ( Input(..)
  , Output(..)
  , Wire(..)
  , Term(..)
  , Type(..)
  , WireTarget(..)
  , CFM(..)
  , fromCFM
  , fromCircuit
  , symbolTable
  , rmDouble
  , termType
  , prType
  , prTypeFnChain
  , constants
  , predicates
  , functions
  , statistics
  ) where

-----------------------------------------------------------------------------

import GHC.Generics (Generic)

import Data.Maybe (fromMaybe, mapMaybe)

import Data.Either (partitionEithers)

import Control.Arrow ((***))

import Data.List (find, groupBy, sortBy, transpose)

import Data.Function (on)

import Control.Monad (foldM)

import Control.Exception (assert)

import Control.Monad.ST (ST, runST)

import Data.Set
  ( deleteAt
  , difference
  , elemAt
  , empty
  , fromList
  , insert
  , member
  , size
  , toList
  , union
  , unions
  )

import Data.Array (array, (!))

import qualified Data.Array as A (bounds)

import Data.Array.ST
  ( STArray
  , mapArray
  , newArray
  , readArray
  , runSTArray
  , writeArray
  )

import TSL.SymbolTable (IdRec(..), SymbolTable(..))

import qualified TSL.SymbolTable as ST (Kind(..), symbolTable)

import TSL.Logic
  ( FunctionTerm(..)
  , PredicateTerm(..)
  , SignalTerm(..)
  , decodeInputAP
  , decodeOutputAP
  )

import TSL.Error (Error, errFormat)

import TSL.Types (ExprType(..))

import qualified TSL.Types as T (prType)

import TSL.Aiger (Circuit, parseAag)

import qualified TSL.Aiger as Circuit
  ( Circuit(..)
  , Input(..)
  , Invertible(..)
  , Output(..)
  )

import qualified Data.Map.Strict as Map (fromList, member, (!))

import qualified Data.IntMap as IM (fromList, lookup, (!))

-----------------------------------------------------------------------------

type Name = String

-----------------------------------------------------------------------------

newtype Input =
  Input
    { input :: Int
    }
  deriving (Ord, Eq)

instance Show Input where
  show = show . input

-----------------------------------------------------------------------------

newtype Output =
  Output
    { output :: Int
    }
  deriving (Ord, Eq)

instance Show Output where
  show = show . output

-----------------------------------------------------------------------------

newtype Wire =
  Wire
    { wire :: Int
    }
  deriving (Ord, Eq)

instance Show Wire where
  show = show . wire

-----------------------------------------------------------------------------

newtype Term =
  Term
    { term :: Int
    }
  deriving (Ord, Eq)

instance Show Term where
  show = show . term

-----------------------------------------------------------------------------

data Type =
    Boolean
  | Poly Int
  deriving (Show, Ord, Eq, Generic)

-----------------------------------------------------------------------------

data WireTarget =
    SwitchTarget Output
  | TermTarget Term
  | CircuitInputTarget Circuit.Input
  deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------

data CFM =
  CFM
    { -- | inputs signals to the model (including looped
      -- inputs)
      inputs :: [Input]

    , -- | outputs signals from the model (including looped
      -- outputs)
      outputs :: [Output]

    , -- | terms used by the CFM
      terms :: [Term]

    , -- | wires to connect terms with inputs, outputs, and cells
      wires :: [Wire]

    , -- | polymorphic types used by the CFM
      types :: [Type]

    , -- | control circuit that controls the different output
      -- switches
      control :: Circuit

    , -- | wires starting from the inputs signals to the model
      inputWire :: Input -> Wire

    , -- | wires assigned to the inputs of the given function term
      termInputWires :: Term -> [Wire]

    , -- | wire assigned to the output of the given function term
      termOutputWire :: Term -> Wire

    , -- | wires connected to the Boolean inputs of the internal
      -- control circuit
      controlInputWire :: Circuit.Input -> Wire

    , -- | output switches to select the signal selected by the
      -- control circuit
      outputSwitch :: Output -> [(Wire, Circuit.Output)]

    , -- | the unique source of a wire, which is either a direct input
      -- or the output of a term
      wireSource :: Wire -> Either Input Term

    , -- | the targets of a wire, which are either an input to the
      -- control circuit, a term argument or an output switch
      wireTargets :: Wire -> [WireTarget]

    , -- | checks whether the given term is a predicate
      isPredicate :: Term -> Bool

    , -- | checks wheter the given input is looped back from an
      -- output
      loopedInput :: Input -> Bool

    , -- | the list of output / input signal pairs that are connected
      -- by a loop
      loops :: [(Input, Output)]

    , -- | returns the name assigned of the given input signal
      inputName :: Input -> Name

    , -- | returns the name of the given output
      outputName :: Output -> Name

    , -- | returns the name of the given term
      termName :: Term -> Name

    , -- | returns the type of the data transfered via the given wire
      wireType :: Wire -> Type
    }

-----------------------------------------------------------------------------

-- | Returns the number of inputs, the number of outputs, the number
-- of cells, the number predicates, the number of functions, the
-- number of cells, and the number of vertics of a Control Flow Model.

statistics
  :: CFM -> (Int, Int, Int, Int, Int, Int)

statistics cfm@CFM{..} =
  let
    nI = length $ filter (not . loopedInput) inputs
    nC = length $ filter loopedInput inputs
    nP = length (predicates cfm)
    nF = nP + length (functions cfm) + length (constants cfm)
    mC = nC + length (Circuit.latches control)
    mV = length terms + length (Circuit.gates control) + inverters control
  in
    (nI, length outputs, nP, nF, mC, mV)

  where
    inverters Circuit.Circuit{..} =
      let
        oW = fromList $
             map outputWire outputs
        lI = fromList $
             map latchInput latches
        gA = fromList $
             map gateInputA gates
        gB = fromList $
             map gateInputB gates
      in
        length $ mapMaybe isNegative $ toList $ unions [oW,lI,gA,gB]

    isNegative = \case
      Circuit.Negative x -> Just x
      _                  -> Nothing

-----------------------------------------------------------------------------

-- | Creates a symbol table from the CFM.

symbolTable
  :: CFM -> SymbolTable

symbolTable cfm@CFM{..} =
  let
    xs =
      [ ( sourceId $ Left i
        , IdRec
            { idName     = inputName i
            , idType     = t2et [wireType $ inputWire i]
            , idKind     = ST.Input
            , idDeps     = []
            , idArgs     = []
            , idPos      = Nothing
            , idBindings = Nothing
            }
        )
      | i <- inputs
      , not $ loopedInput i
      ]
      ++
      [ ( outputId o
        , IdRec
            { idName     = outputName o
            , idType     = t2et [wireType $ fst $ head $ outputSwitch o]
            , idKind     = ST.Output
            , idDeps     = transitive empty empty $ map fst $ outputSwitch o
            , idArgs     = []
            , idPos      = Nothing
            , idBindings = Nothing
            }
        )
      | o <- outputs
      ]
      ++
      [ ( sourceId $ Right t
        , IdRec
            { idName     = termName t
            , idType     = t2et $ termType cfm t
            , idKind     = ST.Constant
            , idDeps     = []
            , idArgs     = []
            , idPos      = Nothing
            , idBindings = Nothing
            }
        )
      | t <- constants cfm
      ]
      ++
      [ ( sourceId $ Right t
        , IdRec
            { idName     = termName t
            , idType     = t2et $ termType cfm t
            , idKind     = ST.Predicate
            , idDeps     = []
            , idArgs     = []
            , idPos      = Nothing
            , idBindings = Nothing
            }
        )
      | t <- predicates cfm
      ]
      ++
      [ ( sourceId $ Right t
        , IdRec
            { idName     = termName t
            , idType     = t2et $ termType cfm t
            , idKind     = ST.Function
            , idDeps     = []
            , idArgs     = []
            , idPos      = Nothing
            , idBindings = Nothing
            }
        )
      | t <- functions cfm
      ]
  in
    ST.symbolTable $ array (0,length xs - 1) $ sortIds xs

  where
    t2et = \case
      []           -> assert False undefined
      [Boolean]    -> TBoolean
      [Poly x]     -> TPoly x
      Boolean : xr -> TFml TBoolean $ t2et xr
      Poly x : xr  -> TFml (TPoly x) $ t2et xr

    -- | Computes the transitive closure of the dependencies

    transitive is ws = \case
      []     -> toList is
      (x:xr) -> case wireSource x of
        Left i  ->  transitive (insert (sourceId (Left i)) is) (insert x ws) xr
        Right t
          | termName t == "true" || termName t == "false" -> transitive is ws xr
          | otherwise ->
            let ti = fromList $ termInputWires t
            in transitive
                 (insert (sourceId (Right t)) is)
                 (insert x ws)
                 (toList $ difference (ti `union` fromList xr) ws)


    -- | Updates the term ids to redirect the removed, redundant
    -- entries.

    upd =
        (\x y -> fromMaybe y $ IM.lookup y x)
      $ IM.fromList
      $ concatMap
          ((\xs -> map (, head xs) xs) . map (sourceId . Right))
      $ groupBy ((==) `on` termName)
      $ sortBy (compare `on` termName)
      $ filter ((/= "false") . termName)
      $ filter ((/= "true") . termName)
        terms

    -- | Updates ids to appear in sorted order

    sortIds xs =
      let
        -- get the current id ordern
        is = map fst xs

        -- create an update mapping to rearrange them in order
        rearrange =
          (\x y -> assert (Map.member y x) (x Map.! y)) $
          Map.fromList $ zip is [0,1..length is - 1]

        -- create an update function to update the entry
        updEntry (i, x@IdRec{..}) =
          (rearrange i, x { idDeps = map (rearrange . upd)  idDeps })
      in
        -- apply everthing
        map updEntry xs

    -- | Updates the output index with respect to the looped signals.

    updOutputId =
      let
        -- get the non-looped outputs
        os =
          toList $ difference
            (fromList outputs)
            (fromList (map snd loops))
      in
        -- create an update mapping to the compressed range
        (\x y -> assert (Map.member y x) (x Map.! y)) $
        Map.fromList $ zip os [0,1..length os - 1]


    -- | Prints the output id.

    outputId o = case find ((== o) . snd) loops of
      Just (i,_) -> input i
      Nothing    -> length inputs + updOutputId o


    -- | Assigns wire sources their respective id.

    sourceId = \case
      Left i  -> input i
      Right t ->
        let
          n =
            size $ difference
              (fromList outputs)
              (fromList (map snd loops))
        in
          term t + n + length inputs

-----------------------------------------------------------------------------

-- | Parses a Control Flow Model that is given as an AIGER circuit,
-- which has been synthesized from a TLSF specification that
-- (generated from a TSL specification).

fromCFM
  :: String -> Either Error CFM

fromCFM str =
  parseAag str >>= fromCircuit

-----------------------------------------------------------------------------

fromCircuit
  :: Circuit -> Either Error CFM

fromCircuit circuit = do
  -- parse the function terms assigned to the inputs of the circuit
  its <- inputTerms

  -- parse the update terms that are triggered by the outputs of the
  -- circuit
  ots <- outputTerms

  let
    -- get all terms and their subterms
    ts =
        toList
      $ foldl subTerms (foldl subTerms empty $ map PredicateTerm its)
      $ map (snd . fst) ots

    -- separate pure signals from function applications
    cs = partitionEithers $ map classify ts

    -- list of all output signal names
    os = toList $ fromList $ map (fst . fst) ots

    -- names of the input signals
    is = fst cs -- difference (fromList $ fst cs) os

    -- names of all used function terms
    fs = snd cs

    -- assgins a wire to each function term
    tm = Wire . ((Map.fromList $ zip ts [0,1..length ts - 1]) Map.!)

    zs = map (\o -> map (\(x,y) -> (tm $ snd x,y))
                   $ filter ((== o) . fst . fst) ots) os

    -- identify the looped signals
    loops =
      -- extract the input / output pairs from the grouped elements
      map ((head *** head) . partitionEithers . map snd)
      -- filter the signals that appear twice
      $ filter ((== 2) . length)
      -- group on the same signal name
      $ groupBy ((==) `on` fst)
      -- sort the list according to the signal names
      $ sortBy (compare `on` fst)
      -- tag inputs by a left version of their id and outputs by a
      -- right version
      $ zip (map fst is) (map (Left . Input) [0,1..length is - 1]) ++
        zip os (map (Right . Output) [0,1..length os - 1])

    -- collect the looped inputs
    ls = map ((`member` fromList (map fst loops)) . Input)
         [0,1..length is - 1]

    isP =
      (`member` fromList (map PredicateTerm $ filter noBooleanInput its))

  inferTypes $ wireConnections
    CFM
      { inputs           = map Input [0,1..length is - 1]
      , outputs          = map Output [0,1..length os - 1]
      , terms            = map Term [0,1..length fs - 1]
      , wires            = map Wire [0,1..length ts - 1]
      , types            = undefined
      , control          = circuit
      , inputWire        = aMap (map (tm . (\(x,c) -> c x)) is) . input
      , termInputWires   = aMap (map (map tm . stArgs) fs) . term
      , termOutputWire   = aMap (map tm fs) . term
      , controlInputWire = aMap (map (tm . PredicateTerm) its) . Circuit.input
      , outputSwitch     = aMap zs . output
      , wireSource       = undefined
      , wireTargets      = undefined
      , isPredicate      = aMap (map isP fs) . term
      , loopedInput      = aMap ls . input
      , loops            = loops
      , inputName        = aMap (map fst is) . input
      , outputName       = aMap os . output
      , termName         = aMap (map stName fs) . term
      , wireType         = undefined
      }

  where
    -- | Extracts the arguments of a function or predicate term
    -- wrapped into the 'SignalTerm' constructor.

    stArgs = \case
      FunctionTerm f  -> ftArgs [] f
      PredicateTerm p -> ptArgs [] p
      Signal {}       -> assert False undefined


    -- | Extracts the arguments of a function term.

    ftArgs a = \case
      FunctionSymbol {} -> reverse a
      FApplied f x      -> ftArgs (x:a) f


    -- | Extracts the arguments of a predicate term.

    ptArgs a = \case
      PApplied p x -> ptArgs (x:a) p
      _            -> reverse a

    -- | Extracts the name of a function or predicate term wrapped
    -- into the 'SignalTerm' constructor.

    stName = \case
      FunctionTerm f  -> ftName f
      PredicateTerm p -> ptName p
      Signal {}       -> assert False undefined


    -- | Extracts the name of a function term.

    ftName = \case
      FunctionSymbol x -> x
      FApplied f _     -> ftName f


    -- | Extracts the name of a predicate term.

    ptName = \case
      BooleanTrue       -> "true"
      BooleanFalse      -> "false"
      BooleanInput x    -> x
      PredicateSymbol x -> x
      PApplied p _      -> ptName p


    -- | Creates an efficient index mapping to access elements of a
    -- fixed given list via their indices.

    aMap xs =
      let a = (array (0, length xs - 1) $ zip [0,1..length xs - 1] xs)
      in \x -> let (l,u) = A.bounds a in assert (x >= l && x <= u) (a ! x)

    -- | Parses the predicate terms from the circuit input names.

    inputTerms =
      mapM (decodeInputAP . Circuit.inputName circuit)
        $ Circuit.inputs circuit

    -- | Parses the update terms from the circuit output names.

    outputTerms = do
      xs <-
        mapM (decodeOutputAP . Circuit.outputName circuit)
          $ Circuit.outputs circuit

      return $ zip xs $ Circuit.outputs circuit

    -- | Adds all sub-terms of a given term to the given set.

    subTerms a = \case
      Signal x        -> insert (Signal x) a
      FunctionTerm t  -> subTermsF (insert (FunctionTerm t) a) t
      PredicateTerm t -> subTermsP (insert (PredicateTerm t) a) t

    subTermsF a = \case
      FunctionSymbol _ -> a
      FApplied f x     -> subTermsF (subTerms a x) f

    subTermsP a = \case
      PApplied f x -> subTermsP (subTerms a x) f
      _            -> a

    -- | Classifies function terms int pure signals and larger
    -- constructed terms.

    classify = \case
      Signal x                       -> Left (x, Signal)
      PredicateTerm (BooleanInput i) -> Left (i, PredicateTerm . BooleanInput)
      PredicateTerm t                -> Right $ PredicateTerm t
      FunctionTerm t                 -> Right $ FunctionTerm t

    noBooleanInput = \case
      BooleanInput {} -> False
      _               -> True

-----------------------------------------------------------------------------

-- | Creates the mappings assigning each wire their source and targets.

wireConnections
  :: CFM -> CFM

wireConnections cfm@CFM{..} =
  let
    wTA =
      (!)
      -- create an array out of the mapping
      $ array (0, length wires - 1)
      -- rearrange the tuple layout to reduce redundency
      $ map (\xs -> (fst $ head xs, map snd xs))
      -- group entries with the same wire togehter
      $ groupBy ((==) `on` fst)
      -- sort the entries on equal wires
      $ sortBy (compare `on` fst)
      -- collect the input wires of all terms
      $ [ (wire w, TermTarget t)
        | t <- terms
        , w <- termInputWires t
        ]
        ++
        -- collect the input wires of the boolean cirucit (this is
        -- actually never needed, but it is added for the sake of
        -- completeness)
        [ (wire $ controlInputWire i, CircuitInputTarget i)
        | i <- Circuit.inputs control
        ]
        ++
        -- collect the input wires of all output switches
        [ (wire w, SwitchTarget o)
        | o <- outputs
        , (w, _) <- outputSwitch o
        ]

    wSA =
      (!)
      -- create an array out of the mapping
      $ array (0, length wires - 1)
      -- sort the entries on equal wires
      $ sortBy (compare `on` fst)
      -- collect all inputs an the wires associated with them
      $ [ (wire $ inputWire i, Left i)
        | i <- inputs
        ]
        ++
        -- collect all terms and the wires connected to their output
        [ (wire $ termOutputWire t, Right t)
        | t <- terms
        ]
  in
    cfm
      { wireTargets = wTA . wire
      , wireSource = wSA . wire
      }

-----------------------------------------------------------------------------

-- | Infers the wire types

inferTypes
  :: CFM -> Either Error CFM

inferTypes cfm@CFM{..} = do
  -- check that all terms with equal name have the same arith
  mapM_ checkArity
    $ toList $ fromList $ map termName terms

  -- rearrange and count polymorphic types
  return $ rearrange cfm { wireType = wireType' . wire }

  where
    -- | Rearranges polymorph type ids to appear in order in the type
    -- table

    rearrange c@CFM{..} =
      let
        ps =
            rmDouble
          $ mapMaybe isPoly
          $ map (wireType' . wire . inputWire) (filter (not . loopedInput) inputs) ++
            map (wireType' . wire . fst . head . outputSwitch ) outputs ++
            concatMap (termType c) (constants c) ++
            concatMap (termType c) (predicates c) ++
            concatMap (termType c) (functions c)

        f = (IM.!) $ IM.fromList $ zip ps [0,1..]

        upd = \case
          Boolean -> Boolean
          Poly i  -> Poly $ f i
      in
        c { types = toList $ fromList $ map (upd . wireType' . wire) wires
          , wireType = upd . wireType' . wire
          }

    -- | Marks polymorphic identifiers.

    isPoly = \case
      Boolean -> Nothing
      Poly i  -> Just i


    -- | Checks that all terms of the given name have the same arity.

    checkArity name =
      case toList $ fromList $ map (length . termInputWires) $ terms' name of
        [_] -> return ()
        _   -> errFormat $
                "The term \"" ++ name ++ "\" appears multiple times " ++
                "with a different number of arguments."


    -- | Assigns each wire a type.

    wireType' =
      (!) $ runSTArray $ do
         -- create a new array, initally assigning each wire some type
         a <- newArray (0, length wires - 1) Boolean
         -- update every wire by a poly type with the same index
         mapM_ (\w -> writeArray a w $ Poly w)
           [0,1..length wires - 1]
         -- update circuit inputs by a Boolean type
         mapM_ (\i -> writeArray a (wire $ controlInputWire i) Boolean)
           $ Circuit.inputs control
         -- check whether the constants 'true' and 'false' are used and
         -- assign them a boolean type
         mapM_ (\i -> writeArray a (wire $ termOutputWire i) Boolean)
           $ filter knownConstant terms

         -- update the type assignment until we reach a fixpoint, which
         -- must exist as we always join equally typed wires to the
         -- minimal type; note that it suffices to iterate over the terms
         -- only, since model inputs cannot influence the type table
         -- actively.
         infer a $ fromList wires
         -- compress the index range
         compressed a


    -- | Indicates that a given term is one of the known constants
    -- (true, false)

    knownConstant t =
      null (termInputWires t) && (termName t == "true") || (termName t == "false")


    -- | Compresses the index range and reorders it according to the
    -- different component types.

    compressed a = do
      let
        -- input signal wires
        is = map inputWire $ sortBy cmpI inputs
        -- predicate wires
        ps =
            map termOutputWire
          $ sortBy (compare `on` termName)
          $ filter isPredicate terms
        -- non-predicate wires
        fs =
            map termOutputWire
          $ sortBy (compare `on` termName)
          $ filter (not . isPredicate) terms

      -- get the list of all types in the right order
      xs <- mapM (readArray a . wire) $ is ++ ps ++ fs

      let
        -- remove Boolean entries, double entries, and the constructor
        -- wrappers
        ts =
            rmDouble
          $ map (\(Poly x) -> x)
          $ filter (/= Boolean) xs


        -- create an update mapping to the compressed range
        updPoly =
          (\x y -> assert (Map.member y x) (x Map.! y)) $ Map.fromList $ zip ts [0,1..length ts - 1]

        upd = \case
          Boolean -> Boolean
          Poly i  -> Poly $ updPoly i

      mapArray upd a


    -- | Returns the list of terms that have the given name.

    terms' =
        (\x y -> assert (Map.member y x) (x Map.! y))
      -- crate the mapping
      $ Map.fromList
      -- rearrange the layout to be of the right type
      $ map (\xs -> (termName $ head xs, xs))
      -- group all entries with the same name together
      $ groupBy ((==) `on` termName)
      -- sort the entries on equal names
      $ sortBy (compare `on` termName) terms


    -- | Update the given array assigment 'a' by infering equal types
    -- of each component until a fixpoint is reached.

    infer a s
      | size s == 0 = return ()
      | otherwise  =
          -- update every component with the given wire as input
          foldM (inferWire a) (deleteAt 0 s) (wireTargets $ elemAt 0 s)
          -- do so until the types for every wire stabilized
          >>= infer a


    -- | Infers equal types with respect to the given component.

    inferWire a s = \case
      -- circuit inputs already have type Boolean, so they won't
      -- change any more
      CircuitInputTarget _ -> return s
      -- all inputs to an output switch have equal type
      SwitchTarget o -> equalType a s $ map fst $ outputSwitch o
      -- output types are equal, as well as argument types
      TermTarget t -> do
        -- get the other terms using the same name
        let ts = terms' $ termName t
        -- the output types of all those terms must be equal
        s' <- equalType a s $ map termOutputWire ts
        -- the types of arguments at the same positions must be equal
        foldM (equalType a) s' $ transpose $ map termInputWires ts


    -- | Assigns all wires of the given list the same type, which is
    -- the minimal type of all those elements. Every wire that is
    -- updated is re-inserted to the workset.

    equalType a s = \case
      []   -> return s
      x:xr -> do
        -- get some initial type
        t <- readArray a $ wire x
        -- get the remaining types
        ts <- mapM (readArray a . wire) xr
        -- select the minimal element and update the remaining
        -- elements
        foldM (update a (foldl minType t ts)) s (x:xr)


    -- | Udates the given array 'a' at position 'w' by the minimal
    -- type 'tmin' if the type at that position is not alreay 'tmin'.
    -- If an update is executed, i.e., the type value has changed, then
    -- the updated wire position is re-inserted into the workset 's'.

    update a tmin s w = do
      -- get the current type
      t <- readArray a $ wire w
      -- update if not already the minimal element
      if t == tmin
      then return s
      else do
        -- update to the minimal element
        writeArray a (wire w) tmin
        -- insert the updated element to the workqueue
        return $ insert w s


    -- | Returns the minimum of two given types

    minType (Poly i) (Poly j) = Poly $ min i j
    minType _        _        = Boolean


    -- | Comparison function to sort inputs such that looped inputs
    -- are moved to the end
    cmpI i j =
      let s = fromList $ map fst loops
      in case (i `member` s, j `member` s) of
        (True, False) -> GT
        (False, True) -> LT
        _             -> compare (inputName i) (inputName j)

-----------------------------------------------------------------------------

constants
  :: CFM -> [Term]

constants CFM{..} =
    map head
  $ groupBy ((==) `on` termName)
  $ sortBy (compare `on` termName)
  $ filter ((/= "false") . termName)
  $ filter ((/= "true") . termName)
  $ filter (null . termInputWires) terms

-----------------------------------------------------------------------------

predicates
  :: CFM -> [Term]

predicates CFM{..} =
    map head
  $ groupBy ((==) `on` termName)
  $ sortBy (compare `on` termName)
  $ filter isPredicate
  $ filter (not . null . termInputWires) terms

-----------------------------------------------------------------------------

functions
  :: CFM -> [Term]

functions CFM{..} =
    map head
  $ groupBy ((==) `on` termName)
  $ sortBy (compare `on` termName)
  $ filter (not . isPredicate)
  $ filter (not . null . termInputWires) terms

-----------------------------------------------------------------------------

-- | Returns the type chain of the term application.

termType
  :: CFM -> Term -> [Type]

termType CFM{..} t =
    reverse
  $ ((wireType $ termOutputWire t) :)
  $ map wireType
  $ termInputWires t

-----------------------------------------------------------------------------

-- | Printer for the restricted version of 'CFM' types.

prType
  :: Type -> String

prType =
  T.prType id . t2et

  where
    t2et = \case
      Boolean -> TBoolean
      Poly i  -> TPoly i

-----------------------------------------------------------------------------

-- | Printer for application chains of the restricted version of 'CFM'
-- types.

prTypeFnChain
  :: [Type] -> String

prTypeFnChain = \case
  []   -> assert False undefined
  t:tr -> prType t ++ concatMap ((" -> " ++) . prType) tr

-----------------------------------------------------------------------------

-- | Removes double entries of an 'Int' list without distorting the
-- order by using a lookup table.

rmDouble
  :: [Int] -> [Int]

rmDouble xs =
  let m = maximum xs
  in runST $ do
    a <- newArray (0, m) False
    ys <- mapM (markDouble a) xs
    return $ map fst $ filter snd ys

  where
    markDouble
      :: STArray s Int Bool -> Int -> ST s (Int, Bool)

    markDouble a x = do
      y <- readArray a x
      if y
      then return (x, False)
      else do
        writeArray a x True
        return (x, True)

-----------------------------------------------------------------------------
