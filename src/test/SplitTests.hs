----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Maintainer  :  Gideon Geier
--
-- Splitting Test cases.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module SplitTests
  ( tests
  ) where

-----------------------------------------------------------------------------

import Distribution.TestSuite
  ( Progress(..)
  , Result(..)
  , Test(..)
  , TestInstance(..)
  )

import TSL (Specification, fromTSL, split, toTSL)

import Data.Char (isSpace)

import Data.Set as Set (fromList, (\\))

-----------------------------------------------------------------------------

tests :: [Test]
tests =
  zipWith genSplitTest [1..] splitTestsRaw


genSplitTest :: Int -> (String, [String]) -> Test
genSplitTest i (spec, splits) =
  let
    x =
      TestInstance
        { run = do
            fromTSL Nothing spec
            >>= \case
              Left _ -> do
                putStrLn $ "Incorrect Specification:\n\n" ++ spec
                return $ Finished $ Fail $
                  "Split test " ++ show i ++ " failed."
              Right s -> case splitTest' s splits of
                Right () -> return $ Finished Pass
                Left err -> do
                  putStrLn err
                  return $ Finished $ Fail $
                    "Split test " ++ show i ++ " failed."
        , name = "split" ++ show i
        , tags = []
        , options = []
        , setOption = \_ _ -> Right x
        }
  in
    Test x

  where
    splitTest' :: Specification -> [String] -> Either String ()
    splitTest' spec splitsExpected =
      let
        setExpected = Set.fromList $ map trim splitsExpected
        nExpected = length setExpected

        splitsGot = split spec
        setGot = Set.fromList $ map (trim . toTSL) splitsGot
        nGot = length setGot
      in
      if nGot /= nExpected
      then
        Left $ "Expected " ++ show nExpected ++
                " many splits, but got " ++
                show nGot ++ "."
      else
        if setGot /= setExpected
        then
          let
            expected = show $ setExpected \\ setGot
            got = show $ setGot \\ setExpected
          in
          Left $ "Expected:\n\n" ++ expected ++ "\nbut Got:\n\n" ++ got ++ "\n"
        else Right ()

    trim = f . f
    f = reverse . dropWhile isSpace


splitTestsRaw
  :: [(String, [String])]

splitTestsRaw =
  [ ( unlines
        [ "always assume {"
        , "    F a;"
        , "    !(a && c)"
        , "}"
        , ""
        , "always guarantee {"
        , "// asöldkfalöskdf"
        , "[out <- b()] U a; //asdflök"
        , "c <-> [out2 <- fun a]"
        , "//asdflkj"
        , "}"
        ]

    , [ unlines
          [ "assume {"
          , "  (G (F a));"
          , "  (G (! (a && c)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (c <-> [out2 <- (fun a)]));"
          , "}"
          ]
      , unlines
          [ "assume {"
          , "  (G (F a));"
          , "  (G (! (a && c)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G ([out <- b()] U a));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " //should be split in a, b, c, f and d, e and g"
        , "guarantee{"
        , "    !([a <- c1()] && [b <- c1()])"
        , "}"
        , "always guarantee {"
        , "    in1 -> [a <- c1()];"
        , "    [a <- c2()] <-> [c <- c3()];"
        , "    in2 && in3 -> ([d <- fun in2] || [e <- e]);"
        , "    in2 || in4 <-> [e <- fun in3];"
        , "    [a <- c2()] || [b <- c5()];"
        , "    [c <- c3()] <-> [f <- c6()];"
        , "    (in2 || in3 || in4) -> [g <- c3()];"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (((in2 || in3) || in4) -> [g <- c3()]));"
          , "}"
          ]
      , unlines
          [ " assume {"
          , "}"
          , ""
          , "guarantee {"
          , "  (G ((in2 && in3) -> ([d <- (fun in2)] || [e <- e])));"
          , "  (G ((in2 || in4) <-> [e <- (fun in3)]));"
          , "}"
          ]
      , unlines
          [ " assume {"
          , "}"
          , ""
          , "guarantee {"
          , "  (! ([a <- c1()] && [b <- c1()]));"
          , "  (G (in1 -> [a <- c1()]));"
          , "  (G ([a <- c2()] <-> [c <- c3()]));"
          , "  (G ([a <- c2()] || [b <- c5()]));"
          , "  (G ([c <- c3()] <-> [f <- c6()]));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " /*  Specification with several dependencies"
        , "    a - b - c - f"
        , "    d - e"
        , "    g"
        , "    not splittable when overapproximating assumption dependencies"
        , "*/"
        , "assume  {"
        , "    in2 && !in4;"
        , "}"
        , "always assume {"
        , "    [c <- c3()] -> F in3; // dep c - in3"
        , "}"
        , "guarantee{"
        , "    !([a <- c1()] && [b <- c1()]) // dep a - b"
        , "}"
        , "always guarantee {"
        , "    in1 -> [a <- c1()];"
        , "    [a <- c2()] <-> [c <- c3()]; // dep a - c"
        , "    in2 && in3 -> ([d <- fun in2] || [e <- e]); // dep d - e"
        , "    in2 || in4 <-> [e <- fun in3];"
        , "    [a <- c2()] || [b <- c5()]; // dep a - b"
        , "    [c <- c3()] <-> [f <- c6()]; // dep c - f"
        , "    (in2 || in3 || in4) -> [g <- c3()];"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "  (in2 && (! in4));"
          , "  (G ([c <- c3()] -> (F in3)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (! ([a <- c1()] && [b <- c1()]));"
          , "  (G (in1 -> [a <- c1()]));"
          , "  (G ([a <- c2()] <-> [c <- c3()]));"
          , "  (G ((in2 && in3) -> ([d <- (fun in2)] || [e <- e])));"
          , "  (G ((in2 || in4) <-> [e <- (fun in3)]));"
          , "  (G ([a <- c2()] || [b <- c5()]));"
          , "  (G ([c <- c3()] <-> [f <- c6()]));"
          , "  (G (((in2 || in3) || in4) -> [g <- c3()]));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " // This specification should be split in three parts: b, c, a"
        , "always assume {"
        , "}"
        , "always guarantee {"
        , "    in1 -> [a <- c1()] U in3;"
        , "    F [a <- c4()];"
        , "    in1 <-> [c <- c3()];"
        , "    [c <- c3()] || [c <- c2()];"
        , "    in3 <-> [b <- fun a];"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (in1 <-> [c <- c3()]));"
          , "  (G ([c <- c3()] || [c <- c2()]));"
          , "}"
          ]
      , unlines
          [ " assume {"
          , "}"
          , ""
          , "guarantee {"
          , "  (G ((in1 -> [a <- c1()]) U in3));"
          , "  (G (F [a <- c4()]));"
          , "  (G (in3 <-> [b <- (fun a)]));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " // This specification should be split in three parts: b, c, a"
        , "always assume {"
        , "}"
        , "always guarantee {"
        , "    in1 -> [a <- c1()] U in3;"
        , "    F [a <- c4()];"
        , "    in1 <-> [c <- c3()];"
        , "    [c <- c3()] || [c <- c2()];"
        , "    in3 <-> [b <- fun a];"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (in1 <-> [c <- c3()]));"
          , "  (G ([c <- c3()] || [c <- c2()]));"
          , "}"
          ]
      , unlines
          [ " assume {"
          , "}"
          , ""
          , "guarantee {"
          , "  (G ((in1 -> [a <- c1()]) U in3));"
          , "  (G (F [a <- c4()]));"
          , "  (G (in3 <-> [b <- (fun a)]));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " // This specification should not be split"
        , "always assume {"
        , "    ([a <- val1()] && [b <- val2()]) -> X in1;"
        , "}"
        , ""
        , "always guarantee {"
        , "    F in1;"
        , ""
        , "    F [a <- val2()];"
        , "    F [b <- val1()];"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "  (G (([a <- val1()] && [b <- val2()]) -> (X in1)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (F in1));"
          , "  (G (F [a <- val2()]));"
          , "  (G (F [b <- val1()]));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " // This specification should not be split"
        , "always assume {"
        , "    ([a <- val1()] && [b <- val2()]) -> X in1;"
        , "}"
        , ""
        , "always guarantee {"
        , "    in1 <-> [c <- val3];"
        , "    F [c <- val3];"
        , "    F [a <- val2()];"
        , "    F [b <- val1()];"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "  (G (([a <- val1()] && [b <- val2()]) -> (X in1)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (in1 <-> [c <- val3]));"
          , "  (G (F [c <- val3]));"
          , "  (G (F [a <- val2()]));"
          , "  (G (F [b <- val1()]));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " // This specification should not be split"
        , "always assume {"
        , "    ([a <- val1()] && [b <- val2()]) -> X in1;"
        , "    in1 -> X in2;"
        , "}"
        , ""
        , "always guarantee {"
        , "    F in1;"
        , "    F in2;"
        , "    F [a <- val2()];"
        , "    F [b <- val1()];"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "  (G (([a <- val1()] && [b <- val2()]) -> (X in1)));"
          , "  (G (in1 -> (X in2)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (F in1));"
          , "  (G (F in2));"
          , "  (G (F [a <- val2()]));"
          , "  (G (F [b <- val1()]));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " //this specification should not be split"
        , "always assume {"
        , "    ([a <- val1()] && [b <- val2()]) -> X in1;"
        , "    [c <- val3()] -> X in2;"
        , "}"
        , ""
        , "always guarantee {"
        , "    F in1;"
        , "    F in2;"
        , "    !(in1 && in2);"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "  (G (([a <- val1()] && [b <- val2()]) -> (X in1)));"
          , "  (G ([c <- val3()] -> (X in2)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (F in1));"
          , "  (G (F in2));"
          , "  (G (! (in1 && in2)));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " // this specification should be split in two parts: a,b and c "
        , "always assume {"
        , "    ([a <- val1()] && [b <- val2()]) -> X in1;"
        , "    [c <- val3()] -> X in2;"
        , "    F !in3;"
        , "}"
        , ""
        , "always guarantee {"
        , "    F in1;"
        , "    F in2;"
        , "    "
        , "    in3 -> X !in1;"
        , "    in3 -> X !in2;"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "  (G (F (! in3)));"
          , "  (G ([c <- val3()] -> (X in2)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (F in2));"
          , "  (G (in3 -> (X (! in2))));"
          , "}"
          ]
      , unlines
          [ " assume {"
          , "  (G (F (! in3)));"
          , "  (G (([a <- val1()] && [b <- val2()]) -> (X in1)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (F in1));"
          , "  (G (in3 -> (X (! in1))));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ " // should be split in two parts: out and out2"
        , "always assume {"
        , "    !(a && c)"
        , "}"
        , ""
        , "always guarantee {"
        , "    [out <- b()] U a;"
        , "    c <-> [out2 <- fun a];"
        , "}"
        ]
    , [ unlines
          [ " assume {"
          , "  (G (! (a && c)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G (c <-> [out2 <- (fun a)]));"
          , "}"
          ]
      , unlines
          [ " assume {"
          , "  (G (! (a && c)));"
          , "}"
          , ""
          , "guarantee {"
          , "  (G ([out <- b()] U a));"
          , "}"
          ]
      ]
    )
  , ( unlines
        [ "assume {"
        , "    (G in1 -> in2);"
        , "    ((! G in1) -> in3);"
        , "    (in2 <-> in4);"
        , "    (in3 <-> !in4)"
        , "}"
        , ""
        , "guarantee {"
        , "    (G in1) <-> [out <- top()];"
        , "}"
        ]
    , [ unlines
        [ "assume {"
        , "  ((G in1) -> in2);"
        , "  ((! (G in1)) -> in3);"
        , "  (in2 <-> in4);"
        , "  (in3 <-> (! in4));"
        , "}"
        , ""
        , "guarantee {"
        , "  ((G in1) <-> [out <- top()]);"
        , "}"
        ]
      ]
    )
  ]

-----------------------------------------------------------------------------
