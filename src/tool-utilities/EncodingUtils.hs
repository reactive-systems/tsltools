----------------------------------------------------------------------------
-- |
-- Module      :  EncodingUtils
-- Maintainer  :  Marvin Stegner (Stenger@ProjectJARVIS.de)
--
-- This module implements encoding utilities that can are used in
-- the executables of tsltools.
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
module EncodingUtils
  ( initEncoding
  ) where

-----------------------------------------------------------------------------

import GHC.IO.Encoding
  ( utf8
  , setLocaleEncoding
  , setFileSystemEncoding
  , setForeignEncoding
  )

import System.IO
  ( BufferMode(..)
  , hSetBuffering
  , stderr
  , stdout
  )

-----------------------------------------------------------------------------
-- | 'initEncoding' initializes the standard encoding
initEncoding :: IO ()
initEncoding = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8

-----------------------------------------------------------------------------