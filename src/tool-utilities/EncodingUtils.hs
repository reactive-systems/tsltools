----------------------------------------------------------------------------
-- |
-- Module      :  EncodingUtils
-- Maintainer  :  Marvin Stenger
--
-- This module implements encoding utilities that can be used in
-- the executables of tsltools.
--
-----------------------------------------------------------------------------

module EncodingUtils
  ( initEncoding
  ) where

-----------------------------------------------------------------------------

import GHC.IO.Encoding
  ( setFileSystemEncoding
  , setForeignEncoding
  , setLocaleEncoding
  , utf8
  )

import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)

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
