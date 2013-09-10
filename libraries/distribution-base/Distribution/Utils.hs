{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
--                portions Copyright (c) 2007, Galois Inc.
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A large and somewhat miscellaneous collection of utility functions used
-- throughout the rest of the Cabal lib and in other tools that use the Cabal
-- lib like @cabal-install@. It has a very simple set of logging actions. It
-- has low level functions for running programs, a bunch of wrappers for
-- various directory and file functions that do extra logging.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Utils (
        -- * Unicode
        normaliseLineEndings,

        -- * generic utils
        equating,
        comparing,
        isInfixOf,
        intercalate,
        lowercase
  ) where

import Data.List
    (isPrefixOf, tails, intercalate )
import Data.Char as Char
    ( toLower )




-- | Fix different systems silly line ending conventions
normaliseLineEndings :: String -> String
normaliseLineEndings [] = []
normaliseLineEndings ('\r':'\n':s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r':s)      = '\n' : normaliseLineEndings s -- old osx
normaliseLineEndings (  c :s)      =   c  : normaliseLineEndings s

-- ------------------------------------------------------------
-- * Common utils
-- ------------------------------------------------------------

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y

comparing :: Ord a => (b -> a) -> b -> b -> Ordering
comparing p x y = p x `compare` p y

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

lowercase :: String -> String
lowercase = map Char.toLower
