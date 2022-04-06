{-# LANGUAGE TemplateHaskell #-}

-- | This module uses an external library in a TH splice.
--
-- The goal is to test whether our builder deals with it.
--
module THmod where

import Language.Haskell.TH

import Data.ByteString.Short.Base64

mkThHello :: Q Exp
mkThHello = [|decodeBase64Lenient "SGVsbG8gVEghCg=="|]
