{-# LANGUAGE TemplateHaskell #-}

-- | "GHC stage restriction" stipulates that definitions of TH values
-- and their use sites must be split across separate modules.
module THmodUseSite where

import Data.ByteString.Short

import THmod (mkThHello)

staticSplice :: ShortByteString
staticSplice = $(mkThHello)
