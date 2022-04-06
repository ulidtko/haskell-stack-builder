{-# LANGUAGE TypeApplications #-}
module Main where
import Data.ByteString hiding (putStrLn)
import Data.Text
import Data.Text.Conversions
import Text.CaseConversion
import THmodUseSite (staticSplice)
main :: IO ()
main = do
    print $ fromText @(Maybe (Base16 ByteString)) "68656c6c6f3f2021"
    print $ toSnakeCase ["hello", "Stack", "wOrLd"]
    print staticSplice
    print @Text "* Test passed! *"
