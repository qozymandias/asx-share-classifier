-- --------------------------------------------------------------------
-- | Main Module
-- --------------------------------------------------------------------
-- |    Module which handles interact with user and converts data to
-- |    Haskell format.
-- --------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.Text as T
import GHC.Generics
import Spectral

-- | Share type:
-- |    JSON representation for converting JSON file into Records
-- |    Haskell can understand.
-- --------------------------------------------------------------------
data Share = Share { name :: !Text 
                   , score :: ![Double]
                   } deriving (Show, Generic)

instance FromJSON Share

instance ToJSON Share

-- | shareToList:
-- |    Converts the list of JSON Record types into a list of pairs
-- |    which is readable by the core function
-- --------------------------------------------------------------------
shareToList :: [Share] -> [(String, (Double, Double))]
shareToList ss = [ let Share{name,score} = x 
                     in ( T.unpack name
                        , ( sum (Prelude.filter (> 50) score)
                          , sum (Prelude.filter (< 50) score)
                        ) )
                 | x <- ss]

-- | main:
-- |    top level function for entire project
-- --------------------------------------------------------------------
main :: IO ()
main = do
    Prelude.putStrLn "Please enter k, sigma and filepath to data points"
    Prelude.putStrLn "Followed by <ENTER>"
    Prelude.putStrLn "e.g. 10 0.25 \"output.json\"\n"
    args <- getLine
    let a = Prelude.words args
    d <- (eitherDecode <$> (getJSON (read (a!!2)::String))) :: IO (Either String [Share])
    case d of
      Left err -> (do 
                    print err )
      Right ps -> (do
                     spectral (read (a!!0)::Int) (read (a!!1)::Double) (shareToList ps))
    where
        getJSON :: FilePath -> IO B.ByteString
        getJSON j = B.readFile j
