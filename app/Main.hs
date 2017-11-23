module Main where

import Lib
import Options.Applicative
import Data.Semigroup ((<>))

type ItemIndex = Int
type ItemDescription = Maybe String

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item") 

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

data Options = Options FilePath ItemIndex ItemDescription deriving Show

optionsParser :: Parser Options
optionsParser = Options <$> dataPathParser <*> itemIndexParser <*> updateItemDescriptionParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser = Just <$> 
                         itemDescriptionValueParser <|> 
                         flag' Nothing (long "clear-desc")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser = strOption (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

dataPathParser :: Parser FilePath
dataPathParser = strOption $
        value  defaultDataPath
        <> long "data-path"
        <> short 'p'
        <> metavar "DATAPATH"
        <> help ("path to data file (default " ++ defaultDataPath ++ ")")

main :: IO ()
main = do
      options <- execParser (info (optionsParser) ( progDesc "To-Do list manager"))
      putStrLn $ "options = " ++ show options 
      {-datapath <- execParser (info (dataPathParser) ( progDesc "To-Do list manager"))-}
      {-putStrLn $ "datapath = " ++ show datapath-}
   {-itemIndex <- execParser (info (itemIndexParser <**> helper) (progDesc "To-Do list manager" <> header "--------------"))-}
   {-putStrLn $ "itemIndex=" ++ show itemIndex-}

