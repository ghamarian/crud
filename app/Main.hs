{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Lib
import qualified System.IO.Strict as S
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics
import qualified Data.Yaml as Yaml
import Options.Applicative hiding (infoParser)
import Control.Exception
import Data.String.Utils
import System.IO.Error
import System.Directory

type ItemIndex       = Int
type ItemTitle       = String
type ItemDescription = Maybe String
type ItemPriority    = Maybe String
type ItemDueBy       = Maybe String

data ItemUpdate = ItemUpdate { titleUpdate :: Maybe ItemTitle
                             , descriptionUpdate :: Maybe ItemDescription
                             , priorityUpdate :: Maybe ItemPriority
                             , dueByUpdate :: Maybe ItemDueBy } deriving Show

data Options = Options FilePath Command deriving Show

data Command = Info 
             | List 
             | Init 
             | View  ItemIndex
             | Update ItemIndex ItemUpdate 
             | Remove ItemIndex 
             | Add Item
             deriving Show

data Item = Item {  title :: ItemTitle
                  , description:: ItemDescription
                  , priority:: ItemPriority
                  , dueBy:: ItemDueBy } deriving (Show, Generic)

instance ToJSON Item
instance FromJSON Item
   

data ToDoList = ToDoList  [Item] deriving (Generic, Show)

instance ToJSON ToDoList
instance FromJSON ToDoList

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item") 

itemTitleValueParser :: Parser String
itemTitleValueParser = strOption (long "title" <> short 't'  <> metavar "TITLE" <> help "title")

itemPriorityValueParser :: Parser String
itemPriorityValueParser = strOption (long "priority" <> short 'p'  <> metavar "PRIORITY" <> help "priority")

itemDueByValueParser :: Parser String
itemDueByValueParser = strOption (long "due-by" <> short 'd'  <> metavar "DUE-BY" <> help "due-by")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser = strOption (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

optionsParser :: Parser Options
optionsParser = Options <$> dataPathParser <*> commandParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser = Just 
                         <$> itemDescriptionValueParser 
                         <|> flag' Nothing (long "clear-desc")

updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser = Just 
                          <$> itemPriorityValueParser
                          <|> flag' Nothing (long "clear-priority")

updateItemDueByParser   :: Parser ItemPriority
updateItemDueByParser  = Just 
                          <$> itemDueByValueParser
                          <|> flag' Nothing (long "clear-due-by")
                          
updateItemTitleParser :: Parser ItemTitle 
updateItemTitleParser = itemTitleValueParser

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> addItemParser

viewParser :: Parser Command
viewParser =  View <$> itemIndexParser

updateParser :: Parser Command
updateParser = Update  <$> itemIndexParser <*> updateItemParser 

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate 
                  <$> optional updateItemTitleParser 
                  <*> optional updateItemDescriptionParser 
                  <*> optional updateItemPriorityParser  
                  <*> optional updateItemDueByParser 

addItemParser :: Parser Item
addItemParser = Item
                  <$> argument str (metavar "TITLE" <> help "title")
                  <*> optional itemDescriptionValueParser 
                  <*> optional itemPriorityValueParser  
                  <*> optional itemDueByValueParser 

removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

commandParser :: Parser Command
commandParser = subparser $ mconcat 
          [command "info" (info infoParser (progDesc "Show info"))
          ,command "init" (info initParser (progDesc "Initialize items"))
          ,command "list" (info listParser (progDesc "List all items"))
          ,command "add" (info addParser (progDesc "Add item"))
          ,command "view" (info viewParser (progDesc "View item"))
          ,command "update" (info updateParser (progDesc "Update item"))
          ,command "remove" (info removeParser (progDesc "Remove item"))
          ]

dataPathParser :: Parser FilePath
dataPathParser = strOption $
        value  defaultDataPath
        <> long "data-path"
        <> short 'p'
        <> metavar "DATAPATH"
        <> help ("path to data file (default " ++ defaultDataPath ++ ")")

run :: FilePath -> Command -> IO ()
run datapath Info = putStrLn "Info"
run datapath Init = putStrLn "Init"
run datapath (Update idx itemUpdate) = putStrLn $ "Update: idx= " ++ show idx ++ " itemupdate= " ++ show itemUpdate
run datapath (View idx) = putStrLn $ "View idx= " ++ show idx
run datapath (Add item) = putStrLn $ "Add: item= " ++ show item
run datapath List = putStrLn "List"
run datapath (Remove idx) = putStrLn $ "Remove idx= " ++ show idx

writeToDoList :: FilePath -> ToDoList -> IO ()
writeToDoList dataPath todoList = BS.writeFile dataPath (Yaml.encode todoList)

readToDoList :: FilePath -> IO ToDoList
readToDoList dataPath =  do
         maybeTodoList <- catchJust 
                        (\e -> if isDoesNotExistError e then Just () else Nothing)
                        (BS.readFile dataPath >>= return . Yaml.decode)
                        (\_ -> return $ Just (ToDoList []))
         case maybeTodoList of
              Nothing -> error "Yaml file is corrupt"
              Just todolist -> return todolist

main :: IO ()
main = do
      Options datapath command <- execParser (info (optionsParser) ( progDesc "To-Do list manager"))
      homeDir <- getHomeDirectory
      let expandedDataPath = replace "~" homeDir datapath
      writeToDoList expandedDataPath (ToDoList [Item "the-name" (Just "the-description") (Just "prio1") (Just "dueBye1"),
                                       Item "the-name2" (Just "the-description2") (Just "prio2") (Just "dueBye2")])
      todo <- readToDoList expandedDataPath 
      putStrLn $ show todo
