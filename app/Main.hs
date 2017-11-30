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
import Data.Time
import Data.List.Safe((!!))
import Prelude hiding ((!!))


type ItemIndex       = Int
type ItemTitle       = String
type ItemDescription = Maybe String
type ItemPriority    = Maybe Priority
type ItemDueBy       = Maybe LocalTime

data ItemUpdate = ItemUpdate { titleUpdate       :: Maybe ItemTitle
                             , descriptionUpdate :: Maybe ItemDescription
                             , priorityUpdate    :: Maybe ItemPriority
                             , dueByUpdate       :: Maybe ItemDueBy } deriving Show

data Options = Options FilePath Command deriving Show

data Priority = Low | Normal | High deriving (Show,  Generic)

instance ToJSON Priority
instance FromJSON Priority


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

itemPriorityValueParser :: Parser Priority
itemPriorityValueParser = option readPriority (long "priority" <> short 'p'  <> metavar "PRIORITY" <> help "priority")
                              where readPriority = eitherReader $ \arg ->
                                             case arg of
                                                  "1" -> Right Low
                                                  "2" -> Right Normal
                                                  "3" -> Right High
                                                  _  -> Left $ "Invalid Priority " ++ arg
                                                   {-_  -> Left $ "Invalid Priority " ++ arg-}

itemDueByValueParser :: Parser LocalTime 
itemDueByValueParser = option readDateTime (long "due-by" <> short 'b'  <> metavar "DUE-BY" <> help "due-by")
                         where 
                               readDateTime = eitherReader $ \arg ->
                                  case parseDateTimeMaybe arg of
                                       (Just dateTime) -> Right dateTime
                                       Nothing -> Left $ "Date/Time string must be in format " ++ dateTimeFormat
                               parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
                               dateTimeFormat = "%Y/%m/%d %H:%M:%S"

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

updateItemDueByParser   :: Parser ItemDueBy
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
run datapath (Update idx itemUpdate) = updateItem datapath idx itemUpdate
run datapath (View idx) = viewItem datapath idx
run datapath (Add item) = addItem datapath item
run datapath List = putStrLn "List"
run datapath (Remove idx) = deleteItem datapath idx

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

showItem :: ItemIndex -> Item -> IO ()
showItem idx (Item title mbDescription mbPriority mbDueBy) = do
   putStrLn $ "[" ++  show idx ++ "]: " ++ title
   putStr " Descriptin: "
   putStrLn $ showField id mbDescription
   putStr " Priority: "
   putStrLn $ showField show mbPriority
   putStr " Due by: "
   putStrLn $ showField (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S")  mbDueBy

showField :: (a -> String) -> Maybe a -> String
showField f (Just a) = f a
showField _ Nothing = "(not set)"

addItem :: FilePath -> Item -> IO ()
addItem datapath item = do
   ToDoList items <- readToDoList datapath
   let newToDoList = ToDoList (item:items)
   writeToDoList datapath newToDoList

deleteItem :: FilePath -> ItemIndex -> IO ()
deleteItem datapath idx = do
   ToDoList items <- readToDoList datapath
   let mbItems = items `removeAt` idx
   case mbItems of
        Nothing -> putStrLn "Invalid index number"
        Just items' -> 
           let toDoList = ToDoList items' in
           writeToDoList datapath toDoList

removeAt :: [a] -> Int -> Maybe [a] 
removeAt items idx = 
      if idx < 0 || idx >= length items
        then Nothing
        else let (before, after) = splitAt idx items
                 _:after' = after
                 xs' = before ++ after'
            in Just xs'
                               
updateItem :: FilePath -> ItemIndex -> ItemUpdate -> IO ()
updateItem datapath idx item = do
   ToDoList items <- readToDoList datapath
   putStrLn "Amir"

viewItem :: FilePath -> ItemIndex -> IO ()
viewItem datapath idx = do
   ToDoList items <- readToDoList datapath
   let x = items !! idx
   case x of
        Nothing -> putStrLn "Invalid item index"
        Just a -> showItem idx a

main :: IO ()
main = do
      Options datapath command <- execParser (info (optionsParser) ( progDesc "To-Do list manager"))
      homeDir <- getHomeDirectory
      let expandedDataPath = replace "~" homeDir datapath
      run expandedDataPath command

