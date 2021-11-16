{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReadAndPrepareMarks
  where

-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified System.Exit as Exit
import System.IO.Unsafe

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
  ( FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , (.:)
  )
import qualified Data.Csv as Cassava

-- vector
import Data.Vector (Vector, toList)

-- Libreria arboles 
import Tipos
import Utils


data Item =
  Item
    { itemGender :: String
    , itemRace :: String
    , itemParentsEd :: String
    , itemLunch :: String
    , itemCourse :: String
    , itemMathScore :: Double
    , itemReadingScore :: Double
    , itemWritingScore :: Double
    }
  deriving (Eq, Show)

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> m .: "gender"
      <*> m .: "race/ethnicity"
      <*> m .: "parental level of education"
      <*> m .: "lunch"
      <*> m .: "test preparation course"
      <*> m .: "math score"
      <*> m .: "reading score"
      <*> m .: "writing score"

decodeItems
  :: ByteString
  -> Either String (Vector Item)
decodeItems =
  fmap snd . Cassava.decodeByName

decodeItemsFromFile
  :: FilePath
  -> IO (Either String (Vector Item))
decodeItemsFromFile filePath =
  catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeItems

catchShowIO
  :: IO a
  -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show


-----------------------------------
------------ Atributos ------------
-----------------------------------

gender = C "gender" (0,1) Nothing  -- ["male","female"]

gendertoC s = case s of
         "male" -> 0
         "female" -> 1

race = C "race" (0,4) Nothing -- ["group A", "group B", "group C", "group D", "group E"]

racetoC s = case s of
        "group A" -> 0
        "group B" -> 1
        "group C" -> 2
        "group D" -> 3
        "group E" -> 4
        
parentsed = C "parentsed" (0,5) Nothing -- ["some high school","bachelor's degree","some college","master's degree","associate's degree","high school"]

parentsedtoC s = case s of
        "some high school" -> 0
        "high school" -> 1
        "some college" -> 2
        "associate's degree" -> 3
        "bachelor's degree" -> 4
        "master's degree" -> 5

lunch = C "lunch" (0,1) Nothing  -- ["standard","free/reduced"]

lunchtoC s = case s of
         "standard" -> 0
         "free/reduced" -> 1

course = C "course" (0,1) Nothing -- ["none","completed"]

coursetoC s = case s of
         "none" -> 0
         "completed" -> 1
         
mathscore = C "mathscore" (0,100) Nothing

readingscore = C "readingscore" (0,100) Nothing

writingscore = C "writingscore" (0,100) Nothing

-----------------------------------
-----------------------------------

prepareItem item =
    let i = item
    in
    (
    [
    aright (gender, gendertoC $ itemGender i),
    aright (race, racetoC $ itemRace i),
    aright (parentsed, parentsedtoC $ itemParentsEd i),
    aright (lunch, lunchtoC $ itemLunch i),
    aright (course, coursetoC $ itemCourse i),
    aright (readingscore, itemReadingScore i),
    aright (writingscore, itemWritingScore i)
    ],
    aright (mathscore, itemMathScore i)
    )
    
prepare :: Int -> String -> (Item -> Ejemplo) -> IO [Ejemplo]
prepare n file prepareItems = do
    eitherItems <- decodeItemsFromFile file

    case eitherItems of
      Left reason ->
        Exit.die reason

      Right items -> do
        return $ map prepareItems (take n (toList items))