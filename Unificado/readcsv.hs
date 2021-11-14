{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReadCSV
  where


-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- Libreria arboles --
import Tipos
import Utils

-----------------------------------
------------ Atributos ------------
-----------------------------------

gender = D "gender" ["male","female"]

genderC = C "gender" (0,1) Nothing

gendertoC s = case s of
         "male" -> 0
         "female" -> 1

race = D "race" ["group A", "group B", "group C", "group D", "group E"]

raceC = C "race" (0,4) Nothing

racetoC s = case s of
        "group A" -> 0
        "group B" -> 1
        "group C" -> 2
        "group D" -> 3
        "group E" -> 4
        
parentsed = D "parentsed" ["some high school","bachelor's degree","some college","master's degree","associate's degree","high school"]

parentsedC = C "parentsed" (0,5) Nothing

parentsedtoC s = case s of
        "some high school" -> 0
        "high school" -> 1
        "some college" -> 2
        "associate's degree" -> 3
        "bachelor's degree" -> 4
        "master's degree" -> 5
        

lunch = D "lunch" ["standard","free/reduced"]

lunchC = C "lunch" (0,1) Nothing

lunchtoC s = case s of
         "standard" -> 0
         "free/reduced" -> 1

course = D "course" ["none","completed"]

courseC = C "course" (0,1) Nothing

coursetoC s = case s of
         "none" -> 0
         "completed" -> 1
         
mathscore = C "mathscore" (0,100) Nothing

readingscore = C "readingscore" (0,100) Nothing

writingscore = C "writingscore" (0,100) Nothing

-----------------------------------
-----------------------------------


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

data ItemType
  = Atributo
  | Other Text
  deriving (Eq, Show)

header :: ByteString
header = "gender,race,parentsed,lunch,course,mathscore,readingscore,writingscore"

firstRecord :: ByteString
firstRecord =
  "female,group B,bachelor's degree,standard,none,72,72,74"

openDataItem =  decodeItems "Gender,race/ethnicity,parental level of education,lunch,test preparation course,math score,reading score,writing score\nfemale,group B,bachelor's degree,standard,none,72,72,74\n" 

--prepareItem :: Item -> Ejemplo
prepareItemC45 item =
    let i = item
    in
    (
    [aleft (gender,itemGender i),aleft (race,itemRace i),aleft (parentsed,itemParentsEd i),
    aleft (lunch,itemLunch i),
    aright (readingscore,itemReadingScore i), aright (writingscore,itemWritingScore i),
    aright (mathscore, itemMathScore i)],
    aleft (course,itemCourse i)
    )

prepareItemCART item =
    let i = item
    in
    (
    [
    aright (genderC, gendertoC $ itemGender i),
    aright (raceC, racetoC $ itemRace i),
    aright (parentsedC, parentsedtoC $ itemParentsEd i),
    aright (lunchC, lunchtoC $ itemLunch i),
    aright (courseC, coursetoC $ itemCourse i),
    aright (readingscore, itemReadingScore i),
    aright (writingscore, itemWritingScore i)
    ],
    aright (mathscore, itemMathScore i)
    )
    
firstItem :: Item
firstItem =
  Item
    { itemGender = "female"
    , itemRace = "group B"
    , itemParentsEd = "bachelor's degree"
    , itemLunch = "standard"
    , itemCourse = "none"
    , itemMathScore = 72.0
    , itemReadingScore = 72.0
    , itemWritingScore = 74.0
    }

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
      
-- instance FromField ItemType where
--   parseField "International Country" =
--     pure Country

--   parseField otherType =
--     Other <$> parseField otherType


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
