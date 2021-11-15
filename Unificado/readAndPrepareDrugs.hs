{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReadandPrepareDrugs
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
    { itemAge :: Double
    , itemSex :: String
    , itemBP :: String
    , itemCholesterol :: String
    , itemNa_to_K :: Double
    , itemDrug :: String
    }
  deriving (Eq, Show)

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> m .: "Age"
      <*> m .: "Sex"
      <*> m .: "BP"
      <*> m .: "Cholesterol"
      <*> m .: "Na_to_K"
      <*> m .: "Drug"

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

age          = C "age" (10,90) Nothing

sex          = D "sex" ["F", "M"]

bp           = D "BP" ["HIGH","NORMAL","LOW"]

cholesterol = D "cholesterol" ["HIGH","NORMAL"]

na_to_K     = C "na_to_K" (5,40) Nothing

drug         = D "drug" ["DrugX","DrugY","DrugA","DrugB","DrugC"]

-----------------------------------
-----------------------------------

prepareItem :: Item -> Ejemplo
prepareItem item =
    (
    [aright (age,itemAge item),
    aleft (sex,itemSex item),
    aleft (bp, itemBP item),
    aleft (cholesterol,itemCholesterol item),
    aright (na_to_K,itemNa_to_K item)],
    aleft (drug,itemDrug item)
    )


prepare :: Int -> String -> (Item -> Ejemplo) -> IO [Ejemplo]
prepare n file prepareItems = do
    eitherItems <- decodeItemsFromFile file

    case eitherItems of
      Left reason ->
        Exit.die reason

      Right items -> do
        return $ map prepareItems (take n (toList items))