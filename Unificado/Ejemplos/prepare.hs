module Prepare
  ( prepare
  )
  where

-- open-data
import ReadDrugCSV 

-- base
import qualified Control.Monad as Monad
import qualified System.Exit as Exit

-- UsafeIO
import System.IO.Unsafe

--
import TestIO
import Tipos
import qualified Data.Vector as V

prepare :: Int -> String -> (Item -> Ejemplo) -> IO [Ejemplo]
prepare n file prepareItems = do
    eitherItems <- decodeItemsFromFile file

    case eitherItems of
      Left reason ->
        Exit.die reason

      Right items -> do
        return $ map prepareItems (take n (V.toList items))