module Prepare
  ( prepare
  )
  where

-- open-data
import ReadCSV

-- base
import qualified Control.Monad as Monad
import qualified System.Exit as Exit

-- UsafeIO
import System.IO.Unsafe

--
import TestIO
import Tipos
import qualified Data.Vector as V

prepare :: Int -> IO [Ejemplo]
prepare n = do
    eitherItems <- decodeItemsFromFile "students_performance.csv"

    case eitherItems of
      Left reason ->
        Exit.die reason

      Right items -> do
        return $ map prepareItemCART (take n (V.toList items))