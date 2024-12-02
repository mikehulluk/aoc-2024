{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}




module Main (main) where

import Lib

import Prelude
import Witch

--import Data.Sequence ((|>), (<|), zipWith, singleton, Seq)
--import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
--import Data.Sequence ((|>), (<|), zipWith, singleton, Seq)

import Flow ((|>))
import Debug.Trace

import Data.List (transpose, sort)
import Control.Monad


type LocationId = Int

-- newtype LocationId = LocationId {unlocationId :: Int}
--     deriving (Eq, Ord)

-- instance Show LocationId where
--     show (LocationId idx) = "LocIdx: " ++ show idx



-- List of Indices
type LocationIdList = [LocationId]






main :: IO ()
main = do
    locations <- loadLocationFile "test-input-0.txt" 

    when (length locations /= 2) $ error "Bad input lengths (I)"

    let (lst0::LocationIdList) = locations !! 0
    let (lst1::LocationIdList) = locations !! 1

    when (length lst0 /= length lst1) $ error "Bad input lengths (II)"

    traceM $ show lst0
    traceM $ show lst1

    let dist = listsDistance lst0 lst1
    putStrLn $ "List distance: " ++ show dist

    return ()









loadLocationFile :: FilePath -> IO [LocationIdList]
loadLocationFile pth = do 
    
        inputText <- readFile pth

        -- This is by row.
        let inputDataByCol:: [[LocationId]]
            inputDataByCol = inputText 
                                |> lines
                                |> (fmap words) 
                                |> ((fmap . fmap) (read @Int))
                                |> transpose
--        traceM $ "inputDataByRow:" ++ show inputDataByRow

        -- -- This is by row. transform to by column:
        -- let inputDataByCol = inputDataByRow
        --                     |> transpose


        traceM $ "inputDataByCol:" ++ show inputDataByCol
        return inputDataByCol
        

listsDistance :: LocationIdList -> LocationIdList -> Int
listsDistance l0 l1 = 

        zip (sort l0) (sort l1)
            |> fmap (\(l0, l1) -> abs ( l0 -  l1))
            |> sum

