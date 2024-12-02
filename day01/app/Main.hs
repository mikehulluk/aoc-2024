{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Prelude

import Debug.Trace
import Data.List (transpose, sort)
import Control.Monad
import qualified Data.Map as M


type LocationId = Int
type LocationIdList = [LocationId]


main :: IO ()
main = do
    locations <- loadLocationFile "test-input-0.txt" 
    -- locations <- loadLocationFile "test-input-1.txt" 

    when (length locations /= 2) $ error "Bad input lengths (I)"

    let (lst0::LocationIdList) = locations !! 0
    let (lst1::LocationIdList) = locations !! 1

    when (length lst0 /= length lst1) $ error "Bad input lengths (II)"

    traceM $ "List0: " ++ show lst0
    traceM $ "List1: " ++ show lst1

    let dist1 = listsDistancePt1 lst0 lst1
    traceM $ "List distance1: " ++ show dist1

    let dist2 = listsDistancePt2 lst0 lst1
    traceM $ "List distance1: " ++ show dist2

    pure ()


loadLocationFile :: FilePath -> IO [LocationIdList]
loadLocationFile pth = do 
    
        inputText <- readFile pth

        let inputDataByCol:: [[LocationId]]
            inputDataByCol = inputText 
                                |> lines
                                |> (fmap words) 
                                |> ((fmap . fmap) (read @Int))
                                |> transpose
        --traceM $ "inputDataByCol:" ++ show inputDataByCol
        pure inputDataByCol
        

listsDistancePt1 :: LocationIdList -> LocationIdList -> Int
listsDistancePt1 lst0 lst1 = 
        zip (sort lst0) (sort lst1)
            |> fmap (\(l0, l1) -> abs  (l0 - l1)) 
            |> sum


listsDistancePt2 :: LocationIdList -> LocationIdList -> Int
listsDistancePt2 lst0 lst1 = 
        lst0
            |> fmap (\v -> (M.findWithDefault 0 v lst1Counts) *v)
            |> sum

    where lst1Counts = countMap lst1









countMap :: (Ord a) => [a] -> M.Map a Int
countMap elems =
    elems 
        |> fmap (\k -> M.singleton k 1)
        |> M.unionsWith (+)


infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x