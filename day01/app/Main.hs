{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Prelude


import Debug.Trace
import Data.List (transpose, sort)
import Control.Monad
import Flow ((|>))

type LocationId = Int
type LocationIdList = [LocationId]



main :: IO ()
main = do
    --locations <- loadLocationFile "test-input-0.txt" 
    locations <- loadLocationFile "test-input-1.txt" 

    when (length locations /= 2) $ error "Bad input lengths (I)"

    let (lst0::LocationIdList) = locations !! 0
    let (lst1::LocationIdList) = locations !! 1

    when (length lst0 /= length lst1) $ error "Bad input lengths (II)"

    traceM $ "List0: " ++ show lst0
    traceM $ "List1: " ++ show lst1

    let dist = listsDistance lst0 lst1
    putStrLn $ "List distance: " ++ show dist

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
        

listsDistance :: LocationIdList -> LocationIdList -> Int
listsDistance lst0 lst1 = 
        zip (sort lst0) (sort lst1)
            |> fmap (\(l0, l1) -> abs  (l0 - l1)) 
            |> sum

