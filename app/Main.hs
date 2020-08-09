module Main where

import Lib

import Data.Maybe

import           System.Environment (getArgs)

import Crystallography.HallSymbols.SpacegroupSymbols

main :: IO ()
main = do
  args <- getArgs
  let hall = fromNumberAndChoice $ head args
  someFunc $ fromJust hall

fromNumberAndChoice :: NumberAndChoice -> Maybe HallName
fromNumberAndChoice numberAndChoice = lookup numberAndChoice hallNames
  where
    hallNames = map (\(a,b,c) -> (a,c)) spacegroupSymbols