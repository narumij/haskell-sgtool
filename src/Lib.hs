module Lib
    ( someFunc
    ) where

import Text.PrettyPrint.Boxes

import Data.Matrix
import Data.Matrix.AsXYZ
import Data.Matrix.SymmetryOperationsSymbols
import Crystallography.HallSymbols
import Crystallography.HallSymbols.SpacegroupSymbols

someFunc :: String -> IO ()
someFunc s = do
    hoge s
    someFunc' s

hoge :: String -> IO ()
hoge s = do
    let a = lookup s rows
    let b = (fmap hoge a)
    case b of
        Just c -> putStrLn $ render c
        Nothing -> return ()
    where
        rows = map (\a@(b,c,d)->(d,a)) spacegroupSymbols
        hoge (numberAndChoice,hmFull,hallName) =
            hcat left [
                vcat left [
                    (text "Number and choice"), (text "Hermann–Mauguin"), (text "Hall Name")
                    ],
                vcat left [
                    (text " : "),
                    (text " : "),
                    (text " : ")
                ],
                vcat left [
                    (text numberAndChoice), (text hmFull), (text hallName)
                ]
            ]

someFunc' :: String -> IO ()
someFunc' s = putStr $ render doc
    where
        h1 = uvw s
        h2 = symbols s
        h0 = take (length h1) $ map (\n -> show n ++ ") ") [1..]
        hh = take (length h1) $ repeat "  :"
        cols = map (\l -> vcat left (map (\ll -> text ll) l)) [h0,h1,hh,h2]
        doc = hcat left $ cols

uvw :: String -> [String]
uvw s = prettyXYZ <$> fromHallSymbols' s

symbols :: String -> [String]
symbols s = fromHallSymbols' s >>= fromMatrix'

