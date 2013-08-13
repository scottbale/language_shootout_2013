import qualified Data.Map as Map
import Data.List

countNucleotides :: String -> Map.Map Char Int
countNucleotides word = foldr f counts word
                 where counts = Map.fromList [('A', 0), ('C', 0), ('G', 0), ('T', 0)]
                       f symbol cts = Map.insertWith (+) symbol 1 cts

formattedCount :: Map.Map Char Int -> String
formattedCount counts = concat . (intersperse " ") . map show $ (map gitCount ['A', 'C', 'G', 'T'])
               where gitCount ch = case Map.lookup ch counts of
                                   Nothing -> 0
                                   Just n -> n

count :: String->String
count = formattedCount . countNucleotides

demo :: String
demo = count "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"

main :: IO ()
main = print $ "yo " ++ demo

