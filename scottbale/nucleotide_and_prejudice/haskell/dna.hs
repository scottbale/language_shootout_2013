import qualified Data.Map as Map

countNucleotides word = foldr f counts word
                 where counts = Map.fromList [('A', 0), ('C', 0), ('G', 0), ('T', 0)]
                       f symbol cts = Map.insertWith (+) symbol 1 cts


demo = countNucleotides "ACGCATGAAT"

main :: IO ()
main = print $ "yo " ++ show demo

