import Data.List.Split (splitOn)
import Data.Function ((&))

data Operation = Operation Int [Int]

main :: IO ()
main = do
  operations <- parseFile "2024/input/day-07.txt"
  operations
    & filter isValid
    & map (\(Operation target _) -> target)
    & sum
    & print

isValid :: Operation -> Bool
isValid (Operation target values) = any (evalsTo target) options
  where evalsTo x ops = x == evaluate ops
        options = generateOperations $ reverse values

data Operator = Id | Add | Multiply

generateOperations :: [Int] -> [[(Int, Operator)]]
generateOperations [] = []
generateOperations [x] = [[(x, Id)]]
generateOperations (x:xs) =
  [((x, op) : rest) | rest <- generateOperations xs, op <- [Add, Multiply]]

evaluate :: [(Int, Operator)] -> Int
evaluate = foldr calculate 0
  where
    calculate (x, Id)       _   = x
    calculate (x, Add)      acc = acc + x
    calculate (x, Multiply) acc = acc * x

parseFile :: FilePath -> IO [Operation]
parseFile filePath = do
  content <- readFile filePath
  let linesOfFile = lines content
  return (map parseLine linesOfFile)

parseLine :: String -> Operation
parseLine line =
  let (targetPart : valuePart : _) = splitOn ":" line
      target = read targetPart
      values = map read (words valuePart)
   in Operation target values
