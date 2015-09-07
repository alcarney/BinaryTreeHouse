module Data.Block
(
    Block (..),
    makeBlock,

    padRight,
    padLeft,
    padTop,
    padBottom,

    addOnTop,
    addBelow,
    addToTheRight,
    addToTheLeft
) where

data Side = STop | SBottom | SRight | SLeft
data Block = Block {width :: Int, height :: Int, contents :: [String]}


instance Show Block where
    show (Block _ _ str) = unlines str

makeBlock :: [String] -> Block
makeBlock s = Block {width = newWidth, height = newHeight, contents = newContents}
  where newWidth = maximum $ map length s
        newHeight = length s
        newContents = map (makeLength newWidth) s
        makeLength x str
          | length str < x = str ++ spacePadding (x - length str)
          | otherwise = str

padBlock :: Side -> Int -> Block -> Block
padBlock _ 0 x = x
padBlock STop pad x = addOnTop 0 x (makeBlock $ replicate pad $ spacePadding $ width x)
padBlock SBottom pad x = addBelow 0 x (makeBlock $ replicate pad $ spacePadding $ width x)
padBlock SLeft pad x = addToTheLeft 0 x (makeBlock $ replicate (height x) $ spacePadding pad)
padBlock SRight pad x = addToTheRight 0 x (makeBlock $ replicate (height x) $ spacePadding pad)

padTop :: Int -> Block -> Block
padTop = padBlock STop

padBottom :: Int -> Block -> Block
padBottom = padBlock SBottom

padLeft :: Int -> Block -> Block
padLeft = padBlock SLeft

padRight :: Int -> Block -> Block
padRight = padBlock SRight

spacePadding :: Int -> String
spacePadding x = replicate x ' '

addOnTop :: Int -> Block -> Block -> Block
addOnTop pad x y = Block {width = newWidth, height = newHeight, contents = newContents}
  where newWidth = max (width x) (width y)
        newHeight = height x + height y + pad
        padding = replicate pad $ spacePadding newWidth
        newContents = contents (enforceWidth y) ++ padding ++ contents (enforceWidth x)
        enforceWidth a
          | width a < newWidth = padRight (newWidth - width a) a
          | otherwise = a

addBelow :: Int -> Block -> Block -> Block
addBelow pad x y = addOnTop pad y x

addToTheRight :: Int -> Block -> Block -> Block
addToTheRight pad x y = Block {width = newWidth, height = newHeight, contents = newContents}
  where newWidth = width x + width y + pad
        newHeight = max (height x) (height y)
        x' = x {width = width x + 1, contents = map (++ spacePadding pad) (contents x)}
        newContents = sideBySideZip (contents x') (contents y)

addToTheLeft :: Int -> Block -> Block -> Block
addToTheLeft pad x y = addToTheRight pad y x

sideBySideZip :: [String] -> [String] -> [String]
sideBySideZip x y = zipWithPadding (++) (spacePadding x') (spacePadding y') x y
  where x' = length $ head x
        y' = length $ head y

zipWithPadding :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithPadding f a b (x:xs) (y:ys) = f x y : zipWithPadding f a b xs ys
zipWithPadding f a _ [] ys = zipWith f (repeat a) ys
zipWithPadding f _ b xs [] = zipWith f xs (repeat b)
