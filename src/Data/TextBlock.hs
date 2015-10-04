module Data.TextBlock
(
    TextBlock,
    makeTextBlock,

    padRight,
    padLeft,
    padTop,
    padBottom,

    addOnTop,
    addBelow,
    addToTheRight,
    addToTheLeft

) where

-- Data Types
data Side = STop | SBottom | SLeft | SRight
data TextBlock = TextBlock {width :: Int, height :: Int, contents :: [String]}

-- Instance Definitions
instance Show TextBlock where
    show (TextBlock _ _ str) = unlines str

-- Functions
makeTextBlock :: [String] -> TextBlock
makeTextBlock str = TextBlock {width = newWidth, height = newHeight, contents = newContents}
  where newWidth = maximum $ map length str
        newHeight = length str
        newContents = map (makeLength newWidth str)
        makeLength x s
            | length s < x = s ++ spacePadding (x - length s)
            | otherwise = s

spacePadding :: Int -> String
spacePadding x = replicate x ' '
-- Adds the second block on top of the first
addOnTop :: Int -> TextBlock -> TextBlock -> TextBlock
addOnTop pad x y = TextBlock {width = newWidth, height = newHeight, contents = newContents}
  where newWidth = max (width x) (width y)
        newHeight = height x + height y + pad
        padding = replicate pad $ spacePadding newWidth
        newContents = contents (enforceWidth y) ++ padding ++ contents (enforceWidth x)
        enforceWidth a
            | width a < newWidth = padRight (newWidth - width a) a
            | otherwise  = a

addBelow :: Int -> TextBlock -> TextBlock -> TextBlock
addBelow pad x y = addOnTop pad y x

addToTheRight :: Int -> TextBlock -> TextBlock
addToTheRight pad x y = TextBlock {width = newWidth, height = newHeight, contents = newContents}
  where newWidth = width x + width y + pad
        newHeight = max (height x) (height y)
        x' = x {width = width x + 1, contents = map (++ spacePadding pad) (contents x)}
        newContents = sideBySideZip (contents x') (contents y)

addToTheLeft :: Int -> TextBlock -> TextBlock -> TextBlock
addToTheLeft pad x y = addToTheRight pad y x
padBlock :: Side -> Int -> TextBlock -> TextBlock
padBlock _ 0 x = x
padBlock STop pad x = addOnTop 0 x (makeTextBlock $ replicate pad $ spacePadding $ width x)
padBlock SBottom pad x = addBelow 0 x (makeTextBlock $ replicate pad $ spacePadding $ width x)
padBlock SLeft pad x = addToTheLeft 0 x (makeTextBlock $ replicate (height x) $ spacePadding pad)
padBlock SRight pad x = addToTheRight 0 x (makeTextBlock $ replicate (height x) $ spacePadding pad)

padTop :: Int -> TextBlock -> TextBlock
padTop = padBlock STop

padBottom :: Int -> TextBlock -> TextBlock
padBottom = padBlock SBottom

padLeft :: Int -> TextBlock -> TextBlock
padLeft = padBlock SLeft

padRight :: Int -> TextBlock -> TextBlock
padRight = padBlock SRight
zipWithPad :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithPad f a _ [] ys = zipWith f (repeat a) ys
zipWithPad f _ b xs [] = zipWith f xs (repeat b)
zipwithPad f a b (x:xs) (y:ys) = f x y : zipWithPad f a b xs ys
sideBySideZip :: [String] -> [String] -> [String]
sideBySideZip x y = zipWithPad (++) (spacePadding x') (spacePadding y') x y
  where x' = max $ map length x
        y' = max $ map length y
