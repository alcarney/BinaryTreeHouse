module Data.BinaryTreeHouse
(
    BinaryTreeHouse (..),

    binTreeMap,
    binTreeLeafMap,
    binTreeBranchMap,

    growTree
) where

import Data.Block

data BinaryTreeHouse a b = Nil
                         | Leaf a
                         | Branch b (BinaryTreeHouse a b) (BinaryTreeHouse a b)

instance (Show a, Show b) => Show (BinaryTree a b) where
    show t = show $ showBinaryTreeHouse t

showBox :: Show a => Int -> a -> Block
showBox off x = makeBlock [edge, showitem, edge]
    where item = show x
          showlength = length item + 2
          offset = replicate off ' '
          edge = "+" ++ replicate (showlength + 2*off) '-' ++ "+"
          showitem = "|" ++ offset ++ " " ++ item ++ " " ++ offset ++ "|"

showBinaryTreeHouse :: (Show a, Show b) => BinaryTreeHouse a b -> Block
showBinaryTreeHouse Nil = makeBlock [""]
showBinaryTreeHouse (Leaf x) = showBox 0 x
showBinaryTreeHouse (Branch h x y) = addToTheRight 0 (addToTheRight 0 left house) right
  where house = showBox 0 h
        left = padTop (height house) (showBinaryTreeHouse x)
        right = padTop (height house) (showBinaryTreeHouse y)

binTreeMap :: (a -> b) -> BinaryTreeHouse a a -> BinaryTreeHouse b b
binTreeMap f (Leaf a) = Leaf (f a)
binTreeMap f (Branch house left right) = Branch (f house) (binTreeMap f left) (binTreeMap f right)
binTreeMap _ Nil = Nil

binTreeBranchMap :: (b -> c) -> BinaryTreeHouse a b -> BinaryTreeHouse a c
binTreeBranchMap _ Nil = Nil
binTreeBranchMap _ (Leaf a) = Leaf a
binTreeBranchMap f (Branch house left right) = Branch (f house) (binTreeBranchMap f left) (binTreeBranchMap f right)

binTreeLeafMap :: (a -> b) -> BinaryTreeHouse a c -> BinaryTreeHouse b c
binTreeLeafMap _ Nil = Nil
binTreeLeafMap f (Leaf a) = Leaf (f a)
binTreeLeafMap f (Branch house left right) = Branch house (binTreeLeafMap f left) (binTreeLeafMap f right)

growTree :: b -> BinaryTreeHouse a b -> BinaryTreeHouse a b -> BinaryTreeHouse a b
growTree _ Nil Nil = Nil
growTree house Nil right = Branch house right Nil
growTree house left Nil = Branch house left Nil
growTree house left right = Branch house left right
