module Data.BinaryTreeHouse
(
    BinaryTreeHouse(..),

    growTree
) where

import Data.Block
import Data.Bifunctor

data BinaryTreeHouse a b = Nil
                         | Leaf a
                         | Branch b (BinaryTreeHouse a b) (BinaryTreeHouse a b)
instance (Show a, Show b) => Show (BinaryTreeHouse a b) where
    show t = show $ showBinaryTreeHouse t

showBinaryTreeHouse :: (Show a, Show b) => BinaryTreeHouse a b -> TextBlock
showBinaryTreeHouse Nil = makeTextBlock [""]
showBinaryTreeHouse (Leaf x) = showBox 0 x
showBinaryTreeHouse (Branch h x y) = addToTheRight 0 (addToTheRight 0 left house) right
  where house = showBox 0 h
        left = padTop (height house) (showBinaryTreeHouse x)
        right = padTop (height house) (showBinaryTreeHouse y)
instance Bifunctor BinaryTreeHouse where

  first _ Nil = Nil
  first f (Leaf a) = Leaf (f a)
  first f (Branch house left right) = Branch house (first f left) (first f right)

  second _ Nil = Nil
  second _ (Leaf a) = Leaf a
  second f (Branch house left right) = Branch (f house) (second f left) (second right)
growTree :: b -> BinaryTreeHouse a b -> BinaryTreeHouse a b -> BinaryTreeHouse a b
growTree _ Nil Nil = Nil
growTree house left right = Branch house left right
