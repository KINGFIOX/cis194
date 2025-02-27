{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module W07Sized where

import Data.Monoid

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

-- getSize :: Size -> Int
-- getSize (Size i) = i
getSize :: (Sized a) => a -> Int
getSize x = i where (Size i) = size x

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance (Sized b) => Sized (a, b) where
  size = size . snd

instance Semigroup Size where
  (<>) = (+)

instance Monoid Size where
  mempty = Size 0