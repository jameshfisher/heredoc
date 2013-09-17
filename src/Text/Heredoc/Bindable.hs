{-# LANGUAGE FlexibleInstances #-}

module Text.Heredoc.Bindable (Bindable(bind)) where

-- | Types that can be binded.
class Bindable a where
    bind :: a -> String

instance Bindable Int where
    bind = show

instance Bindable Integer where
    bind = show

instance Bindable Double where
    bind = show

instance Bindable Float where
    bind = show

instance Bindable Bool where
    bind = show

instance Bindable Char where
    bind = (:[])

instance Bindable String where
    bind = id
