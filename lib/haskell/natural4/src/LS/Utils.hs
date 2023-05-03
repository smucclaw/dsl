{-# LANGUAGE GHC2021 #-}

module LS.Utils
  ( (|$>)
  )
where

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = flip fmap