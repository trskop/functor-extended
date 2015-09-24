{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Extended module Data.Functor with additional
--               functions.
-- Copyright:    (c) 2011, 2013-2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, NoImplicitPrelude
--
-- Extended module "Data.Functor" with additional functions.
module Data.Functor.Extended
    (
    -- * Re-export of Data.Functor from base
      module Data.Functor
#if !MIN_VERSION_base(4,7,0)
    -- ** Compatibility with base>=4.7
    , ($>)
#endif

    -- * Extra functions

    -- ** Variations on fmap
    , (<$$>)
    , (<&>)
    , (<&$>)

    -- ** Apply value to a function before fmap
    , (<^$^>)
    , (<^$$^>)

    -- ** Apply value to a function in a functor
    , (<#>)
    , (<##>)
    , (<&#>)
    )
  where

import Data.Function (($), flip)
import Data.Functor


-- Function ($>) is available in base since version 4.7.0.0.
#if !MIN_VERSION_base(4,7,0)
-- | Flipped version of '<$'.
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
infixl 4 $>
{-# INLINE ($>) #-}
#endif

-- | Flipped version of '<$>'.
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap
infixl 4 <$$>
{-# INLINE (<$$>) #-}

-- | Flipped version of '<$>' with different fixity.
--
-- This operator can be also found in
-- <https://hackage.haskell.org/package/lens lens> library.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
{-# INLINE (<&>) #-}

-- | Version of '<$>' with fixity as '<&>' has.
(<&$>) :: Functor f => (a -> b) -> f a -> f b
(<&$>) = fmap
infixl 1 <&$>
{-# INLINE (<&$>) #-}

-- | Instead of @\\x -> f x '<$>' g x@ this function allows to write
-- @f '<^$^>' g@.
(<^$^>) :: Functor f => (a -> b -> c) -> (a -> f b) -> a -> f c
(f <^$^> g) x = f x `fmap` g x
infix 4 <^$^>
{-# INLINE (<^$^>) #-}

-- | Flipped variant of '<^$^>'.
(<^$$^>) :: Functor f => (a -> f b) -> (a -> b -> c) -> a -> f c
(<^$$^>) = flip (<^$^>)
infix 4 <^$$^>
{-# INLINE (<^$$^>) #-}

-- | Apply value to a function inside a functor. Similar to using:
--
-- @
-- \\f x -> f 'Control.Applicative.<*>' 'Control.Applicative.pure' x
-- @
--
-- But it does require only 'Functor' and not 'Applicative' constraint.
--
-- === Examples
--
-- >>> Just (+1) <#> 2
-- Just 3
-- >>> [(+1), (*2)] <#> 3
-- [4,6]
--
-- For @instance 'Functor' ((->) r)@ this function behaves as infix version of
-- flip 'flip':
--
-- >>> (-) <#> 1 $ 2
-- 1
--
-- === Implementation
--
-- @
-- f '<#>' x = ('$' x) '<$>' f
-- @
(<#>) :: Functor f => f (a -> b) -> a -> f b
f <#> x = ($ x) `fmap` f
infixl 4 <#>
{-# INLINE (<#>) #-}

-- | Apply value to a function inside a functor. Similar to using:
--
-- @
-- \\x f -> f 'Control.Applicative.<*>' 'Control.Applicative.pure' x
-- @
--
-- But it does require only 'Functor' and not 'Control.Applicative.Applicative'
-- constraint. Flipped version of '<#>'.
--
-- === Implementation
--
-- @
-- x '<##>' f = ('$' x) '<$>' f
-- @
(<##>) :: Functor f => a -> f (a -> b) -> f b
x <##> f = ($ x) `fmap` f
infixl 4 <##>
{-# INLINE (<##>) #-}

-- | Variant of '<#>' with fixity as function '<&>' has.
--
-- This operator can be also found in
-- <https://hackage.haskell.org/package/lens lens> library under the name @??@.
(<&#>) :: Functor f => f (a -> b) -> a -> f b
(<&#>) = (<#>)
infixl 1 <&#>
{-# INLINE (<&#>) #-}
