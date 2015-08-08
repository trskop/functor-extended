{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Extended module Data.Functor, from base, with additional
--               functions.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, NoImplicitPrelude
--
-- Extended module "Data.Functor", from base, with additional functions.
module Data.Functor.Extended
    (
    -- * Re-export of Data.Functor from base
      module Data.Functor

    -- * Extra functions
#if !MIN_VERSION_base(4,7,0)
    , ($>)
#endif
    , (<$$>)
    , (<&>)
    , (<&$>)
    )
  where

import Data.Function (flip)
import Data.Functor


-- Function ($>) is available in base since version 4.7.0.0.
#if !MIN_VERSION_base(4,7,0)
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
infixl 4 $>
{-# INLINE ($>) #-}
#endif

-- | Flipped version of ('<$>').
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap
infixl 4 <$$>
{-# INLINE (<$$>) #-}

-- | Flipped version of ('<$>') with different fixity.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
{-# INLINE (<&>) #-}

-- | Version of ('<$>') with fixity as ('<&>') has.
(<&$>) :: Functor f => (a -> b) -> f a -> f b
(<&$>) = fmap
infixl 1 <&$>
{-# INLINE (<&$>) #-}
