{-# LANGUAGE CPP, DataKinds, DeriveDataTypeable, DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction               #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TupleSections         #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Effect.Random
       (module System.Random,
        Rand, EffectRandom,
        -- * Execution
        runRand, evalRand, evalRandIO,
        -- * Generator functions
        getRandom, getRandomR, getRandoms, getRandomRs,
        fromList, uniform,
        -- * Misc
        withSplit
        ) where
import Control.Arrow  (first)
import Control.Effect (Effect, EffectLift, Is, MemberEffect, Row (..))
import Control.Effect (eliminate, intercept, lift, send)
import Control.Monad  (liftM)
import Data.Data      (Typeable)
import System.Random  (Random (..), RandomGen (..), StdGen, newStdGen)
#ifdef MONADRANDOM
import qualified Control.Monad.Random.Class as C
#endif

-- | Random number generator
--
-- Since 0.1.0.0
data Rand g a = RandomGen g => Rand (g -> (a, g))
              deriving (Typeable)

instance Functor (Rand g) where
  fmap g (Rand f) = Rand (first g . f)

type instance Is Rand f = IsRand f


type family IsRand f where
  IsRand (Rand g) = True
  IsRand f        = False

class (RandomGen g, MemberEffect Rand (Rand g) l) => EffectRandom g l
instance (RandomGen g, MemberEffect Rand (Rand g) l) => EffectRandom g l

#ifdef MONADRANDOM
instance EffectRandom g l => C.MonadRandom (Effect l) where
  getRandom   = getRandom
  getRandoms  = getRandoms
  getRandomR  = getRandomR
  getRandomRs = getRandomRs
#endif

-- | Return a randomly-selected value of type a. See 'random' for details.
--
-- Since 0.1.0.0
getRandom :: forall a g l . (Random a, EffectRandom g l) => Effect l a
getRandom = send $ Rand (random :: g -> (a, g))

-- | Return an infinite stream of random values of type a. See 'randoms' for details.
--
-- Since 0.1.0.0
getRandoms :: forall a g l. (Random a, EffectRandom g l) => Effect l [a]
getRandoms = send $ Rand $ \(g :: g) ->
  first randoms $ split g

-- | Return a randomly-selected value of type a in the range @(lo,hi)@. See 'randomR' for details.
--
-- Since 0.1.0.0
getRandomR :: forall a g l. (Random a, EffectRandom g l) => (a, a) -> Effect l a
getRandomR bd = send $ Rand $ \ (g :: g) -> randomR bd g

-- | Return an infinite stream of randomly-selected value
-- of type @a@ in the range @(lo,hi)@. See 'randomRs' for details.
--
-- Since 0.1.0.0
getRandomRs :: forall a g l. (Random a, EffectRandom g l) => (a, a) -> Effect l [a]
getRandomRs bd = send $  Rand $ \(g :: g) -> let (g', g'') = split g in (randomRs bd g', g'')

-- | Sample a random value from a weighted list. The total weight of all elements must not be 0.
--
-- Since 0.1.0.0
fromList :: EffectRandom g l => [(a, Rational)] -> Effect l a
fromList [] = error "Effect.Random.fromList called with empty list"
fromList [(x, _)] = return x
fromList xs = do
  let s  = fromRational $ sum $ map snd xs :: Double
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs
  p <- liftM toRational (getRandomR (0.0,s))
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

-- | Sample a value from a uniform distribution of a list of elements.
--
-- Since 0.1.0.0
uniform :: (EffectRandom g l) => [a] -> Effect l a
uniform xs = fromList $ map (flip (,) 1) xs

-- | Run a computation with random numbers
--
-- Since 0.1.0.0
runRand :: forall a l g. RandomGen g
        => g                         -- ^ initial internal random generator
        -> Effect (Rand g :+ l) a    -- ^ Effectect using random numbers
        -> Effect l (a, g)
runRand g0 act = eliminate ret handle act g0
  where
    ret a g = return (a, g)
    handle :: Rand g (g -> Effect l (a, g)) -> g -> Effect l (a, g)
    handle (Rand tog) g = do
      let (cont, g') = tog g
      cont g'

-- | Run a computation with random numbers, discarding the final generator.
--
-- Since 0.1.0.0
evalRand :: RandomGen g => g -> Effect (Rand g :+ l) a -> Effect l a
evalRand g = liftM fst . runRand g

-- | Run a computation with random numbers, discarding the final generator.
--
-- Since 0.1.0.0
evalRandIO :: EffectLift IO l => Effect (Rand StdGen :+ l) a -> Effect l a
evalRandIO eff = do
  g <- lift newStdGen
  evalRand g eff

-- | Split the current generator and execute the given computation with it.
--
-- Since 0.1.0.0
withSplit :: forall g l a. EffectRandom g l => Effect l a -> Effect l a
withSplit eff = do
  g <- send $ Rand split
  let run (Rand fromg) = fst $ fromg (g :: g)
  intercept return run eff
