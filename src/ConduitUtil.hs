{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}

module ConduitUtil where
import Control.Monad.Trans.Class

import Data.Conduit
import Data.Conduit.Internal hiding (leftover,yieldOr,transPipe)

import Data.Void

import Control.Monad

-- | Represents a stateful monad morphism, including finalization.
--   Code using this object must unsure for every stateful morphism st, exactly one of the following occurs
--     1) `stepStatefulMorph st m` is called exactly once, or
--     2) `finalizeStatefulMorph st` is called exactly once.
data StatefulMorph m n = StatefulMorph
    -- | apply the morphism to a monadic value, returning (in the new monad)
    --   the value and the new stateful monad morphism that should be applied
    --   to transform the next value
    { stepStatefulMorph :: forall a. m a -> n (StatefulMorph m n, a)
    -- | An action to be performed when no more values will be transformed.
    , finalizeStatefulMorph :: n () }

-- | promote a pure monad morphism to a stateful monad morphism
makeStateful :: Monad n => (forall a. m a -> n a) -> StatefulMorph m n
makeStateful f = StatefulMorph step (return ())
  where
    step ma = do
        a <- f ma
        return (makeStateful f,a)

-- | compose two stateful monad morphisms
composeStateful :: (Monad m2, Monad m3) => StatefulMorph m2 m3 -> StatefulMorph m1 m2 -> StatefulMorph m1 m3
composeStateful left right = StatefulMorph step final
  where
    step m1a = do
        (left',(right',a)) <- stepStatefulMorph left $ stepStatefulMorph right m1a
        return (composeStateful left' right',a)
    final = lastStepStatefulMorph left (finalizeStatefulMorph right)

-- | step a stateful morphism, finalize it, and return the result.
lastStepStatefulMorph :: Monad n => StatefulMorph m n -> (forall a. m a -> n a)
lastStepStatefulMorph st mx = do
    (st',x) <- stepStatefulMorph st mx
    finalizeStatefulMorph st'
    return x

-- | Class of monad transformers which can have stateful monad morphisms hoisted in
class StatefulHoist t where
    statefulHoist :: (Monad m, Monad n) =>
                     StatefulMorph m n ->
                     StatefulMorph (t m) (t n)

runStatefulMorph :: Monad n => StatefulMorph m n -> m a -> n a
runStatefulMorph = lastStepStatefulMorph

-- | Instance to allow StatefulMorph in the underlying monad to be hoisted to a transformation of Pipes.
--   Code based on transPipe.
instance StatefulHoist (Pipe l i o u) where
    statefulHoist st0 = StatefulMorph (step st0) (return ())
      where
        step :: Monad n => StatefulMorph m n -> Pipe l i o u m r -> Pipe l i o u n (StatefulMorph (Pipe l i o u m) (Pipe l i o u n), r)
        step st (HaveOutput p c o) = HaveOutput (step st p) (lastStepStatefulMorph st c) o
        step st (NeedInput p c) = NeedInput (step st . p) (step st . c)
        step st (Leftover p i) = Leftover (step st p) i
        step st (Done r) = return (StatefulMorph (step st) (lift $ finalizeStatefulMorph st), r)
        step st (PipeM mp) = PipeM np where
            np = do
                (st',p') <- stepStatefulMorph st mp
                return $ step st' p'

-- | Instance to allow StatefulMorph in the underlying monad to be hoisted to a transformation of Conduits.
instance StatefulHoist (ConduitM i o) where
    statefulHoist st = makeStateful ConduitM `composeStateful` statefulHoist st `composeStateful` makeStateful unConduitM

-- | fuses a left conduit onto the input of the inner conduit
fuseInner :: Monad m
          => Conduit a m b
          -> ConduitM i o (ConduitM b c m) r
          -> ConduitM i o (ConduitM a c m) r
fuseInner = runStatefulMorph . statefulHoist . fuseStateful

fuseInnerInner :: Monad m
               => ConduitM a b m ()
               -> ConduitM i1 o (ConduitM i2 Void (ConduitM b Void m)) ()
               -> ConduitM i1 o (ConduitM i2 Void (ConduitM a Void m)) ()
fuseInnerInner = runStatefulMorph . statefulHoist . statefulHoist . fuseStateful

-- | stateful version of (=$=)
fuseStateful :: Monad m
       => Conduit a m b
       -> StatefulMorph (ConduitM b c m) (ConduitM a c m)
fuseStateful (ConduitM left) = makeStateful ConduitM `composeStateful` fusePipeStateful left `composeStateful` makeStateful unConduitM


-- | stateful version of pipeL
fusePipeStateful :: forall m l a b c r0 r1.
         Monad m
      => Pipe l a b r0 m r1
      -> StatefulMorph (Pipe b b c r1 m) (Pipe l a c r0 m)
fusePipeStateful left0 = StatefulMorph (goRight (return ()) left0) (return ())
  where
    goRight :: m () -> Pipe l a b r0 m r1 -> Pipe b b c r1 m r2 -> Pipe l a c r0 m (StatefulMorph (Pipe b b c r1 m) (Pipe l a c r0 m), r2)
    goRight final left right =
        case right of
            HaveOutput p c o -> HaveOutput (recurse p) (c >> final) o
            NeedInput rp rc -> goLeft rp rc final left
            Done r2 -> Done (StatefulMorph recurse (lift final), r2)
            PipeM mp -> PipeM (liftM recurse mp)
            Leftover right' i -> goRight final (HaveOutput left final i) right'
      where
        recurse = goRight final left
    goLeft rp rc final left = 
        case left of
            HaveOutput left' final' o -> goRight final' left' (rp o)
            NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
            Done r1                   -> goRight (return ()) (Done r1) (rc r1)
            PipeM mp                  -> PipeM (liftM recurse mp)
            Leftover left' i          -> Leftover (recurse left') i
      where
        recurse = goLeft rp rc final
