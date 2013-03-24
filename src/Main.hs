module Main where


import Control.Monad.Trans.Class
import Data.Void
import Data.Conduit
import qualified Data.Conduit.List as CL

import ConduitUtil

-- | reads an int n from the outer stream,
--   yields n
--   drops n values from the inner stream
--   and finally runs bar
foo :: Monad m => Conduit Int (ConduitM Int Void m) (Either Int Int)
foo = do
    mn <- await
    case mn of
        Nothing -> return ()
        Just n -> do
            yield (Left n)
            lift $ CL.drop n
            bar

-- | reads an int n from the inner stream
--   yields n
--   drops n values from the outer stream,
--   and finally runs foo
bar :: Monad m => Conduit Int (ConduitM Int Void m) (Either Int Int)
bar = do
    mn <- lift await
    case mn of
        Nothing -> return ()
        Just n -> do
            yield (Right n)
            CL.drop n
            foo

-- | This incorrect implementation of fuse inner resets `left` on each action in the outer conduit.
--   Therefore, `fuseInner_WRONG (isolate n)` does nothing unless at least `n` values are read
--   from the inner stream with no outer stream actions in between.
fuseInner_WRONG :: Monad m
          => Conduit a m b
          -> ConduitM i o (ConduitM b c m) r
          -> ConduitM i o (ConduitM a c m) r
fuseInner_WRONG left = transPipe (left =$=)

main::IO()
main = CL.sourceList nats $$ (CL.sourceList nats $$ (test =$ CL.mapM_ (lift . print)))
  where
    -- | runs until Left 33, which drops up to element 53 of the inner stream, thus terminating
    test = CL.isolate 40 `fuseInner` foo
    
    -- | runs too long (until foo tries to drop more than 40 elements of the inner stream AT ONCE)
    --test = CL.isolate 40 `fuseInner_WRONG` foo 
    
    -- | would run forever, but it actually overflows the stack and crashes
    --test = foo
    nats = iterate (1+) 0