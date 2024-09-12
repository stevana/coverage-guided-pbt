module Example.Counter where

import Data.IORef

import Generate
import Test

------------------------------------------------------------------------

newtype BrokenCounter = BrokenCounter (IORef Int)

incrBroken :: BrokenCounter -> IO ()
incrBroken (BrokenCounter ref) = do
  n <- readIORef ref
  if n == 42
  then return ()
  else writeIORef ref (n + 1)

readBrokenCounter :: BrokenCounter -> IO Int
readBrokenCounter (BrokenCounter ref) = readIORef ref

newBrokenCounter :: IO BrokenCounter
newBrokenCounter = BrokenCounter <$> newIORef 0

resetCounter :: BrokenCounter -> IO ()
resetCounter (BrokenCounter ref) = writeIORef ref 0

data Command = Incr
  deriving Show

mIncr :: Manual Command
mIncr = Manual
  { _gen    = return Incr
  , _shrink = const []
  }

newtype Model = Model Int

initModel :: Model
initModel = Model 0

model :: [Command] -> Model -> Int
model cmds _m = length cmds

exec :: [Command] -> BrokenCounter -> IO Int
exec cmds c = do
  mapM_ exec1 cmds
  readBrokenCounter c
  where
    exec1 Incr = incrBroken c

testCounter :: Seed -> IO ()
testCounter seed = do
  c <- newBrokenCounter
  checkM seed 500 (integrated mIncr) (resetCounter c) $ \cmds cov -> do
    resetCounter c
    r <- exec cmds c
    let m = model cmds initModel
    addCoverage cov (length cmds)
    return (r == m)

