{-# LANGUAGE ScopedTypeVariables #-}

module Example.GuessGame where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Random

import Coverage
import Generator
import Test

------------------------------------------------------------------------

data GuessGame = GuessGame (IORef GuessState)

data GuessState = GuessState
  { _correctAnswers :: [Int]
  , _level          :: Int
  , _attempts       :: Int
  , _attemptsLeft   :: Int
  }

data GuessResponse
  = WrongGuess Attempts
  | CorrectFirstAnswer
  | CorrectSecondAnswer
  | YouHaveWon
  | YouHaveLost
  deriving (Eq, Ord, Show)

type UpperBound = Int
type Attempts = Int

newGuessGame :: Seed -> UpperBound -> Attempts -> IO GuessGame
newGuessGame seed upperBound attempts = do
  let prng0 = mkStdGen seed
      (a1, prng1)  = randomR (0, upperBound) prng0
      (a2, prng2)  = randomR (0, upperBound) prng1
      (a3, _prng3) = randomR (0, upperBound) prng2
  ref <- newIORef (GuessState [a1, a2, a3] 0 attempts attempts)
  return (GuessGame ref)

resetGame :: GuessGame -> IO ()
resetGame (GuessGame ref) = do
  oldState <- readIORef ref
  writeIORef ref (oldState { _level = 0, _attemptsLeft = _attempts oldState })

gameLogic :: Int -> GuessState -> (GuessState, GuessResponse)
gameLogic newGuess s@(GuessState answers level _attempts attemptsLeft)
  = if answers !! level == newGuess
    then case level of
           0 -> (s { _level = 1 }, CorrectFirstAnswer)
           1 -> (s { _level = 2 }, CorrectSecondAnswer)
           2 -> (s , YouHaveWon)
           _ -> error "impossible"
    else
      let
        attemptsLeft' = attemptsLeft - 1
      in
        if attemptsLeft' == 0
        then (s { _attemptsLeft = attemptsLeft' }, YouHaveLost)
        else (s { _attemptsLeft = attemptsLeft' }, WrongGuess attemptsLeft')


guess :: GuessGame -> Int -> IO GuessResponse
guess (GuessGame ref) newGuess = atomicModifyIORef' ref (gameLogic newGuess)

correctAnswers :: GuessGame -> IO [Int]
correctAnswers (GuessGame ref) = _correctAnswers <$> readIORef ref

play :: IO ()
play = do
  seed <- randomIO
  game <- newGuessGame seed 3 3
  loop game
  where
    loop game = do
      putStr "Enter guess: "
      eNewGuess <- try readLn
      case eNewGuess of
        Left (_e :: IOException) -> loop game
        Right newGuess -> do
          resp <- guess game newGuess
          putStrLn (show resp)
          case resp of
            YouHaveLost -> do
              answers <- correctAnswers game
              putStrLn ("The correct answers were: " ++
                        intercalate ", " (map show answers))
            _otherwise  -> loop game

------------------------------------------------------------------------

data GuessCommand = Guess Int
  deriving (Eq, Ord, Show)

genCommand :: Int -> Gen GuessCommand
genCommand hi = Guess <$> genIntegral 0 hi

execGuesses :: GuessGame -> [GuessCommand] -> IO [GuessResponse]
execGuesses game = go []
  where
    go acc [] = return (reverse acc)
    go acc (Guess i : cmds) = do
      resp <- guess game i
      go (resp : acc) cmds

testG_ :: IO ()
testG_ = do
  seed <- randomIO
  testG seed

testG :: Seed -> IO ()
testG seed = do
  let upperBound = 100
      attempts   = 10
  game <- newGuessGame seed upperBound attempts
  answers <- correctAnswers game
  putStrLn $ "We are looking for: " ++ show answers
  checkM seed 500 (genCommand upperBound) $ \_shrinking coverage cmds -> do
    resetGame game
    resps <- execGuesses game cmds
    zipWithM_ (monitoring coverage) cmds resps
    return (safeLast resps /= Just YouHaveWon)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

monitoring :: Coverage GuessResponse -> GuessCommand -> GuessResponse -> IO ()
monitoring cov _cmd (WrongGuess _) = addCoverage cov (WrongGuess (-1))
monitoring cov _cmd resp           = addCoverage cov resp

------------------------------------------------------------------------
