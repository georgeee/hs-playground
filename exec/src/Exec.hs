module Main where

import           Control.Concurrent.Async (async, waitAny)
import           Data.Foldable            (find)
import           Data.String              (fromString)
import           System.Environment       (getArgs)
import           Turtle.Prelude           (shell)

waitSeq [] = return ()
waitSeq asyncs = waitAny (map snd asyncs) >>= \(as, v) -> do
    let predicate (_, a) = a == as
        i = maybe (error "waitAny returned unknown async") fst $ find predicate asyncs
    putStrLn $ "Async #" ++ show i ++ " returned code " ++ show v
    waitSeq $ filter (not . predicate) asyncs

main = do
  ps <- getArgs >>= mapM (async . flip shell mempty . fromString)
  waitSeq $ zip [1..] ps

