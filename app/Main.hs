module Main where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.HTML.Parser
import System.Environment

main :: IO ()
main = do 
    files <- getArgs
    forM_ files $ \fname -> do
        t <- T.readFile fname
        print $ length $ parseTokens t

