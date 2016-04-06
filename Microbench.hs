{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Exts
import Data.Char
import qualified Data.Text as T
import Data.Attoparsec.Text
import Criterion.Main
import Prelude hiding (take, takeWhile)

isAsciiAlpha1 :: Char -> Bool
isAsciiAlpha1 c = isAsciiLower c || isAsciiUpper c

isAsciiAlpha2 :: Char -> Bool
isAsciiAlpha2 (C# c) =
    tagToEnum# ( ((c `geChar#` 'A'#) `andI#` (c `leChar#` 'Z'#))
          `orI#` ((c `geChar#` 'a'#) `andI#` (c `leChar#` 'z'#)) )

main :: IO ()
main = defaultMain
    [ bench "Text isAsciiAlpha1"
      $ whnf (T.length . T.takeWhile isAsciiAlpha1) testString
    , bench "Text isAsciiAlpha2"
      $ whnf (T.length . T.takeWhile isAsciiAlpha2) testString

    , bench "Attoparsec isAsciiAlpha1"
      $ whnf (parseOnly (T.length <$> takeWhile isAsciiAlpha1)) testString
    , bench "Attoparsec isAsciiAlpha2"
      $ whnf (parseOnly (T.length <$> takeWhile isAsciiAlpha2)) testString
    ]

testString :: T.Text
testString = T.replicate 10 "helloworldthisisarelativelylongstring"
