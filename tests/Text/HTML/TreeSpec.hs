{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Text.HTML.TreeSpec
where

import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser
import           Text.HTML.ParserSpec
import           Text.HTML.Tree


arbitraryTokenForest :: Gen (Forest Token)
arbitraryTokenForest = listOf arbitraryTokenTree

arbitraryTokenTree :: Gen (Tree Token)
arbitraryTokenTree = oneof
    [ Node <$> validOpen <*> scale (`div` 5) arbitraryTokenForest
    , Node <$> validFlat <*> pure []
    ]


spec :: Spec
spec = do
  it "parseTokenForests and renderTokenForest are inverses"
    . property . forAllShrink arbitraryTokenForest shrink $
      \forest -> tokensToForest (tokensFromForest forest) `shouldBe` Right forest
