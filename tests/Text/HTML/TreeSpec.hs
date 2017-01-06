{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
    [ Node <$> validClosingOpen    <*> scale (`div` 5) arbitraryTokenForest
    , Node <$> validNonClosingOpen <*> pure []
    , Node <$> validFlat           <*> pure []
    ]


validNonClosingOpen :: Gen Token
validNonClosingOpen = TagOpen <$> elements nonClosing <*> arbitrary

validClosingOpen :: Gen Token
validClosingOpen = do
    n <- validXmlTagName
    let n' = if n `elem` nonClosing then "_" else n
    TagOpen n' <$> arbitrary


spec :: Spec
spec = do
  it "parseTokenForests and renderTokenForest are inverses"
    . property . forAllShrink arbitraryTokenForest shrink $
      \forest -> tokensToForest (tokensFromForest forest) `shouldBe` Right forest
