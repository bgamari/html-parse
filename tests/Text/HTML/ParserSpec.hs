{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Text.HTML.ParserSpec
where

import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser


instance Arbitrary Token where
  arbitrary = oneof [validOpen, validClose, validFlat]

  shrink (TagOpen n as)  = TagOpen n <$> shrink as
  shrink (TagClose _)    = []
  shrink (ContentText _) = []
  shrink (ContentChar _) = []
  shrink (Comment b)     = Comment . B.fromText <$> (shrink . TL.toStrict . B.toLazyText $ b)
  shrink (Doctype t)     = Doctype <$> shrink t

instance Arbitrary Attr where
  arbitrary = Attr <$> validXmlTag <*> validXmlText
  shrink (Attr k v) = Attr <$> shrink k <*> shrink v

validOpen :: Gen Token
validOpen = TagOpen <$> validXmlTag <*> arbitrary

validClose :: Gen Token
validClose = TagClose <$> validXmlTag

validFlat :: Gen Token
validFlat = oneof
    [ ContentChar <$> validXmlChar
    , ContentText <$> validXmlText
    , Comment . B.fromText <$> validXmlText
    , Doctype <$> validXmlText
    ]

validXmlChar :: Gen Char
validXmlChar = elements (' ' : ['a'..'z'])  -- FIXME: generate wider range of values

validXmlTag :: Gen T.Text
validXmlTag = T.pack <$> sized (`maxListOf1` elements ['a'..'z'])  -- FIXME: generate wider range of values

validXmlText :: Gen T.Text
validXmlText = mconcat <$> sized (`maxListOf` (T.cons <$> validXmlChar <*> validXmlTag))  -- FIXME: generate wider range of values

maxListOf :: Int -> Gen a -> Gen [a]
maxListOf n g = take n <$> listOf g

maxListOf1 :: Int -> Gen a -> Gen [a]
maxListOf1 (min 1 -> n) g = take n <$> listOf1 g



spec :: Spec
spec = do
  it "parseTokens and renderTokens are inverse" . property . forAllShrink arbitrary shrink $
    \(canonicalizeTokens -> tokens)
      -> (parseTokens . TL.toStrict . renderTokens $ tokens) `shouldBe` tokens

  it "canonicalizeTokens is idempotent" . property . forAllShrink arbitrary shrink $
    \tokens
      -> canonicalizeTokens tokens `shouldBe` canonicalizeTokens (canonicalizeTokens tokens)
