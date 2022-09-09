{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Text.HTML.Tree
    ( -- * Constructing forests
      tokensToForest
    , ParseTokenForestError(..), PStack(..)
    , nonClosing
      -- * Deconstructing forests
    , tokensFromForest
    , tokensFromTree
    ) where

import           Data.Monoid
import           Data.Text (Text)
import           Data.Tree
import           Prelude

import           Text.HTML.Parser

-- | construct a 'Forest' from a 'Token' list.
--
-- This code correctly handles void elements. Void elements are required to have a start tag and must not have an end tag. See 'nonClosing'.
--
-- This code does __not__ correctly handle optional tags. It assumes all optional start and end tags are present.
--
-- <https:\/\/www.w3.org\/TR\/html52\/syntax.html#optional-tags>
tokensToForest :: [Token] -> Either ParseTokenForestError (Forest Token)
tokensToForest = f (PStack [] [])
  where
    f (PStack ss []) [] = Right (reverse ss)
    f pstack []         = Left $ ParseTokenForestErrorBracketMismatch pstack Nothing
    f pstack (t : ts)   = case t of
        TagOpen n _     -> if n `elem` nonClosing
                             then f (pushFlatSibling t pstack) ts
                             else f (pushParent t pstack) ts
        TagSelfClose {} -> f (pushFlatSibling t pstack) ts
        TagClose n      -> (`f` ts) =<< popParent n pstack
        ContentChar _   -> f (pushFlatSibling t pstack) ts
        ContentText _   -> f (pushFlatSibling t pstack) ts
        Comment _       -> f (pushFlatSibling t pstack) ts
        Doctype _       -> f (pushFlatSibling t pstack) ts

-- | void elements which must not have an end tag
--
-- This list does not include the obsolete @\<command\>@ and @\<keygen\>@ elements.
--
-- @ nonClosing = ["br", "hr", "img", "meta", "area", "base", "col", "embed", "input", "link", "param", "source", "track", "wbr"] @
--
-- <https:\/\/www.w3.org\/TR\/html52\/syntax.html#void-elements>
nonClosing :: [Text]
nonClosing = ["br", "hr", "img", "meta", "area", "base", "col", "embed", "input", "link", "param", "source", "track", "wbr"]

data ParseTokenForestError =
    ParseTokenForestErrorBracketMismatch PStack (Maybe Token)
  deriving (Eq, Show)

data PStack = PStack
    { _pstackToplevelSiblings :: Forest Token
    , _pstackParents          :: [(Token, Forest Token)]
    }
  deriving (Eq, Show)

pushParent :: Token -> PStack -> PStack
pushParent t (PStack ss ps) = PStack [] ((t, ss) : ps)

popParent :: TagName -> PStack -> Either ParseTokenForestError PStack
popParent n (PStack ss ((p@(TagOpen n' _), ss') : ps))
    | n == n' = Right $ PStack (Node p (reverse ss) : ss') ps
popParent n pstack
    = Left $ ParseTokenForestErrorBracketMismatch pstack (Just $ TagClose n)

pushFlatSibling :: Token -> PStack -> PStack
pushFlatSibling t (PStack ss ps) = PStack (Node t [] : ss) ps

-- | convert a 'Forest' of 'Token' into a list of 'Token'.
--
-- This code correctly handles void elements. Void elements are required to have a start tag and must not have an end tag. See 'nonClosing'.
tokensFromForest :: Forest Token -> [Token]
tokensFromForest = mconcat . fmap tokensFromTree

-- | convert a 'Tree' of 'Token' into a list of 'Token'.
--
-- This code correctly handles void elements. Void elements are required to have a start tag and must not have an end tag. See 'nonClosing'.
tokensFromTree :: Tree Token -> [Token]
tokensFromTree (Node o@(TagOpen n _) ts) | n `notElem` nonClosing
    = [o] <> tokensFromForest ts <> [TagClose n]
tokensFromTree (Node t [])
    = [t]
tokensFromTree _
    = error "renderTokenTree: leaf node with children."
