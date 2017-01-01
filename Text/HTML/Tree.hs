{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Text.HTML.Tree
    ( -- * Class
      IsToken(..)
      -- * Parsing forests
    , tokensToForest
    , ParseTokenForestError(..), PStack(..)
      -- * Rendering forests
    , tokensFromForest
    , tokensFromTree
    ) where

import           Data.Monoid
import           Data.Tree
import           Text.HTML.Parser


-- | If you want to store additional data into your tree, you can define your own token type and
-- instantiate this class to define a mapping between your type and 'Token'.  See
-- 'parseTokenForest'.
class IsToken a where
    toToken   :: a -> Maybe Token
    fromToken :: Token -> a

instance IsToken Token where
    toToken   = Just
    fromToken = id


tokensToForest :: (IsToken token) => [token] -> Either (ParseTokenForestError token) (Forest token)
tokensToForest = f (PStack [] [])
  where
    f (PStack ss []) [] = Right (reverse ss)
    f pstack []         = Left $ ParseTokenForestErrorBracketMismatch pstack Nothing
    f pstack (t : ts)   = case toToken t of
        Just (TagOpen "br" _)  -> f (pushFlatSibling t pstack) ts
        Just (TagOpen "hr" _)  -> f (pushFlatSibling t pstack) ts
        Just (TagOpen "img" _) -> f (pushFlatSibling t pstack) ts
        Just (TagOpen _ _)     -> f (pushParent t pstack) ts
        Just (TagClose n)      -> (`f` ts) =<< popParent n pstack
        Just (ContentChar _)   -> f (pushFlatSibling t pstack) ts
        Just (ContentText _)   -> f (pushFlatSibling t pstack) ts
        Just (Comment _)       -> f (pushFlatSibling t pstack) ts
        Just (Doctype _)       -> f (pushFlatSibling t pstack) ts
        Nothing                -> f (pushFlatSibling t pstack) ts

data ParseTokenForestError t =
    ParseTokenForestErrorBracketMismatch (PStack t) (Maybe Token)
  deriving (Eq, Show)

data PStack t = PStack
    { _pstackToplevelSiblings :: Forest t
    , _pstackParents          :: [(t, Forest t)]
    }
  deriving (Eq, Show)

pushParent :: t -> PStack t -> PStack t
pushParent t (PStack ss ps) = PStack [] ((t, ss) : ps)

popParent :: (IsToken t) => TagName -> PStack t -> Either (ParseTokenForestError t) (PStack t)
popParent n (PStack ss ((p@(toToken -> Just (TagOpen n' _)), ss') : ps))
    | n == n' = Right $ PStack (Node p (reverse ss) : ss') ps
popParent n pstack
    = Left $ ParseTokenForestErrorBracketMismatch pstack (Just $ TagClose n)

pushFlatSibling :: t -> PStack t -> PStack t
pushFlatSibling t (PStack ss ps) = PStack (Node t [] : ss) ps


renderTokenForest :: (IsToken t) => Forest t -> [t]
renderTokenForest = mconcat . fmap renderTokenTree

renderTokenTree :: (IsToken t) => Tree t -> [t]
renderTokenTree (Node o@(toToken -> Just (TagOpen n _)) ts)
    = [o] <> renderTokenForest ts <> [fromToken $ TagClose n]
renderTokenTree (Node t []) =
    = [t]
renderTokenTree _
    = error "renderTokenTree: leaf node with children."
