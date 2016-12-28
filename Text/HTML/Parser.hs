{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | This is a performance-oriented HTML tokenizer aim at web-crawling
-- applications. It follows the HTML5 parsing specification quite closely,
-- so it behaves reasonable well on ill-formed documents from the open Web.
--
module Text.HTML.Parser
    ( -- * Parsing
      parseTokens
    , parseTokensLazy
    , token
      -- * Types
    , Token(..)
    , TagName, AttrName, AttrValue
    , Attr(..)
      -- * Rendering, text canonicalization
    , renderTokens
    , renderToken
    , renderAttrs
    , renderAttr
    , canonicalizeTokens
    ) where

import Data.Char hiding (isSpace)
import Data.List (unfoldr)
import GHC.Generics
import Control.Applicative
import Data.Monoid
import Control.Monad (guard)
import Control.DeepSeq

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy as AL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Prelude hiding (take, takeWhile)

-- | A tag name (e.g. @body@)
type TagName   = Text

-- | An attribute name (e.g. @href@)
type AttrName  = Text

-- | The value of an attribute
type AttrValue = Text

-- | An HTML token
data Token
  -- | An opening tag. Attribute ordering is arbitrary.
  = TagOpen !TagName [Attr]
  -- | A closing tag.
  | TagClose !TagName
  -- | The content between tags.
  | ContentText !Text
  -- | A single character of content
  | ContentChar !Char
  -- | Contents of a comment.
  | Comment !Builder
  -- | Doctype
  | Doctype !Text
  deriving (Show, Ord, Eq, Generic)

-- | An attribute of a tag
data Attr = Attr !AttrName !AttrValue
          deriving (Show, Eq, Ord)

instance NFData Token where
    rnf (Comment b) = rnf $ B.toLazyText b
    rnf _           = ()

-- | Parse a single 'Token'.
token :: Parser Token
token = dataState -- Start in the data state.

-- | /§8.2.4.1/: Data state
dataState :: Parser Token
dataState = do
    content <- takeWhile (/= '<')
    if not $ T.null content
      then return $ ContentText content
      else char '<' >> tagOpen

-- | /§8.2.4.3/: Tag open state
tagOpen :: Parser Token
tagOpen =
        (char '!' >> markupDeclOpen)
    <|> (char '/' >> endTagOpen)
    <|> (char '?' >> bogusComment)
    <|> tryStartTag
    <|> other
  where
    tryStartTag = do
        c <- peekChar'
        guard $ isAsciiUpper c || isAsciiLower c
        tagName

    other = do
        return $ ContentChar '<'

-- | /§8.2.4.9/: End tag open state
-- TODO: This isn't right
endTagOpen :: Parser Token
endTagOpen = do
    name <- takeWhile $ \c -> isAsciiUpper c || isAsciiLower c
    char '>'
    return $ TagClose name

-- | /§8.2.4.10/: Tag name state
--
-- deviation: no lower-casing
tagName :: Parser Token
tagName = do
    tag <- takeWhile $ notInClass "\x09\x0a\x0c />"
    id $  (satisfy (inClass "\x09\x0a\x0c ") >> beforeAttrName tag [])
      <|> (char '/' >> selfClosingStartTag tag [])
      <|> (char '>' >> return (TagOpen tag []))

-- | /§8.2.4.43/: Self-closing start tag state
selfClosingStartTag :: TagName -> [Attr] -> Parser Token
selfClosingStartTag tag attrs = do
        (char '>' >> return (TagOpen tag attrs))
    <|> beforeAttrName tag attrs

-- | /§8.2.4.34/: Before attribute name state
--
-- deviation: no lower-casing
beforeAttrName :: TagName -> [Attr] -> Parser Token
beforeAttrName tag attrs = do
    skipWhile $ inClass "\x09\x0a\x0c "
    id $  (char '/' >> selfClosingStartTag tag attrs)
      <|> (char '>' >> return (TagOpen tag attrs))
      -- <|> (char '\x00' >> attrName tag attrs) -- TODO: NULL
      <|> attrName tag attrs

-- | /§8.2.4.35/: Attribute name state
attrName :: TagName -> [Attr] -> Parser Token
attrName tag attrs = do
    name <- takeWhile $ notInClass "\x09\x0a\x0c /=>\x00"
    id $  (satisfy (inClass "\x09\x0a\x0c ") >> afterAttrName tag attrs name)
      <|> (char '/' >> selfClosingStartTag tag attrs)
      <|> (char '=' >> beforeAttrValue tag attrs name)
      <|> (char '>' >> return (TagOpen tag (Attr name T.empty : attrs)))
      -- <|> -- TODO: NULL

-- | /§8.2.4.36/: After attribute name state
afterAttrName :: TagName -> [Attr] -> AttrName -> Parser Token
afterAttrName tag attrs name = do
    skipWhile $ inClass "\x09\x0a\x0c "
    id $  (char '/' >> selfClosingStartTag tag attrs)
      <|> (char '=' >> beforeAttrValue tag attrs name)
      <|> (char '>' >> return (TagOpen tag (Attr name T.empty : attrs)))
      <|> attrName tag (Attr name T.empty : attrs)  -- not exactly sure this is right

-- | /§8.2.4.37/: Before attribute value state
beforeAttrValue :: TagName -> [Attr] -> AttrName -> Parser Token
beforeAttrValue tag attrs name = do
    skipWhile $ inClass "\x09\x0a\x0c "
    id $  (char '"' >> attrValueDQuoted tag attrs name)
      <|> (char '\'' >> attrValueSQuoted tag attrs name)
      <|> (char '>' >> return (TagOpen tag (Attr name T.empty : attrs)))
      <|> attrValueUnquoted tag attrs name

-- | /§8.2.4.38/: Attribute value (double-quoted) state
attrValueDQuoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueDQuoted tag attrs name = do
    value <- takeWhile (/= '"')
    char '"'
    afterAttrValueQuoted tag attrs name value

-- | /§8.2.4.39/: Attribute value (single-quoted) state
attrValueSQuoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueSQuoted tag attrs name = do
    value <- takeWhile (/= '\'')
    char '\''
    afterAttrValueQuoted tag attrs name value

-- | /§8.2.4.40/: Attribute value (unquoted) state
attrValueUnquoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueUnquoted tag attrs name = do
    value <- takeTill (inClass "\x09\x0a\x0c >")
    id $  (satisfy (inClass "\x09\x0a\x0c ") >> beforeAttrName tag attrs) -- unsure: don't emit?
      <|> (char '>' >> return (TagOpen tag (Attr name value : attrs)))

-- | /§8.2.4.42/: After attribute value (quoted) state
afterAttrValueQuoted :: TagName -> [Attr] -> AttrName -> AttrValue -> Parser Token
afterAttrValueQuoted tag attrs name value =
          (satisfy (inClass "\x09\x0a\x0c ") >> beforeAttrName tag attrs')
      <|> (char '/' >> selfClosingStartTag tag attrs')
      <|> (char '>' >> return (TagOpen tag attrs'))
  where attrs' = Attr name value : attrs

-- | /§8.2.4.45/: Markup declaration open state
markupDeclOpen :: Parser Token
markupDeclOpen =
        try comment
    <|> try docType
        -- TODO: Fix the rest
  where
    comment = string "--" >> commentStart
    docType = do
        -- switching this to asciiCI slowed things down by a factor of two
        s <- take 7
        guard $ T.toLower s == "doctype"
        doctype

-- | /§8.2.4.46/: Comment start state
commentStart :: Parser Token
commentStart = do
          (char '-' >> commentStartDash)
      <|> (char '>' >> return (Comment mempty))
      <|> comment mempty

-- | /§8.2.4.47/: Comment start dash state
commentStartDash :: Parser Token
commentStartDash =
          (char '-' >> commentEnd mempty)
      <|> (char '>' >> return (Comment mempty))
      <|> (do c <- anyChar
              comment (B.singleton '-' <> B.singleton c) )

-- | /§8.2.4.48/: Comment state
comment :: Builder -> Parser Token
comment content0 = do
    content <- B.fromText <$> takeWhile (notInClass "-")
    id $  (char '-' >> commentEndDash (content0 <> content))
      <|> (char '\x00' >> comment (content0 <> content <> B.singleton '\xfffd'))

-- | /§8.2.4.49/: Comment end dash state
commentEndDash :: Builder -> Parser Token
commentEndDash content = do
        (char '-' >> commentEnd content)
    <|> (char '\x00' >> comment (content <> "-\xfffd"))
    <|> (anyChar >>= \c -> comment (content <> "-" <> B.singleton c))

-- | /§8.2.4.50/: Comment end state
commentEnd :: Builder -> Parser Token
commentEnd content = do
        (char '>' >> return (Comment content))
    <|> (char '\x00' >> comment (content <> "-\xfffd"))
    -- <|> ()  TODO: other cases
    <|> (anyChar >>= \c -> comment (content <> "-" <> B.singleton c))

-- | /§8.2.4.52/: DOCTYPE state
-- FIXME
doctype :: Parser Token
doctype = do
    content <- takeTill (=='>')
    char '>'
    return $ Doctype content

-- | /§8.2.4.44/: Bogus comment state
bogusComment :: Parser Token
bogusComment = fail "Bogus comment"

-- | Parse a lazy list of tokens from strict 'Text'.
parseTokens :: Text -> [Token]
parseTokens = unfoldr f
  where
    f :: Text -> Maybe (Token, Text)
    f t
      | T.null t = Nothing
      | otherwise =
        case parse token t of
            Done rest tok -> Just (tok, rest)
            Partial cont  ->
                case cont mempty of
                  Done rest tok -> Just (tok, rest)
                  _             -> Nothing
            _             -> Nothing

-- | Parse a lazy list of tokens from lazy 'TL.Text'.
parseTokensLazy :: TL.Text -> [Token]
parseTokensLazy = unfoldr f
  where
    f :: TL.Text -> Maybe (Token, TL.Text)
    f t
      | TL.null t = Nothing
      | otherwise =
        case AL.parse token t of
            AL.Done rest tok -> Just (tok, rest)
            _                -> Nothing

-- | See 'renderToken'.
renderTokens :: [Token] -> TL.Text
renderTokens = mconcat . fmap renderToken

-- | (Somewhat) canonical string representation of 'Token'.
renderToken :: Token -> TL.Text
renderToken = TL.fromStrict . mconcat . \case
    (TagOpen n [])    -> ["<", n, ">"]
    (TagOpen n attrs) -> ["<", n, " ", renderAttrs attrs, ">"]
    (TagClose n)      -> ["</", n, ">"]
    (ContentChar c)   -> [T.singleton c]
    (ContentText t)   -> [t]
    (Comment builder) -> ["<!--", TL.toStrict $ B.toLazyText builder, "-->"]
    (Doctype t)       -> ["<!DOCTYPE", t, ">"]

-- | See 'renderAttr'.
renderAttrs :: [Attr] -> Text
renderAttrs = T.unwords . fmap renderAttr . reverse

-- | Does not escape quotation in attribute values!
renderAttr :: Attr -> Text
renderAttr (Attr k v) = mconcat [k, "=\"", v, "\""]

-- | Meld neighoring 'ContentChar' and 'ContentText' constructors together and drops empty text
-- elements.
canonicalizeTokens :: [Token] -> [Token]
canonicalizeTokens = filter (/= ContentText "") . meldTextTokens

meldTextTokens :: [Token] -> [Token]
meldTextTokens = concatTexts . fmap charToText
  where
    charToText (ContentChar c) = ContentText (T.singleton c)
    charToText t = t

    concatTexts = \case
      (ContentText t : ContentText t' : ts) -> concatTexts $ ContentText (t <> t') : ts
      (t : ts) -> t : concatTexts ts
      [] -> []
