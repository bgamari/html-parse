{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -O2 #-}

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

-- Section numbers refer to W3C HTML 5.2 specification.

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
  -- | A self-closing tag.
  | TagSelfClose !TagName [Attr]
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

-- | This is a bit of a hack
endOfFileToken :: Token
endOfFileToken = ContentText ""

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

-- | /§8.2.4.6/: Tag open state
tagOpen :: Parser Token
tagOpen =
        (char '!' >> markupDeclOpen)
    <|> (char '/' >> endTagOpen)
    <|> (char '?' >> bogusComment mempty)
    <|> tagNameOpen
    <|> other
  where
    other = do
        return $ ContentChar '<'

-- | /§8.2.4.7/: End tag open state
endTagOpen :: Parser Token
endTagOpen = tagNameClose

-- | Equivalent to @inClass "\x09\x0a\x0c "@
isWhitespace :: Char -> Bool
isWhitespace '\x09' = True
isWhitespace '\x0a' = True
isWhitespace '\x0c' = True
isWhitespace ' '    = True
isWhitespace _      = False

orC :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
orC f g c = f c || g c
{-# INLINE orC #-}

isC :: Char -> Char -> Bool
isC = (==)
{-# INLINE isC #-}

-- | /§8.2.4.8/: Tag name state: the open case
--
-- deviation: no lower-casing, don't handle NULL characters
tagNameOpen :: Parser Token
tagNameOpen = do
    tag <- tagName'
    id $  (satisfy isWhitespace >> beforeAttrName tag [])
      <|> (char '/' >> selfClosingStartTag tag [])
      <|> (char '>' >> return (TagOpen tag []))

-- | /§8.2.4.10/: Tag name state: close case
tagNameClose :: Parser Token
tagNameClose = do
    tag <- tagName'
    char '>' >> return (TagClose tag)

-- | /§8.2.4.10/: Tag name state: common code
--
-- deviation: no lower-casing, don't handle NULL characters
tagName' :: Parser Text
tagName' = do
    c <- peekChar'
    guard $ isAsciiUpper c || isAsciiLower c
    takeWhile $ not . (isWhitespace `orC` isC '/' `orC` isC '<' `orC` isC '>')

-- | /§8.2.4.40/: Self-closing start tag state
selfClosingStartTag :: TagName -> [Attr] -> Parser Token
selfClosingStartTag tag attrs = do
        (char '>' >> return (TagSelfClose tag attrs))
    <|> (endOfInput >> return endOfFileToken)
    <|> beforeAttrName tag attrs

-- | /§8.2.4.32/: Before attribute name state
--
-- deviation: no lower-casing
beforeAttrName :: TagName -> [Attr] -> Parser Token
beforeAttrName tag attrs = do
    skipWhile isWhitespace
    id $  (char '/' >> selfClosingStartTag tag attrs)
      <|> (char '>' >> return (TagOpen tag attrs))
      -- <|> (char '\x00' >> attrName tag attrs) -- TODO: NULL
      <|> attrName tag attrs

-- | /§8.2.4.33/: Attribute name state
attrName :: TagName -> [Attr] -> Parser Token
attrName tag attrs = do
    name <- takeWhile $ not . (isWhitespace `orC` isC '/' `orC` isC '=' `orC` isC '>')
    id $  (endOfInput >> afterAttrName tag attrs name)
      <|> (char '=' >> beforeAttrValue tag attrs name)
      <|> try (do mc <- peekChar
                  case mc of
                    Just c | notNameChar c ->  afterAttrName tag attrs name
                    _ -> empty)
      -- <|> -- TODO: NULL
  where notNameChar = isWhitespace `orC` isC '/' `orC` isC '>'

-- | /§8.2.4.34/: After attribute name state
afterAttrName :: TagName -> [Attr] -> AttrName -> Parser Token
afterAttrName tag attrs name = do
    skipWhile isWhitespace
    id $  (char '/' >> selfClosingStartTag tag attrs)
      <|> (char '=' >> beforeAttrValue tag attrs name)
      <|> (char '>' >> return (TagOpen tag (Attr name T.empty : attrs)))
      <|> (endOfInput >> return endOfFileToken)
      <|> attrName tag (Attr name T.empty : attrs)  -- not exactly sure this is right

-- | /§8.2.4.35/: Before attribute value state
beforeAttrValue :: TagName -> [Attr] -> AttrName -> Parser Token
beforeAttrValue tag attrs name = do
    skipWhile isWhitespace
    id $  (char '"' >> attrValueDQuoted tag attrs name)
      <|> (char '\'' >> attrValueSQuoted tag attrs name)
      <|> (char '>' >> return (TagOpen tag (Attr name T.empty : attrs)))
      <|> attrValueUnquoted tag attrs name

-- | /§8.2.4.36/: Attribute value (double-quoted) state
attrValueDQuoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueDQuoted tag attrs name = do
    value <- takeWhile (/= '"')
    _ <- char '"'
    afterAttrValueQuoted tag attrs name value

-- | /§8.2.4.37/: Attribute value (single-quoted) state
attrValueSQuoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueSQuoted tag attrs name = do
    value <- takeWhile (/= '\'')
    _ <- char '\''
    afterAttrValueQuoted tag attrs name value

-- | /§8.2.4.38/: Attribute value (unquoted) state
attrValueUnquoted :: TagName -> [Attr] -> AttrName -> Parser Token
attrValueUnquoted tag attrs name = do
    value <- takeTill (inClass "\x09\x0a\x0c >")
    id $  (satisfy isWhitespace >> beforeAttrName tag attrs) -- unsure: don't emit?
      <|> (char '>' >> return (TagOpen tag (Attr name value : attrs)))
      <|> (endOfInput >> return endOfFileToken)

-- | /§8.2.4.39/: After attribute value (quoted) state
afterAttrValueQuoted :: TagName -> [Attr] -> AttrName -> AttrValue -> Parser Token
afterAttrValueQuoted tag attrs name value =
          (satisfy isWhitespace >> beforeAttrName tag attrs')
      <|> (char '/' >> selfClosingStartTag tag attrs')
      <|> (char '>' >> return (TagOpen tag attrs'))
      <|> (endOfInput >> return endOfFileToken)
  where attrs' = Attr name value : attrs

-- | /§8.2.4.41/: Bogus comment state
bogusComment :: Builder -> Parser Token
bogusComment content = do
        (char '>' >> return (Comment content))
    <|> (endOfInput >> return (Comment content))
    <|> (char '\x00' >> bogusComment (content <> "\xfffd"))
    <|> (anyChar >>= \c -> bogusComment (content <> B.singleton c))

-- | /§8.2.4.42/: Markup declaration open state
markupDeclOpen :: Parser Token
markupDeclOpen =
        try comment_
    <|> try docType
    <|> bogusComment mempty
  where
    comment_ = char '-' >> char '-' >> commentStart
    docType = do
        -- switching this to asciiCI slowed things down by a factor of two
        s <- take 7
        guard $ T.toLower s == "doctype"
        doctype

-- | /§8.2.4.43/: Comment start state
commentStart :: Parser Token
commentStart = do
          (char '-' >> commentStartDash)
      <|> (char '>' >> return (Comment mempty))
      <|> comment mempty

-- | /§8.2.4.44/: Comment start dash state
commentStartDash :: Parser Token
commentStartDash =
          (char '-' >> commentEnd mempty)
      <|> (char '>' >> return (Comment mempty))
      <|> (endOfInput >> return (Comment mempty))
      <|> (comment (B.singleton '-'))

-- | /§8.2.4.45/: Comment state
comment :: Builder -> Parser Token
comment content0 = do
    content <- B.fromText <$> takeWhile (not . (isC '-' `orC` isC '\x00' `orC` isC '<'))
    id $  (char '<' >> commentLessThan (content0 <> content <> "<"))
      <|> (char '-' >> commentEndDash (content0 <> content))
      <|> (char '\x00' >> comment (content0 <> content <> B.singleton '\xfffd'))
      <|> (endOfInput >> return (Comment $ content0 <> content))

-- | /§8.2.46/: Comment less-than sign state
commentLessThan :: Builder -> Parser Token
commentLessThan content =
        (char '!' >> commentLessThanBang (content <> "!"))
    <|> (char '<' >> commentLessThan (content <> "<"))
    <|> comment content

-- | /§8.2.47/: Comment less-than sign bang state
commentLessThanBang :: Builder -> Parser Token
commentLessThanBang content =
        (char '-' >> commentLessThanBangDash content)
    <|> comment content

-- | /§8.2.48/: Comment less-than sign bang dash state
commentLessThanBangDash :: Builder -> Parser Token
commentLessThanBangDash content =
        (char '-' >> commentLessThanBangDashDash content)
    <|> commentEndDash content

-- | /§8.2.49/: Comment less-than sign bang dash dash state
commentLessThanBangDashDash :: Builder -> Parser Token
commentLessThanBangDashDash content =
        (char '>' >> comment content)
    <|> (endOfInput >> comment content)
    <|> commentEnd content

-- | /§8.2.4.50/: Comment end dash state
commentEndDash :: Builder -> Parser Token
commentEndDash content = do
        (char '-' >> commentEnd content)
    <|> (endOfInput >> return (Comment content))
    <|> (comment (content <> "-"))

-- | /§8.2.4.51/: Comment end state
commentEnd :: Builder -> Parser Token
commentEnd content = do
        (char '>' >> return (Comment content))
    <|> (char '!' >> commentEndBang content)
    <|> (char '-' >> commentEnd (content <> "-"))
    <|> (endOfInput >> return (Comment content))
    <|> (comment (content <> "--"))

-- | /§8.2.4.52/: Comment end bang state
commentEndBang :: Builder -> Parser Token
commentEndBang content = do
        (char '-' >> commentEndDash (content <> "--!"))
    <|> (char '>' >> return (Comment content))
    <|> (endOfInput >> return (Comment content))
    <|> (comment (content <> "--!"))

-- | /§8.2.4.53/: DOCTYPE state
-- FIXME
doctype :: Parser Token
doctype = do
    content <- takeTill (=='>')
    _ <- char '>'
    return $ Doctype content

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
    (TagOpen n [])         -> ["<", n, ">"]
    (TagOpen n attrs)      -> ["<", n, " ", renderAttrs attrs, ">"]
    (TagSelfClose n attrs) -> ["<", n, " ", renderAttrs attrs, " />"]
    (TagClose n)           -> ["</", n, ">"]
    (ContentChar c)        -> [T.singleton c]
    (ContentText t)        -> [t]
    (Comment builder)      -> ["<!--", TL.toStrict $ B.toLazyText builder, "-->"]
    (Doctype t)            -> ["<!DOCTYPE", t, ">"]

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
