module XmlTypesContent
  ( textContents,
  )
where

import Data.Char
import Data.Function
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Types
import qualified TextBuilderDev
import Prelude

-- |
-- Convert 'Text' into 'Content' list, escaping undefined Unicode chars as described in https://www.w3.org/TR/xml11/#charsets.
textContents :: Text -> [Content]
textContents text =
  case Text.span charNeedsNoEscaping text of
    (prefix, remainder) ->
      if Text.null prefix
        then entityAndTail
        else ContentText prefix : entityAndTail
      where
        entityAndTail =
          case Text.uncons remainder of
            Nothing -> []
            Just (charToEscape, nextText) ->
              charEntityContent charToEscape : textContents nextText

charEntityContent :: Char -> Content
charEntityContent = ContentEntity . codepointEntityText . ord

codepointEntityText :: Int -> Text
codepointEntityText = \case
  62 -> "gt"
  60 -> "lt"
  38 -> "amp"
  34 -> "quot"
  39 -> "apos"
  codepoint -> codepointDecimalEntityText codepoint

codepointDecimalEntityText :: Int -> Text
codepointDecimalEntityText codepoint =
  TextBuilderDev.fromTextBuilder ("#" <> TextBuilderDev.unsignedDecimal codepoint)

charNeedsNoEscaping :: Char -> Bool
charNeedsNoEscaping = codepointNeedsNoEscaping . ord

codepointNeedsNoEscaping :: Int -> Bool
codepointNeedsNoEscaping codepoint =
  isNotControl && isNotUndefined
  where
    isNotControl =
      not (elem codepoint controlCodepoints)
    isNotUndefined =
      undefinedUnicodeRanges
        & fmap (\(a, b) -> codepoint < a || b < codepoint)
        & and

controlCodepoints :: [Int]
controlCodepoints =
  [ 62, -- >
    60, -- <
    38, -- &
    34, -- "
    39 -- '
  ]

-- |
-- Source: https://www.w3.org/TR/xml11/#charsets.
undefinedUnicodeRanges :: [(Int, Int)]
undefinedUnicodeRanges =
  [ (0x1, 0x8),
    (0xB, 0xC),
    (0xE, 0x1F),
    (0x7F, 0x84),
    (0x86, 0x9F),
    (0xFDD0, 0xFDDF),
    (0x1FFFE, 0x1FFFF),
    (0x2FFFE, 0x2FFFF),
    (0x3FFFE, 0x3FFFF),
    (0x4FFFE, 0x4FFFF),
    (0x5FFFE, 0x5FFFF),
    (0x6FFFE, 0x6FFFF),
    (0x7FFFE, 0x7FFFF),
    (0x8FFFE, 0x8FFFF),
    (0x9FFFE, 0x9FFFF),
    (0xAFFFE, 0xAFFFF),
    (0xBFFFE, 0xBFFFF),
    (0xCFFFE, 0xCFFFF),
    (0xDFFFE, 0xDFFFF),
    (0xEFFFE, 0xEFFFF),
    (0xFFFFE, 0xFFFFF),
    (0x10FFFE, 0x10FFFF)
  ]
