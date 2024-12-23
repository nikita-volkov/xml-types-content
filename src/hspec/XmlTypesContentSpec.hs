module XmlTypesContentSpec (spec) where

import qualified Data.ByteString.Lazy
import qualified Data.Text as Text
import Data.XML.Types
import LawfulConversions
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import qualified Text.XML
import qualified Text.XML.Stream.Parse
import qualified Text.XML.Unresolved
import qualified XmlTypesContent
import Prelude

spec :: Spec
spec = do
  describe "textNodes" do
    it "Properly encodes a control character" do
      let text = "a&b"
      XmlTypesContent.textContents text
        `shouldBe` [ContentText "a", ContentEntity "amp", ContentText "b"]

    it "Properly encodes an undefined character" do
      let text = "a\64976b"
      XmlTypesContent.textContents text
        `shouldBe` [ContentText "a", ContentEntity "#64976", ContentText "b"]

    prop "Produces nodes that decode to the source" do
      -- For some reason xml-conduit remaps '\r' to '\n', so we simply work around that.
      text <- Text.pack <$> suchThat arbitrary (not . elem '\r')
      pure case extractTextContentFromXml (textXml text) of
        Right decodedText ->
          if decodedText == text
            then succeeded
            else failed {Test.QuickCheck.Property.reason}
          where
            reason =
              [ show text,
                "\n  =/=\n",
                show decodedText,
                "\nXML:\n",
                to @_ @Text (from (textXml text))
              ]
                & mconcat
        Left parsingErr ->
          failed {Test.QuickCheck.Property.reason}
          where
            reason =
              [ "Failed to parse due to: ",
                parsingErr,
                "\n",
                "XML:\n",
                from (textXml text)
              ]
                & mconcat
                & to

textXml :: Text -> Data.ByteString.Lazy.ByteString
textXml = nodesXml . fmap NodeContent . XmlTypesContent.textContents

nodesXml :: [Data.XML.Types.Node] -> Data.ByteString.Lazy.ByteString
nodesXml =
  buildLbs . buildDocument . buildRootElement
  where
    buildRootElement elementNodes =
      Data.XML.Types.Element
        { elementName = "a",
          elementAttributes = [],
          elementNodes
        }
    buildDocument documentRoot =
      Data.XML.Types.Document
        { documentPrologue =
            Data.XML.Types.Prologue [] Nothing [],
          documentEpilogue =
            [],
          documentRoot
        }
    buildLbs =
      Text.XML.Unresolved.renderLBS Text.XML.Unresolved.def

extractTextContentFromXml :: Data.ByteString.Lazy.ByteString -> Either Text Text
extractTextContentFromXml = parseLbs >=> parseAst
  where
    parseLbs =
      first (fromString . displayException) . Text.XML.parseLBS settings
      where
        settings =
          Text.XML.def
            { Text.XML.Stream.Parse.psDecodeIllegalCharacters = Just . chr
            }
    parseAst =
      parseNodes . Text.XML.elementNodes . Text.XML.documentRoot
      where
        parseNodes = \case
          [Text.XML.NodeContent content] -> pure content
          [] -> pure ""
          _ -> Left "Invalid nodes"
