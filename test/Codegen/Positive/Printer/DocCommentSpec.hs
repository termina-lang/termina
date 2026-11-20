{-# LANGUAGE OverloadedStrings #-}
-- | The documentation comments of the C: the Doxygen block of a 'CDoc', its
-- wrapping, and the two escapes of the text of the author.
module Codegen.Positive.Printer.DocCommentSpec (spec) where

import Control.Monad (replicateM)
import Control.Monad.Reader (runReader)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (indent, vsep)

import Doc.Model
import Generator.CodeGen.Common (internalAnn)
import Generator.LanguageC.AST
import Generator.LanguageC.DocPrinter
import Generator.LanguageC.Embedded (pre_cr)
import Generator.LanguageC.Printer (CPrinterConfig(..), pprint)
import Utils.Printer (render)

import Test.Hspec

-- | The comment of the prototype of CSPIPSDriver::end, which takes the one of
-- ISPIBus::end, with a section of errors and one of derived facts.
spipsEnd :: CDoc
spipsEnd = CDoc
  [DText "Ends the transaction and releases the bus."]
  [DPara [ DText "As master, the bytes still being shifted are waited for and the chip", DSoftBreak
         , DText "select is deasserted. The bytes not read back are discarded, and the", DSoftBreak
         , DText "transaction is then reported as ", DLink "SPIError__Incomplete"
         , DText "; as slave,", DSoftBreak, DText "that includes the bytes the master did not shift." ]]
  [ CDocParam CParamIn "termina__ev" [DPara [DText "Event that caused the call."]]
  , CDocParam CParamInOut "termina__this" [DPara [DText "The CSPIPSDriver resource."]]
  , CDocParam CParamOut "status" [DPara [DText "Error seen during the transaction, if any."]]
  , CDocSection "Errors" [DList DBullets
      [ [DPara [DCode "NotStarted", DText ": No transaction is in course."]]
      , [DPara [DCode "Underrun", DText ": As slave, the master shifted more bytes than were written, and this line is long."]] ]]
  , CDocSection "Derived from the architecture"
      [DPara [ DText "Implements procedure ", DCode "end", DText " of interface ", DCode "ISPIBus"
             , DText ". Runs with the lock of the resource held." ]]
  ]

-- | A comment with only a brief.
brief :: Text -> CDoc
brief t = CDoc [DText t] [] []

-- | A documented struct with a member whose comment is short, one whose
-- comment is long and one without, and a documented enum with a blank line
-- added before it by 'pre_cr', which keeps its comment.
documentedItems :: [CFileItem]
documentedItems =
  [ CExtDecl (CEDStructUnion (Just "Point") (CStruct CStructTag Nothing
      [ member "x" (Just (brief "Abscissa."))
      , member "y" (Just (brief "Ordinate, measured from the bottom edge of the screen of the device."))
      , member "z" Nothing ] []))
      (internalAnn (CDeclarationAnn False (Just (brief "A point."))))
  , pre_cr (CExtDecl (CEDEnum (Just "Mode") (CEnum Nothing
      [ CEnumerator "Mode__Off" Nothing (internalAnn (CMemberAnn (Just (brief "Off."))))
      , CEnumerator "Mode__On" Nothing (internalAnn (CMemberAnn Nothing)) ] []))
      (internalAnn (CDeclarationAnn False (Just (brief "A mode.")))))
  ]

  where

    member name doc = CMember (CDecl (CTypeSpec (CTInt IntSize32 Unsigned noqual)) (Just name) Nothing) (internalAnn (CMemberAnn doc))

-- | The items of a file printed with or without their comments.
printedItems :: Bool -> [CFileItem] -> Text
printedItems docs items = render (vsep (runReader (mapM pprint items) (CPrinterConfig False False docs)))

spec :: Spec
spec = describe "Documentation comments of the C" $ do

  describe "on the declarations of a file" $ do
    it "are not printed by default" $
      printedItems False documentedItems `shouldBe` T.intercalate "\n"
        [ "typedef struct {"
        , "    uint32_t x;"
        , "    uint32_t y;"
        , "    uint32_t z;"
        , "} Point;"
        , ""
        , "typedef enum {"
        , "    Mode__Off,"
        , "    Mode__On"
        , "} Mode;" ]
    it "go before a declaration, and after a member or an enumerator when they are a short brief" $
      printedItems True documentedItems `shouldBe` T.intercalate "\n"
        [ "/**"
        , " * @brief A point."
        , " */"
        , "typedef struct {"
        , "    uint32_t x; /**< Abscissa. */"
        , "    /**"
        , "     * @brief Ordinate, measured from the bottom edge of the screen of the"
        , "     *        device."
        , "     */"
        , "    uint32_t y;"
        , "    uint32_t z;"
        , "} Point;"
        , ""
        , "/**"
        , " * @brief A mode."
        , " */"
        , "typedef enum {"
        , "    Mode__Off, /**< Off. */"
        , "    Mode__On"
        , "} Mode;" ]

  it "prints the comment of CSPIPSDriver::end" $
    render (pprintCDoc spipsEnd) `shouldBe` T.intercalate "\n"
      [ "/**"
      , " * @brief Ends the transaction and releases the bus."
      , " *"
      , " * As master, the bytes still being shifted are waited for and the chip select"
      , " * is deasserted. The bytes not read back are discarded, and the transaction is"
      , " * then reported as `SPIError__Incomplete`; as slave, that includes the bytes"
      , " * the master did not shift."
      , " *"
      , " * @param[in] termina__ev Event that caused the call."
      , " * @param[in,out] termina__this The CSPIPSDriver resource."
      , " * @param[out] status Error seen during the transaction, if any."
      , " *"
      , " * @par Errors"
      , " * - `NotStarted`: No transaction is in course."
      , " * - `Underrun`: As slave, the master shifted more bytes than were written, and"
      , " *   this line is long."
      , " *"
      , " * @par Derived from the architecture"
      , " * Implements procedure `end` of interface `ISPIBus`. Runs with the lock of the"
      , " * resource held."
      , " */" ]

  it "wraps at 80 columns from where the comment starts, keeping a dash off the start of a line" $
    render (indent 4 (pprintCDoc (brief "A transaction is already in course, and a long text that wraps - here - at the margin of the struct.")))
      `shouldBe` T.intercalate "\n"
        [ "    /**"
        , "     * @brief A transaction is already in course, and a long text that wraps -"
        , "     *        here - at the margin of the struct."
        , "     */" ]

  describe "escapes the text of the author for Doxygen" $ do
    let cases =
          [ ("user@host", "user\\@host"), ("a\\b", "a\\\\b"), ("a*b", "a\\*b"), ("a`b", "a\\`b")
          , ("$5", "\\$5"), ("snake_case", "snake_case"), ("_x_", "\\_x\\_"), ("#name", "\\#name")
          , ("C# is", "C# is"), ("50%", "50%"), ("%s", "\\%s"), ("<b>", "\\<b>"), ("a < b", "a < b")
          , ("&amp;", "\\&amp;"), ("a & b", "a & b") ]
    mapM_ (\(from, to) -> it (T.unpack from) $ escapeDoxygen from `shouldBe` to) cases

  describe "escapes a line of a comment for C" $ do
    let cases =
          [ ("/*", "/ *"), ("*/", "* /"), ("a // b", "a / / b"), ("///", "/ / /")
          , ("https://x.org/a", "https://x.org/a"), ("??=", "? ?="), ("???/", "?? ?/"), ("a?? b", "a?? b") ]
    mapM_ (\(from, to) -> it (T.unpack from) $ escapeComment from `shouldBe` to) cases

  it "never ends the comment early, starts another one or prints a trigraph, for any text up to five characters" $
    [ s | n <- [1 .. 5], s <- replicateM n "/*?=\\:a ", unsafe (render (pprintCDoc (brief (T.pack s)))) ]
      `shouldBe` []

  where

    -- A comment that holds, besides its own delimiters, a @/*@, a @*/@, a
    -- trigraph, or a @//@ that is not after the colon of a URI.
    unsafe text =
      let inside = T.drop 3 (T.dropEnd 3 text)
      in any (`T.isInfixOf` inside) ["/*", "*/"]
           || any (\(preceding, _) -> not (":" `T.isSuffixOf` preceding)) (T.breakOnAll "//" inside)
           || any (\c -> ("??" <> T.singleton c) `T.isInfixOf` inside) ("=/'()!<>-" :: String)
           || not ("/**" `T.isPrefixOf` text && " */" `T.isSuffixOf` text)
