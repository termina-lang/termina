{-# LANGUAGE OverloadedStrings #-}
-- | The documentation model of a comment: the margin, the split into body and
-- tags, the errors of the tags and the Markdown.
module Doc.ModelSpec (spec) where

import Data.List (intercalate)
import Text.Parsec.Pos (initialPos, newPos)

import Core.AST (DocComment(..), DocForm(..), DocPlacement(..))
import Doc.Model
import Utils.Annotations (Location(..))

import Test.Hspec

-- | The lines of a block comment without its margin, given its text between
-- the delimiters, line by line.
blockLines :: [String] -> [String]
blockLines = map lineText . commentLines BlockDoc . written

-- | The lines of a run of line comments without its margin, given the text
-- after each delimiter.
lineLines :: [String] -> [String]
lineLines ls = map lineText (commentLines LineDoc [ DocLine (newPos "test" n 4) l | (n, l) <- zip [1 ..] ls ])

-- | The body and the tags of a block comment, given its text between the
-- delimiters, line by line.
split :: [String] -> Either DocError ([String], [(TagKind, [String])])
split ls =
  case splitDoc (commentLines BlockDoc (written ls)) of
    Left (_, err) -> Left err
    Right (body, tags) -> Right (map lineText body, [ (tagKind t, map lineText (tagText t)) | t <- tags ])

-- | The error in the tags of a block comment, if any.
tagError :: [String] -> Maybe DocError
tagError = either Just (const Nothing) . split

written :: [String] -> [DocLine]
written = linesAt (initialPos "test") . intercalate "\n"

-- | The comment of ISPIBus::end in src/hal/spi.fin of termina-hal, between
-- its delimiters.
spiEnd :: [String]
spiEnd =
  [ ""
  , "     * Ends the transaction and releases the bus."
  , "     *"
  , "     * As master, the bytes still being shifted are waited for and the chip"
  , "     * select is deasserted. The bytes not read back are discarded, and the"
  , "     * transaction is then reported as [`SPIError::Incomplete`]; as slave,"
  , "     * that includes the bytes the master did not shift."
  , "     *"
  , "     * @param[out] status Error seen during the transaction, if any."
  , "     * @error NotStarted No transaction is in course."
  , "     * @error ModeFault As master, another master drove the bus."
  , "     * @error Overrun The receiver lost bytes."
  , "     * @error Underrun As slave, the master shifted more bytes than were"
  , "     *        written."
  , "     * @error Incomplete Bytes were written and not read back."
  , "     " ]

-- | The summary, the body and the tags of the model of a block comment, given
-- its text between the delimiters, line by line.
model :: [String] -> Maybe ([DocInline], [DocBlock], [(TagKind, [DocBlock])])
model ls =
  case docModel (DocComment DocBefore BlockDoc (intercalate "\n" ls) (Position "test" (newPos "test" 1 1) (newPos "test" 1 1))) of
    Right m -> Just (docSummary m, docBody m, [ (tagKind t, tagText t) | t <- docTags m ])
    Left _ -> Nothing

-- | The body of a block comment whose first line is a summary.
bodyOf :: [String] -> Maybe [DocBlock]
bodyOf ls = (\(_, body, _) -> body) <$> model ("" : " * Summary." : " *" : map (" * " ++) ls ++ [" "])

spec :: Spec
spec = describe "Documentation model" $ do

  describe "the Markdown" $ do
    it "builds the model of the comment of ISPIBus::end" $
      model spiEnd `shouldBe` Just
        ( [DText "Ends the transaction and releases the bus."]
        , [ DPara [ DText "As master, the bytes still being shifted are waited for and the chip", DSoftBreak
                  , DText "select is deasserted. The bytes not read back are discarded, and the", DSoftBreak
                  , DText "transaction is then reported as ", DLink (DocRef ["SPIError", "Incomplete"])
                  , DText "; as slave,", DSoftBreak
                  , DText "that includes the bytes the master did not shift." ] ]
        , [ (TagParam ParamOut "status", [DPara [DText "Error seen during the transaction, if any."]])
          , (TagError "NotStarted", [DPara [DText "No transaction is in course."]])
          , (TagError "ModeFault", [DPara [DText "As master, another master drove the bus."]])
          , (TagError "Overrun", [DPara [DText "The receiver lost bytes."]])
          , (TagError "Underrun", [DPara [DText "As slave, the master shifted more bytes than were", DSoftBreak, DText "written."]])
          , (TagError "Incomplete", [DPara [DText "Bytes were written and not read back."]]) ] )
    it "links to what belongs to the language" $
      bodyOf ["Or [`Option::None`]."] `shouldBe` Just [DPara [DText "Or ", DLink (DocRef ["Option", "None"]), DText "."]]
    it "keeps a code span in brackets that is not a name as text" $
      bodyOf ["The sum [`a + b`]."] `shouldBe` Just [DPara [DText "The sum [", DCode "a + b", DText "]."]]
    it "keeps a bracketed word without backticks as text" $
      bodyOf ["An [unprotected] port."] `shouldBe` Just [DPara [DText "An [unprotected] port."]]
    it "turns an escaped @ into @" $
      bodyOf ["\\@ starts a tag."] `shouldBe` Just [DPara [DText "@ starts a tag."]]
    it "keeps raw HTML as text" $
      bodyOf ["A <b>bold</b> word."] `shouldBe` Just [DPara [DText "A <b>bold</b> word."]]
    it "reads math and tables" $
      bodyOf ["Takes $x^2$.", "", "| A | B |", "|:--|--:|", "| 1 | 2 |"] `shouldBe` Just
        [ DPara [DText "Takes ", DMath "x^2", DText "."]
        , DTable [DAlignLeft, DAlignRight] [[DText "A"], [DText "B"]] [[[DText "1"], [DText "2"]]] ]
    it "keeps the language and the text of a code block" $
      bodyOf ["```termina", "let x : u32 = 0;", "```"] `shouldBe` Just [DCodeBlock "termina" "let x : u32 = 0;\n"]
    it "has no summary when the comment starts with something else than a paragraph" $
      model ["", " * - one", " * - two", " "] `shouldBe` Just ([], [DList DBullets [[DPara [DText "one"]], [DPara [DText "two"]]]], [])

  describe "the margin" $ do
    it "removes the stars of a block comment and the blank lines at its ends" $
      blockLines spiEnd `shouldBe`
        [ "Ends the transaction and releases the bus."
        , ""
        , "As master, the bytes still being shifted are waited for and the chip"
        , "select is deasserted. The bytes not read back are discarded, and the"
        , "transaction is then reported as [`SPIError::Incomplete`]; as slave,"
        , "that includes the bytes the master did not shift."
        , ""
        , "@param[out] status Error seen during the transaction, if any."
        , "@error NotStarted No transaction is in course."
        , "@error ModeFault As master, another master drove the bus."
        , "@error Overrun The receiver lost bytes."
        , "@error Underrun As slave, the master shifted more bytes than were"
        , "       written."
        , "@error Incomplete Bytes were written and not read back." ]
    it "removes the common indentation of a block comment without stars" $
      blockLines [" Summary.", "    More text,", "      indented.", ""] `shouldBe` ["Summary.", "More text,", "  indented."]
    it "trims a block comment of one line" $
      blockLines [" A transaction is already in course. "] `shouldBe` ["A transaction is already in course."]
    it "keeps a star that is not the margin" $
      blockLines ["", " * *emphasis*", " * text", " "] `shouldBe` ["*emphasis*", "text"]
    it "removes the common indentation of a run of line comments" $
      lineLines [" Summary.", "", "   indented."] `shouldBe` ["Summary.", "", "  indented."]

  describe "the split into body and tags" $ do
    it "splits the comment of ISPIBus::end" $
      split spiEnd `shouldBe` Right
        ( [ "Ends the transaction and releases the bus."
          , ""
          , "As master, the bytes still being shifted are waited for and the chip"
          , "select is deasserted. The bytes not read back are discarded, and the"
          , "transaction is then reported as [`SPIError::Incomplete`]; as slave,"
          , "that includes the bytes the master did not shift." ]
        , [ (TagParam ParamOut "status", ["Error seen during the transaction, if any."])
          , (TagError "NotStarted", ["No transaction is in course."])
          , (TagError "ModeFault", ["As master, another master drove the bus."])
          , (TagError "Overrun", ["The receiver lost bytes."])
          , (TagError "Underrun", ["As slave, the master shifted more bytes than were", "written."])
          , (TagError "Incomplete", ["Bytes were written and not read back."]) ] )
    it "keeps the first word of a tag without arguments in its text" $
      split [" @return Nothing."] `shouldBe` Right ([], [(TagReturn, ["Nothing."])])
    it "takes the argument of the tags that have one" $
      split [ "", " * @param device Index.", " * @error SPIError::Busy Busy."
            , " * @satisfies REQ-SPI-1 The first.", " * @verifies REQ-SPI-2", " " ] `shouldBe` Right
        ( [], [ (TagParam ParamPlain "device", ["Index."]), (TagError "SPIError::Busy", ["Busy."])
              , (TagSatisfies "REQ-SPI-1", ["The first."]), (TagVerifies "REQ-SPI-2", []) ] )
    it "continues a tag after a blank line on indented lines, without their indentation" $
      split ["", " * @note First paragraph", " * of the note.", " *", " *     Second paragraph.", " *", " *     - an item", " "]
        `shouldBe` Right ([], [(TagNote, ["First paragraph", "of the note.", "", "Second paragraph.", "", "- an item"])])
    it "drops the blank lines between two tags" $
      split ["", " * @pre One.", " *", " * @post Two.", " "] `shouldBe` Right ([], [(TagPre, ["One."]), (TagPost, ["Two."])])
    it "does not open the tags inside a code block" $
      split ["", " * Summary.", " *", " * ```", " * @brief inside", " * ```", " "]
        `shouldBe` Right (["Summary.", "", "```", "@brief inside", "```"], [])
    it "does not open the tags at an escaped @" $
      split ["", " * Summary.", " * \\@brief is not a tag.", " "] `shouldBe` Right (["Summary.", "\\@brief is not a tag."], [])
    it "does not end a tag inside a code block" $
      split ["", " * @note Example:", " *", " *     ```", " *     @param x", " *     ```", " "]
        `shouldBe` Right ([], [(TagNote, ["Example:", "", "```", "@param x", "```"])])

  describe "the errors of the tags" $ do
    it "rejects an unknown tag" $
      tagError [" @brief Summary."] `shouldBe` Just (UnknownTag "brief")
    it "rejects a direction other than [out]" $
      tagError [" @param[in] x Input."] `shouldBe` Just (UnknownTag "param[in]")
    it "rejects [out] on another tag" $
      tagError [" @return[out] Value."] `shouldBe` Just (OutOnOtherTag "return")
    it "rejects @param without a name" $
      tagError [" @param"] `shouldBe` Just (MissingArgument "param")
    it "rejects @error with something that is not a variant" $
      tagError [" @error 3 Three."] `shouldBe` Just (MissingArgument "error")
    it "rejects @see without a link" $
      tagError [" @see "] `shouldBe` Just (MissingArgument "see")
    it "rejects text after the tags" $
      tagError ["", " * @pre One.", " *", " * Loose text.", " "] `shouldBe` Just TextAfterTags
