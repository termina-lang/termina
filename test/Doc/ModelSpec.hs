-- | The documentation model of a comment: the margin, the split into body and
-- tags, and the errors of the tags.
module Doc.ModelSpec (spec) where

import Data.List (intercalate)
import Text.Parsec.Pos (initialPos, newPos)

import Core.AST (DocForm(..))
import Doc.Model

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

spec :: Spec
spec = describe "Documentation model" $ do

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
