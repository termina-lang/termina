{-# LANGUAGE OverloadedStrings #-}
-- | The documentation comments of a module in its C files, as
-- @termina deliver --docs@ writes them.
module Codegen.Positive.DocCommentsSpec (spec) where

import qualified Data.Text as T

import Pipeline.Common (renderDocumentedFiles)

import Test.Hspec

-- | A struct with its fields, and a function with its parameters, one of
-- them only written.
pointAndDistance :: String
pointAndDistance = unlines
  [ "/**"
  , " * A point of the screen."
  , " */"
  , "struct Point {"
  , "    x : u32; /**< Abscissa, in pixels. */"
  , "    y : u32; /**< Ordinate, in pixels. */"
  , "};"
  , ""
  , "/**"
  , " * Distance between two points along the axes."
  , " *"
  , " * @param a First point."
  , " * @param b Second point."
  , " * @param[out] d The distance, in pixels."
  , " */"
  , "function manhattan(a : Point, b : Point, d : &mut u32) {"
  , "    *d = a.x + b.x + a.y + b.y;"
  , "    return;"
  , "}" ]

-- | A module with a comment of its own on each kind of element whose comment
-- reaches the C, each one a different text that names the element. The
-- elements whose comment does not reach the C yet are left without one: the
-- interfaces and their procedures (step 3d), the sink port of a handler and
-- the statements (step 3f).
everyElement :: String
everyElement = unlines
  [ "/** Doc const. */"
  , "const limit : u32 = 9;"
  , ""
  , "/** Doc struct. */"
  , "struct Point {"
  , "    x : u32; /**< Doc struct field. */"
  , "};"
  , ""
  , "/** Doc enum. */"
  , "enum Mode {"
  , "    Off, /**< Doc variant. */"
  , "    On(u32) /**< Doc variant with data. */"
  , "};"
  , ""
  , "interface ICounter {"
  , "    procedure add(&mut self, by : u32);"
  , "};"
  , ""
  , "/** Doc resource class. */"
  , "resource class CCounter provides ICounter {"
  , "    count : u32; /**< Doc resource field. */"
  , ""
  , "    /** Doc procedure. */"
  , "    procedure add(&mut self, by : u32) {"
  , "        self->count = self->clamp(self->count + by);"
  , "        return;"
  , "    }"
  , ""
  , "    /** Doc method. */"
  , "    method clamp(&priv self, v : u32) -> u32 {"
  , "        var r : u32 = v;"
  , "        if (r > limit) {"
  , "            r = self->count;"
  , "        }"
  , "        return r;"
  , "    }"
  , "};"
  , ""
  , "/** Doc task class. */"
  , "task class CWorker {"
  , "    step : u32; /**< Doc task field. */"
  , "    counter : access ICounter; /**< Doc access port. */"
  , "    timer : sink TimeVal triggers tick; /**< Doc sink port. */"
  , ""
  , "    /** Doc viewer. */"
  , "    viewer doubled(&self, b : u32) -> u32 {"
  , "        return b + self->step;"
  , "    }"
  , ""
  , "    /** Doc action. */"
  , "    action tick(&priv self, _t : TimeVal) {"
  , "        self->counter.add(self->doubled(1));"
  , "        return;"
  , "    }"
  , "};"
  , ""
  , "/** Doc function. */"
  , "function twice(v : u32) -> u32 {"
  , "    return v + v;"
  , "}"
  , ""
  , "/** Doc extern function. */"
  , "extern function now() -> u32;" ]

-- | The texts of the comments of 'everyElement'.
everyComment :: [T.Text]
everyComment =
  [ "Doc const.", "Doc struct.", "Doc struct field.", "Doc enum.", "Doc variant."
  , "Doc variant with data.", "Doc resource class.", "Doc resource field.", "Doc procedure."
  , "Doc method.", "Doc viewer.", "Doc task class.", "Doc task field.", "Doc access port.", "Doc sink port."
  , "Doc action.", "Doc function.", "Doc extern function." ]

spec :: Spec
spec = describe "Documentation comments in the C of a module" $ do
  it "reach the C once for every kind of element that has a C declaration" $
    case renderDocumentedFiles "test" [("test", everyElement)] of
      Left err -> expectationFailure (T.unpack err)
      Right (header, source) ->
        [ (c, n) | c <- everyComment, let n = T.count c (header <> source), n /= 1 ] `shouldBe` []
  it "document a struct, its fields, a function and its parameters in the header" $
    fmap fst (renderDocumentedFiles "test" [("test", pointAndDistance)]) `shouldBe` Right (T.unlines
      [ "#ifndef TEST_H__"
      , "#define TEST_H__"
      , ""
      , "#include <termina.h>"
      , ""
      , "/**"
      , " * @brief A point of the screen."
      , " */"
      , "typedef struct {"
      , "    uint32_t x; /**< Abscissa, in pixels. */"
      , "    uint32_t y; /**< Ordinate, in pixels. */"
      , "} Point;"
      , ""
      , "/**"
      , " * @brief Distance between two points along the axes."
      , " *"
      , " * @param[in] a First point."
      , " * @param[in] b Second point."
      , " * @param[out] d The distance, in pixels."
      , " */"
      , "void manhattan(const Point a, const Point b, uint32_t * const d);"
      , ""
      , "#endif" ])
