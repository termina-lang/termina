-- | Exit-path positive tests: well-formed functions and actions (every path
-- reaches an exit point) raise no error.
module ExitPaths.Positive.ExitSpec (spec) where

import ExitPaths.Common (exitPathsError)

import Data.Maybe (isNothing)
import Test.Hspec

-- | A function ending in a return (functions must end with a return statement,
-- even when a preceding if already covers every path).
goodFunction :: String
goodFunction =
  "function f(x : u32) -> u32 {\n" ++
  "    var r : u32 = x;\n" ++
  "    return r;\n" ++
  "}\n"

-- | A task whose action is a single if/else where both branches exit (continue
-- in one, return in the other).
goodAction :: String
goodAction =
  "task class foo {\n" ++
  "    snk : sink u32 triggers action0;\n" ++
  "    action action1(&priv self, data : u32) -> u32 {\n" ++
  "        var st : u32 = 0 : u32;\n" ++
  "        return st;\n" ++
  "    }\n" ++
  "    action action0(&priv self, data : u32) -> u32 {\n" ++
  "        if (data == 0 : u32) {\n" ++
  "            continue self->action1(data);\n" ++
  "        } else {\n" ++
  "            var st : u32 = data;\n" ++
  "            return st;\n" ++
  "        }\n" ++
  "    }\n" ++
  "};\n"

-- | A function that matches every case and then returns.
matchFunction :: String
matchFunction =
  "function f(s : Status<i32>) -> u32 {\n" ++
  "    var r : u32 = 0 : u32;\n" ++
  "    match s {\n" ++
  "        case Success => { r = 1 : u32; }\n" ++
  "        case Failure(_e) => { r = 2 : u32; }\n" ++
  "    }\n" ++
  "    return r;\n" ++
  "}\n"

-- | A function with a bounded for loop followed by a return.
loopFunction :: String
loopFunction =
  "function f(n : usize) -> u32 {\n" ++
  "    var acc : u32 = 0 : u32;\n" ++
  "    for i : usize in 0 : usize .. 10 : usize while (i < n) {\n" ++
  "        acc = acc + 1 : u32;\n" ++
  "    }\n" ++
  "    return acc;\n" ++
  "}\n"

rebootBranchAction :: String
rebootBranchAction =
  "task class foo {\n" ++
  "    snk : sink usize triggers action0;\n" ++
  "    action action0(&priv self, entry : usize) -> Status<i32> {\n" ++
  "        var st : Status<i32> = Success;\n" ++
  "        if (entry != 0) {\n" ++
  "            reboot;\n" ++
  "        }\n" ++
  "        return st;\n" ++
  "    }\n" ++
  "};\n"

continueBranchAction :: String
continueBranchAction =
  "task class foo {\n" ++
  "    snk : sink u32 triggers action0;\n" ++
  "    action action1(&priv self, data : u32) -> Status<i32> {\n" ++
  "        var st : Status<i32> = Success;\n" ++
  "        return st;\n" ++
  "    }\n" ++
  "    action action0(&priv self, data : u32) -> Status<i32> {\n" ++
  "        var st : Status<i32> = Success;\n" ++
  "        if (data == 0 : u32) {\n" ++
  "            continue self->action1(data);\n" ++
  "        }\n" ++
  "        return st;\n" ++
  "    }\n" ++
  "};\n"

spec :: Spec
spec = describe "Exit paths: well-formed paths raise no error" $
  mapM_ (\(name, src) -> it name $ exitPathsError src `shouldSatisfy` isNothing)
    [ ("accepts a function returning on every branch", goodFunction)
    , ("accepts an action that exits on every path", goodAction)
    , ("accepts a function that matches then returns", matchFunction)
    , ("accepts a function with a for loop then a return", loopFunction)
    , ("accepts an action with a reboot in a branch", rebootBranchAction)
    , ("accepts an action with a continue in a branch", continueBranchAction)
    ]
