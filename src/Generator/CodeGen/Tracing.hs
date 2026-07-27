-- | Tracing labels.
--
-- Under the @tracing@ profile the generator marks the entry and every exit of
-- each generated function with an assembly label, emitted through the GCC
-- @__asm__ __volatile__@ extension:
--
-- > __asm__ __volatile__("termina__CTMChannel__send_tm__entry:\n");
-- > ...
-- > __asm__ __volatile__("termina__CTMChannel__send_tm__exit__0:\n");
-- > return;
--
-- The labels end up as symbols in the object file, so the address of the entry
-- and of each exit of an action, a procedure, a method, a viewer or a function
-- can be read from the ELF and used to time the code on the target.
--
-- The body of a label is the name of the generated C function, which is already
-- unique across the program (@Class__member@ for class members, the function
-- name for free functions), so a label maps back to its function symbol without
-- any intermediate table. Exits are numbered from zero in generation order, and
-- always numbered, so that a single pattern matches every label and the name of
-- an exit does not change when another one is added to the function. Actions are
-- the only members that can have more than one exit; the exit-path check forces
-- the rest to have exactly one. A @continue@ is an exit as well, since it
-- returns from the action; its label precedes the call to the chained action,
-- so the interval it closes leaves that action out, and the chained action is
-- measured through its own labels. A @reboot@ gets no label, since it does not
-- return.
--
-- The task loop the generator writes for each task class is not labeled: it is
-- the dispatcher, not a member the developer wrote.
--
-- These labels are a GCC extension and are not MISRA compliant, which is why
-- they belong to a profile of their own and never appear in a @release@ build.
module Generator.CodeGen.Tracing (
    withTracingLabels,
    genTracingEntryLabel,
    genTracingExitLabel
  ) where

import Configuration.Configuration
import Generator.CodeGen.Common
import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Utils.Annotations
import qualified Control.Monad.State as ST

-- | Prefix shared by every tracing label.
tracingPrefix :: Ident
tracingPrefix = "termina"

-- | Generates the body of a function with its tracing labels enabled, provided
-- the tracing profile is selected. The argument is the name of the generated C
-- function, which is the prefix of its labels. Nesting is supported: the
-- previous state is restored on the way out.
withTracingLabels :: Ident -> CGenerator a -> CGenerator a
withTracingLabels cFunctionName genBody = do
    prof <- ST.gets (profile . configParams)
    case prof of
        Tracing -> do
            previous <- ST.gets tracedFunction
            ST.modify $ \env -> env { tracedFunction = Just (cFunctionName, 0) }
            cBody <- genBody
            ST.modify $ \env -> env { tracedFunction = previous }
            return cBody
        _ -> genBody

-- | Entry label of the function being generated, or nothing at all if tracing
-- is not enabled.
genTracingEntryLabel :: Location -> CGenerator [CCompoundBlockItem]
genTracingEntryLabel loc = ST.gets tracedFunction >>= \case
    Nothing -> return []
    Just (cFunctionName, _) ->
        return [pre_cr (_asm_label (tracingPrefix <::> cFunctionName <::> "entry")) |>> loc]

-- | Next exit label of the function being generated, or nothing at all if
-- tracing is not enabled. Consumes the index, so the labels of a function are
-- numbered in the order in which its exits are generated.
genTracingExitLabel :: Location -> CGenerator [CCompoundBlockItem]
genTracingExitLabel loc = ST.gets tracedFunction >>= \case
    Nothing -> return []
    Just (cFunctionName, index) -> do
        ST.modify $ \env -> env { tracedFunction = Just (cFunctionName, index + 1) }
        let label = tracingPrefix <::> cFunctionName <::> "exit" <::> show index
        return [pre_cr (_asm_label label) |>> loc]
