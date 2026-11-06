-- | The SMT prover of the run-time checks: the obligations of each module that
-- Z3 shows to hold, with the evidence named "smt".
module Analysis.SMTBounds (
    smtProjectProofs, smtModuleProofs, noSMTProofs
) where

import Command.Types (BasicBlocksData(..), LoweredProject, ProjectProofs)
import Configuration.Platform (Platform)
import Elaboration.Obligations (Proofs)
import Lowering.AST (AnnotatedProgram)
import Modules.Modules (TerminaModuleData(..))
import Semantic.Types (SemanticAnn)

import qualified Data.Map.Strict as M

-- | The obligations of the modules of a project that the SMT prover shows.
smtProjectProofs :: Platform -> LoweredProject -> IO ProjectProofs
smtProjectProofs plt = traverse (smtModuleProofs plt . loweredAST . metadata)

-- | The obligations of a module that the SMT prover shows. It shows none yet.
smtModuleProofs :: Platform -> AnnotatedProgram SemanticAnn -> IO Proofs
smtModuleProofs _ _ = return M.empty

-- | No proof for any module, for the language server and the tests.
noSMTProofs :: Applicative m => LoweredProject -> m ProjectProofs
noSMTProofs = pure . M.map (const M.empty)
