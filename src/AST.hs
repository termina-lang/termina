-- | Module where ASTs are defined.

module AST where


-- | Annotated AST
data AAST a
  = AST { glbs :: ([Global], a)
        , fs  :: ([Function], a)
        , ags :: ([Agent], a)
    }

type Global = String
type Function = String
type Agent = String

type AST = AAST ()
