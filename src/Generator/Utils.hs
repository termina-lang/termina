module Generator.Utils where

import Prettyprinter.Render.Terminal
import Data.Text (Text)
import Prettyprinter

type DocStyle = Doc AnsiStyle

render :: DocStyle -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

unboundedRender :: DocStyle -> Text
unboundedRender = renderStrict . layoutSmart (LayoutOptions Unbounded)