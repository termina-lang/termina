module Utils.Printer where

import Prettyprinter.Render.Terminal
import qualified Data.Text as T
import Prettyprinter

type DocStyle = Doc AnsiStyle

render :: DocStyle -> T.Text
render = renderStrict . layoutSmart defaultLayoutOptions

unboundedRender :: DocStyle -> T.Text
unboundedRender = renderStrict . layoutSmart (LayoutOptions Unbounded)

class ShowText a where
    showText :: a -> T.Text