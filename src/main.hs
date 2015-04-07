module Main where

import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy.IO as T
import qualified IOL.Y2014.Indiv.P2 as Problem

main = T.putStr $ renderHtml Problem.document
