module Main where

import Text.Blaze.Html.Renderer.Text
import Yanac.ProblemDB

import qualified Data.Text.Lazy.IO as T
import qualified IOL.Y2014.Indiv.P4 as Problem

document = do
  section "materials"   Problem.materials
  section "assignments" Problem.assignments
  section "notes"       Problem.notes


main = T.putStr $ renderHtml document
