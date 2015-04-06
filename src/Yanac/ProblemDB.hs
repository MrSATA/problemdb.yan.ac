module Yanac.ProblemDB where

import Data.Monoid

import Data.String

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!), Html)
import           Text.Blaze.Internal ()

data Metadata = Metadata
  { title   :: String
  , authors :: [String]
  , tags    :: [String]
  }
  deriving (Show)

newtype Language = Language String
  deriving (Show)
mkSimpleLang :: String -> String -> Language
mkSimpleLang lang script = Language $ lang <> "-" <> script

section id = H.section ! A.id (fromString id)
par  = H.p

list = H.ul
item = H.li
listify :: [H.Html] -> H.Html
listify items = sequence_ items'
  where items' = map (\x -> item x) items

dgroup term value = H.dl $ term <> value
dterm  = H.dt
dvalue = H.dd

mention = H.i `of_` "mention"

text :: String -> Html
text   = fromString

countOn frag counter = frag ! A.class_ (fromString counterName)
  where counterName = "counter-" <> counter
of_ frag className = frag ! A.class_ className
in_ frag (Language langValue) = frag ! A.lang (fromString langValue)

data CellContent = CellLit String
                 | CellNA
                 | ToFill

instance IsString CellContent where
  fromString str = CellLit str
