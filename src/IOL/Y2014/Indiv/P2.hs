module IOL.Y2014.Indiv.P2 where

import Data.Monoid
import Data.String (fromString)

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Internal ()

import Yanac.ProblemDB

metadata :: Metadata
metadata = Metadata
  { title   = "基奥瓦语"
  , authors = ["阿列克赛·佩古谢夫"]
  , tags    = ["语音题", "小坑"]
  }

kio :: Language
kio = mkSimpleLang "kio" "Latn"

document = do
  section "materials" $ do
    par $  "以下为一些基奥瓦语名词的单数，双数和复数形式及其汉语翻译。"
        <> "表中名词均有三种形式，但没有全部列出。"
    H.table $ do
      H.thead $ H.tr $ H.th "单数" <> H.th "双数" <> H.th "复数" <> H.th "翻译"
      H.tbody $ mapM_ renderRow table

  section "assignment" $ do
    par $ "在带问号的空格内填上词的相应形式。"

  section "note" $ do
    par "基奥瓦语属于基奥瓦－塔诺安语系。这是一种濒危语言，只有美国俄克拉荷马州的数百人仍在使用。"
    let alt = H.i `in_` kio
    par $  "以上给出的基奥瓦语的词采用简化转写法。"
        <> alt "k’, t’, p’, kh, ph, th"
        <> "是辅音；"
        <> alt "ɔ"
        <> "是一个元音。"

renderRow (Row single dual plural cmn) = H.tr $ do
  mapM_ renderKiowa [single, dual, plural]
  H.td $ fromString cmn
  where
    renderKiowa CellNA = H.td `of_` "not-applicable" $ ""
    renderKiowa ToFill = H.td `in_` kio $ ""
    renderKiowa (CellLit x) = H.td `in_` kio $ fromString x

data Row = Row
  { single :: CellContent
  , dual   :: CellContent
  , plural :: CellContent
  , cmn    :: String
  }

table =
  [ Row "adɔ"        "a"          "a"           "树"
  , Row "matʰɔnsjan" "matʰɔnsjan" "matʰɔnsjadɔ" "小女孩"
  , Row "k’ɔ"        "k’ɔ"        "k’ɔgɔ"       "刀"
  , Row "tʰot’olagɔ" "tʰot’ola"   "tʰot’olagɔ"  "橙子"
  , Row "aufi"       CellNA       "aufigɔ"      "鱼"
  , Row "pʰjaboadɔ"  CellNA       "pʰjaboa"     "路灯"
  , Row "matʰɔn"     CellNA       "matʰɔdɔ"     "姑娘"
  , Row "k’ɔnbohodɔ" CellNA       "k’ɔnbohon"   "帽子"
  , Row "t’ɔ"        CellNA       "t’ɔgɔ"       "勺子"
  , Row CellNA       CellNA       "e"           "面包"
  , Row "alɔsɔhjegɔ" ToFill       "alɔsɔhjegɔ"  "李子"
  , Row ToFill       "tsegun"     "tsegudɔ"     "狗"
  , Row "alɔguk’ogɔ" "alɔguk’o"   ToFill        "柠檬"
  , Row ToFill       "k’apʰtʰɔ"   "k’apʰtʰɔgɔ"  "老汉"
  , Row "kʰɔdɔ"      "kʰɔ"        ToFill        "毯子"
  , Row "k’ɔdɔ"      ToFill       "k’ɔdɔ"       "西红柿"
  , Row ToFill       "alɔ"        ToFill        "苹果"
  , Row ToFill       "pʰɔ"        ToFill        "野牛"
  , Row ToFill       ToFill       "sadɔ"        "儿童"
  , Row "ɔlsun"      ToFill       ToFill        "梳子"
  , Row ToFill       "pitso"      ToFill        "叉子"
  , Row ToFill       "tʰɔpʰpaa"   ToFill        "椅子"
  ]
