module IOL.Y2014.Indiv.P4
  ( metadata
  , materials
  , assignments
  , notes
  ) where

import Data.Monoid

import qualified Data.Text.Lazy.IO as T

import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Internal ()

import Yanac.ProblemDB

metadata :: Metadata
metadata = Metadata
  { title   = "恩盖尼语"
  , authors = ["阿尔图尔·谢梅纽克斯"]
  , tags    = ["罗塞塔石碑", "中级题"]
  }

enn :: Language
enn = mkSimpleLang "enn" "Latn"

materials = do
  par "以下为恩盖尼语的几段短篇对话及其汉语翻译："
  let dterm' = dterm `in_` enn
  let render x = dgroup (dterm' $ text (ennq x) <> H.wbr <> text (enna x))
                        (dvalue $ text (cmnq x) <> H.wbr <> text (cmna x))
  list `countOn` "rosseta" $ listify $ map render mainCorpus

assignments = list `countOn` "assignments" $ asg1 >> asg2 >> asg3

asg1 = item $ do
  par "翻译成汉语："
  let render x = text (ennq x) <> H.wbr <> text (enna x)
  list `countOn` "rosseta" $ listify $ map render toCmn

  let alt = H.i `in_` enn
  par $  "这里还有恩盖尼语的一个答句，但相应的问句没有给出："
      <> H.br
      <> alt "ozyi ânò wei ga ạmó gbunono edèì."
      <> H.br
      <> "翻译成汉语。如果译法不止一种，请全部写出，并解释你这样翻译的理由。"

asg2 = item $ do
  par "翻译成恩盖尼语："
  list `countOn` "rosseta" $ listify $ map render toEnn
  where
    render x = text (cmnq x) <> H.wbr <> text (cmna x)

asg3 = item $ do
  par $  "假设你要编写一部恩盖尼词典，那么表示"
      <> mention "小偷" <> "和" <> mention "姑娘"
      <> "的词的基本形式分别是什么？解释你的答案。"

notes = do
  par "恩盖尼语属于贝努埃－刚果语系。在尼日利亚，大约两万人使用该语言。"
  let alt = H.i `in_` enn
  par $  "一个词的首个元音下面的标记"
      <> alt "◌̣"
      <> "表明该词的所有元音发音时舌位都要稍微降低。"
      <> "标记"
      <> alt "◌́, ◌̀, ◌̂"
      <> "分别表示高、低、降调；如果以上标记均不出现，这个音节就发中调。"

data Sentence = Sentence
  { ennq :: String
  , enna :: String
  , cmnq :: String
  , cmna :: String
  }

mainCorpus =
  [ Sentence
      "edèì âno nwạ́sesè ozyí lẹlemù à?"
      "edèì ânò wei ga òkí nwạsese ozyí lẹlemù."
      "这一个男人会(将来时)吓到[这]受骗的小偷吗？"
      "这一个男人说他{sub: 这一个男人}不会(将来时)吓到[这]受骗的小偷。"
  , Sentence
      "ạvùràmù kịnono amemùrè ânò à?"
      "ạvùràmù wei ga òkì kịnono amemùrè ânò."
      "[这]女人长(过去时)得像这一个姑娘吗？"
      "[这]女人说她{sub: [这]女人}长(过去时)得像这一个姑娘。"
  , Sentence
      "ạmó lẹlemù ậnó wuese ạvùràmù à?"
      "ạmodhyòmù wei ga ò wuese ạvùràmù."
      "这一个受骗的儿童没有杀[这]女人吗？"
      "[这]青年说他{sub: 这一个受骗的儿童}杀了[这]女人。"
  , Sentence
      "edèí dhia gbúnonò ạmò à?"
      "ạvùràmú kofilomù wei ga o gbúnonò ạmò."
      "[这]坏男人会(将来时)治好[这]儿童吗？"
      "[这]咳嗽的女人说他{sub: [这]坏男人}会(将来时)治好[这]儿童。"
  , Sentence
      "amemùré dhiá kịnono opilopo ânò à?"
      "ạvùràmù wei ga ọ́ kịnono opilopo ânò."
      "[这]坏姑娘长(过去时)得不像这一头猪吗？"
      "[这]女人说她{sub: [这]坏姑娘}长(过去时)得不像这一头猪。"
  , Sentence
      "ozyì gbunono okàá nụamù ậnò à?"
      "ozyì wei ga òkí gbunono okàá nụamù ậnò."
      "[这]小偷治好了这一个挨了打的老汉吗？"
      "[这]小偷说他{sub: [这]小偷}没有治好这一个挨了打的老汉。"
  , Sentence
      "ozyi âno kị́nonò edèí kofilomù à?"
      "ạmò ậnò wei ga ọ́ kịnono edèí kofilomù."
      "这一个小偷会(将来时)长得像[这]咳嗽的男人吗？"
      "这一个儿童说他{sub: 这一个小偷}不会(将来时)长得像[这]咳嗽的男人。"
  ]

toCmn =
  [ Sentence
      "edèì ânò nwạsese ozyi à?"
      "amemùrè wei ga ọ̀ nwạsese ozyi."
      "这一个男人吓到了[这]小偷吗？"
      "[这]姑娘说他{sub: 这一个男人}吓到了[这]小偷。"
  , Sentence
      "amemùré lẹlemu dhúnenè ạmodhyòmù ậnò à?"
      "amemùré lẹlemu wei ga òki dhúnenè ạmodhyòmù ậnò."
      "[这]受骗的姑娘会(将来时)杀这一个青年吗？"
      "[这]受骗的姑娘说她{sub: [这]受骗的姑娘}会(将来时)杀这一个青年。"
  ]

toEnn =
  [ Sentence
      "okàa kị́nonò ạmodhyòmú kofilomù ânò à?"
      "ạmò wei ga ọ́ kịnono ạmodhyòmú kofilomù ânò."
      "[这]老汉会(将来时)长得像这一个咳嗽的青年吗？"
      "[这]儿童说他{sub: [这]老汉}不会(将来时)长得像这一个咳嗽的青年。"
  , Sentence
      "ạvùràmú nụamù ậnó nwạsese edèì à?"
      "ạvùràmú nụamù ậnò wei ga òkí nwạsese edèì."
      "这一个挨了打的女人没有吓到[这]男人吗？"
      "这一个挨了打的女人说她{sub: 这一个挨了打的女人}没有吓到[这]男人。"
  ]
