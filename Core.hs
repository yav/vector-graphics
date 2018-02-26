{-# Language OverloadedStrings #-}
module Core where

import Data.Text(Text, pack, intercalate)

import Graphics.Svg

class AttrText a where
  attrText :: a -> Text

instance AttrText Float where
  attrText = pack . show

instance (AttrText a, AttrText b) => AttrText (a,b) where
  attrText (a,b) = attrText a <> " " <> attrText b

attr :: AttrText a => AttrTag -> a -> Attribute
attr x y = bindAttr x (attrText y)

commaSep :: AttrText a => [a] -> Text
commaSep = intercalate "," . map attrText


