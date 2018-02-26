{-# Language OverloadedStrings #-}
{-# Language TypeFamilies #-}
module Transform
  ( Transform
  , IsTransform(..)
  , matrix
  , translate, translateX
  , rotate, rotateAround
  , scale, scaleXY
  , skewX
  , skewY
  ) where

import Data.Maybe(maybeToList)
import Data.Monoid((<>))
import qualified Graphics.Svg as Svg

import Core

data Transform = Matrix Float Float Float Float Float Float
               | Translate Float (Maybe Float)
               | Scale Float (Maybe Float)
               | Rotate Float (Maybe (Float,Float))
               | SkewX Float
               | SkewY Float

instance AttrText Transform where
  attrText x =
    case x of
      Matrix a b c d e f -> fun "matrix" [a,b,c,d,e,f]
      Translate x y -> fun "translate" (x : maybeToList y)
      Scale x y -> fun "scale" (x : maybeToList y)
      Rotate x y -> fun "rotate" (x : maybe [] (\(a,b) -> [a,b]) y)
      SkewX x -> fun "skewX" [x]
      SkewY x -> fun "skewY" [x]
    where
    fun f xs = f <> "(" <> commaSep xs <> ")"

class IsTransform a where
  transform :: a -> Svg.Attribute

instance IsTransform Transform where
  transform a = transform [a]

instance (a ~ Transform) => IsTransform [a] where
  transform xs = Svg.Transform_ Svg.<<- commaSep xs

matrix :: (Float,Float,Float,Float,Float,Float) -> Transform
matrix (a,b,c,d,e,f) = Matrix a b c d e f

translate :: Float -> Float -> Transform
translate x y = Translate x (Just y)

translateX :: Float -> Transform
translateX x = Translate x Nothing

scale :: Float -> Transform
scale x = Scale x Nothing

scaleXY :: Float -> Float -> Transform
scaleXY x y = Scale x (Just y)

rotate :: Float -> Transform
rotate x = Rotate x Nothing

rotateAround :: (Float,Float) -> Float -> Transform
rotateAround pt x = Rotate x (Just pt)

skewX :: Float -> Transform
skewX = SkewX

skewY :: Float -> Transform
skewY = SkewY


