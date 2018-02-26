{-# Language OverloadedStrings #-}
module FillStroke
  ( -- * Fill
    fill
  , fillRule, nonzero, evenodd, FillRule
  , fillOpacity

    -- * Stroke
  , stroke
  , strokeWidth
  , strokeLineCap, buttCap, squareCap, roundCap, StrokeLineCap
  , strokeLineJoin, miterJoin, bevelJoin, roundJoin, StrokeLineJoin
  , strokeMiterLimit
  , strokeDashArray
  , strokeDashOffset
  , strokeOpacity
  )
  where

import Data.Text(Text)
import qualified Graphics.Svg as Svg

import Core
import Color
import Length

fill :: Color -> Svg.Attribute
fill = attr Svg.Fill_

stroke :: Color -> Svg.Attribute
stroke = attr Svg.Stroke_

newtype FillRule = FillRule Text

nonzero :: FillRule
nonzero = FillRule "nonzero"

evenodd :: FillRule
evenodd = FillRule "evenodd"

instance AttrText FillRule where
  attrText (FillRule x) = x

fillRule :: FillRule -> Svg.Attribute
fillRule = attr Svg.Fill_rule_

fillOpacity :: Float -> Svg.Attribute
fillOpacity = attr Svg.Fill_opacity_

strokeWidth :: Length -> Svg.Attribute
strokeWidth = attr Svg.Stroke_width_

newtype StrokeLineCap = StrokeLineCap Text

buttCap :: StrokeLineCap
buttCap = StrokeLineCap "butt"

squareCap :: StrokeLineCap
squareCap = StrokeLineCap "square"

roundCap :: StrokeLineCap
roundCap = StrokeLineCap "round"

instance AttrText StrokeLineCap where
  attrText (StrokeLineCap x) = x

strokeLineCap :: StrokeLineCap -> Svg.Attribute
strokeLineCap = attr Svg.Stroke_linecap_

newtype StrokeLineJoin = StrokeLineJoin Text

miterJoin :: StrokeLineJoin
miterJoin = StrokeLineJoin "miter"

bevelJoin :: StrokeLineJoin
bevelJoin = StrokeLineJoin "bevel"

roundJoin :: StrokeLineJoin
roundJoin = StrokeLineJoin "round"

instance AttrText StrokeLineJoin where
  attrText (StrokeLineJoin x) = x

strokeLineJoin :: StrokeLineJoin -> Svg.Attribute
strokeLineJoin = attr Svg.Stroke_linejoin_

strokeMiterLimit :: Float -> Svg.Attribute
strokeMiterLimit = attr Svg.Stroke_miterlimit_

strokeDashArray :: [Length] -> Svg.Attribute
strokeDashArray xs = Svg.Stroke_dasharray_ Svg.<<- commaSep xs

strokeDashOffset :: Length -> Svg.Attribute
strokeDashOffset = attr Svg.Stroke_dashoffset_

strokeOpacity :: Float -> Svg.Attribute
strokeOpacity = attr Svg.Stroke_opacity_



