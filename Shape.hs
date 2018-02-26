module Shape where

import qualified Graphics.Svg as Svg

import Core
import Length


rectangle :: Svg.Term a => [Svg.Attribute] -> a
rectangle = Svg.rect_

topLeftX :: Length -> Svg.Attribute
topLeftX = attr Svg.X_

topLeftY :: Length -> Svg.Attribute
topLeftY = attr Svg.Y_

width :: Length -> Svg.Attribute
width = attr Svg.Width_

height :: Length -> Svg.Attribute
height = attr Svg.Height_


circle :: Svg.Term a => [Svg.Attribute] -> a
circle = Svg.circle_

centerX :: Length -> Svg.Attribute
centerX = attr Svg.Cx_

centerY :: Length -> Svg.Attribute
centerY = attr Svg.Cy_

radius :: Length -> Svg.Attribute
radius = attr Svg.R_


ellipse :: Svg.Term a => [Svg.Attribute] -> a
ellipse = Svg.ellipse_

radiusX :: Length -> Svg.Attribute
radiusX = attr Svg.Rx_

radiusY :: Length -> Svg.Attribute
radiusY = attr Svg.Ry_



line :: Svg.Term a => [Svg.Attribute] -> a
line = Svg.line_

fromX :: Length -> Svg.Attribute
fromX = attr Svg.X1_

fromY :: Length -> Svg.Attribute
fromY = attr Svg.Y1_

toX :: Length -> Svg.Attribute
toX = attr Svg.X2_

toY :: Length -> Svg.Attribute
toY = attr Svg.Y2_


polyline :: Svg.Term a => [Svg.Attribute] -> a
polyline = Svg.polyline_

polygon :: Svg.Term a => [Svg.Attribute] -> a
polygon  = Svg.polygon_

points :: [(Float,Float)] -> Svg.Attribute
points xs = Svg.Points_ Svg.<<- commaSep xs


