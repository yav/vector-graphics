{-# Language OverloadedStrings #-}
module Color
  ( Color
  , rgb
  , red, white
  , gray
  , light
  ) where

import Data.Word(Word8)
import qualified Data.Text as Text
import Data.Monoid((<>))

import Core

data Color = RGB Word8 Word8 Word8
              deriving (Eq,Show)

instance AttrText Color where
  attrText (RGB r g b) = "rgb(" <> sh r <> "," <> sh g <> "," <> sh b <> ")"
    where sh = Text.pack . show

rgb :: (Word8,Word8,Word8) -> Color
rgb (r,g,b) = RGB r g b

gray :: Word8 -> Color
gray x = rgb (x,x,x)

red :: Color
red = RGB 255 0 0

green :: Color
green = RGB 0 255 0


white :: Color
white = RGB 255 255 255

--------------------------------------------------------------------------------

ciexyz_srgb :: (Double,Double,Double) -> Color
ciexyz_srgb (x,y,z) = RGB (gamma rlin) (gamma glin) (gamma blin)
  where
  v a b c = a * x + b * y + c * z
  rlin = v 3.2406 (-1.5372) (-0.4986)
  glin = v (-0.9689) 1.8758 0.0415
  blin = v 0.0557 (-0.2040) 1.0570

  gamma c
    | c <= 0.0031308 = toB (12.92 * c)
    | otherwise = toB ((1+a) * (c ** (1/2.4)) - a)

  a = 0.055


toB :: Double -> Word8
toB x' =
  let x = 255 * x'
  in if x > 255 then 255 else if x < 0 then 0 else round x


srgb_ciexyz :: Color -> (Double,Double,Double)
srgb_ciexyz (RGB r g b) = ( v 0.4124 0.3576 0.1805
                          , v 0.2126 0.7152 0.0722
                          , v 0.0193 0.1192 0.9505
                          )
  where
  rlin = lin r
  glin = lin g
  blin = lin b

  v x y z = x * rlin + y * glin + z * blin

  lin x'
    | x <= 0.04045 = x / 12.92
    | otherwise = ((x + a) / (1 + a)) ** 2.4
    where
    x = fromIntegral x' / 255

  a = 0.055

illuminant_D65 :: (Double,Double,Double)
illuminant_D65 = (0.95047, 1, 1.08883)

ciexyz_cielab :: (Double,Double,Double) -> (Double,Double,Double)
ciexyz_cielab (x,y,z) = (l,a,b)
  where
  l = 116 * fy - 16
  a = 500 * (fx - fy)
  b = 200 * (fy - fz)

  fx = f (x / xn)
  fy = f (y / yn)
  fz = f (z / zn)

  (xn,yn,zn) = illuminant_D65

  f t
    | t > delta * delta * delta = t ** (1/3)
    | otherwise                 = t / (3 * delta * delta) + 4/29



cielab_ciexyz :: (Double,Double,Double) -> (Double,Double,Double)
cielab_ciexyz (l,a,b) = ( xn * f' (l' + a/500)
                        , yn * f' l'
                        , zn * f' (l' - b/200)
                        )
  where
  l' = (l + 16) / 116
  (xn,yn,zn) = illuminant_D65

  f' t | t > delta = t * t * t
       | otherwise = 3 * delta * delta * (t - 4/29)

delta :: Double
delta = 6 / 29


srgb_lab :: Color -> (Double,Double,Double)
srgb_lab = ciexyz_cielab . srgb_ciexyz

lab_srgb :: (Double,Double,Double) -> Color
lab_srgb = ciexyz_srgb . cielab_ciexyz

light :: (Double -> Double) -> Color -> Color
light f c = let (l,a,b) = srgb_lab c
            in lab_srgb (f l, a, b)


