{-# Language OverloadedStrings #-}
import Graphics.Svg hiding (translate,skewX, skewY, rotate, rotateAround)
import Data.Text(Text)

import FillStroke
import Color
import Length
import Transform
import Shape

main :: IO ()
main = print (svg pic)

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content)
        [Version_ <<- "1.1", Width_ <<- "600" , Height_ <<- "300"]


pic :: Element
pic =
  g_ [ transform (translate 200 100)
     ] $
     mconcat
     [ polygon [ points (regPoly n r)
               , transform [ rotateAround (50,50) (fromIntegral i')
                           ]
               , fill (light (* (fromIntegral i' / 180)) red)
               ]
     | i' <- [ 0, 5 .. 360 ], let i = fromIntegral i'
     ]

  where
  r = 70
  n = 3
  h = rheight n r
  w = pi/2


regPoly :: Int -> Float -> [(Float,Float)]
regPoly n r = map snd (take n (iterate step (w0, (0, -r))))
  where
  w0     = pi / fromIntegral n
  w      = 2 * w0
  s      = side n r
  step (a,(x,y)) = (a + w, (x + s * cos a, y + s * sin a))



-- | Side of a regular polygon, given a radius.
side :: Int -> Float -> Float
side n r = 2 * r * sin (pi / fromIntegral n)

-- | Distance from center to edge for a regular polygon
-- with the given radius.
rheight :: Int -> Float -> Float
rheight n r = r * cos (pi / fromIntegral n)

