{-# Language OverloadedStrings #-}
import Graphics.Svg
import Graphics.Svg.Path
import Data.Text(Text)

main :: IO ()
main = print (svg pic)

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content)
        [Version_ <<- "1.1", Width_ <<- "600" , Height_ <<- "200"]

pic :: Element
pic = path
       [ Stroke_ <<- "red"
       , Fill_opacity_  <<- "0"
       ]
    $ Steps
        [ Move (10,80)
        , Quadratic (Just (95,10)) (180,80)
        , Quadratic Nothing (250,80)
        , Quadratic Nothing (350,80)
        , Quadratic (Just (170,190)) (10,80)
        ]

data Path =
    Move Point
  | Line Point
  | HLine Float
  | VLine Float
  | Cubic (Maybe Point) Point Point
  | Quadratic (Maybe Point) Point

  | Steps [ Path ]
  | Mode How Path


path xs p = path_ ( (D_ <<- draw p) : xs)

draw :: Path -> Text
draw = go Abs
  where
  go how pic =
    case pic of
      Move p -> move how p
      HLine x -> hline how x
      VLine x -> vline how x
      Line x  -> line how x
      Cubic x y z -> cubic how x y z
      Quadratic x y -> quadratic how x y

      Mode x p -> go x p
      Steps ps -> mconcat (map (go how) ps)





data How = Abs | Rel

type Point = (Float,Float)

move :: How -> Point -> Text
move h (x,y) = case h of
                 Abs -> mA x y
                 Rel -> mR x y


line :: How -> Point -> Text
line h (x,y) = case h of
                 Abs -> lA x y
                 Rel -> lR x y

hline :: How -> Float -> Text
hline h x = case h of
              Abs -> hA x
              Rel -> hR x


vline :: How -> Float -> Text
vline h y = case h of
              Abs -> vA y
              Rel -> vR y

cubic :: How -> Maybe Point -> Point -> Point -> Text
cubic how mb (d2x,d2y) (x,y) =
  case mb of
    Nothing -> smooth d2x d2y x y
    Just (d1x,d1y) -> curve d1x d1y d2x d2y x y
  where
  smooth = case how of
            Abs -> sA
            Rel -> sR
  curve = case how of
            Abs -> cA
            Rel -> cR


quadratic :: How -> Maybe Point -> Point -> Text
quadratic how mb (x,y) =
  case mb of
    Nothing -> smooth x y
    Just (dx,dy) -> curve dx dy x y
  where
  smooth = case how of
             Abs -> tA
             Rel -> tR
  curve = case how of
            Abs -> qA
            Rel -> qR


