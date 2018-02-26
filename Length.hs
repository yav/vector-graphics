{-# Language OverloadedStrings #-}
module Length(Length, em, ex, px, inch, cm, mm, pt, pc, percent) where

import Data.Text(Text,unpack)
import Data.Monoid((<>))

import Core

data Length = Length Float (Maybe Text)
  deriving (Eq,Show)

instance AttrText Length where
  attrText (Length a mb) =
    case mb of
      Nothing -> attrText a
      Just u  -> attrText a <> u

withUnit :: Text -> Float -> Length
withUnit u f = Length f (Just u)

em :: Float -> Length
em = withUnit "em"

ex :: Float -> Length
ex = withUnit "ex"

px :: Float -> Length
px = withUnit "px"

inch :: Float -> Length
inch = withUnit "in"

cm :: Float -> Length
cm = withUnit "cm"

mm :: Float -> Length
mm = withUnit "mm"

pt :: Float -> Length
pt = withUnit "pt"

pc :: Float -> Length
pc = withUnit "pc"

percent :: Float -> Length
percent = withUnit "%"

instance Num Length where
  fromInteger x = Length (fromInteger x) Nothing
  (+) = op2 "+" (+)
  (*) = op2 "*" (*)
  abs = op1 abs
  signum = op1 signum
  negate = op1 negate

instance Fractional Length where
  fromRational x = Length (fromRational x) Nothing
  (/) = op2 "/" (/)

op1 :: (Float -> Float) -> Length -> Length
op1 f (Length a u) = Length (f a) u

op2 :: String -> (Float -> Float -> Float) -> Length -> Length -> Length
op2 nm f (Length a u1) (Length b u2)
  | u1 == u2  = Length (f a b) u1
  | otherwise =
    error ("[" ++ nm ++ "] Unit mismatch: " ++ showU u1 ++ " vs. " ++ showU u2)
    where showU = maybe "no unit" unpack


