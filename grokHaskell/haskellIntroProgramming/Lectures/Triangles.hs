-- Triangles.hs (Lecture 1)

-- | A module for working with triangles.

module Triangles where

-- | Compute the length of the hypotenuse of a right triangle
--   from the lengths of its two other sides.

hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt $ square a + square b

-- | Square a number

square :: Num n => n -> n
square x = x * x

-- | Convert an angle from degrees to radians

deg2rad :: Double -> Double
deg2rad θ = θ * pi / 180

-- | Law of cosines, compute the length of the third side c of a triangle
--   given the lengths of the other two sides, a and b, and the angle γ
--   between them.  The angle γ is in degrees.

law_of_cosines :: Double -> Double -> Double -> Double
law_of_cosines a b γ =
  let
    θ = deg2rad γ
  in
    sqrt $ square a + square b - 2*a*b*cos θ 
