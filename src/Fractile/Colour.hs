module Fractile.Colour where

data Colour = RGB Int Int Int | LAB Double Double Double
data XYZ = XYZ Double Double Double

instance Show Colour where
  show (RGB r g b) = "RGB: (" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")"
  show (LAB l a b) = "CIELAB: (" ++ (show l) ++ "," ++ (show a) ++ "," ++ (show b) ++ ")"

instance Show XYZ where
  show (XYZ x y z) = "RGB: (" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ ")"

-- limits
rgb_upper :: Int
rgb_upper = 255
rgb_lower :: Int
rgb_lower = 0
l_upper :: Double
l_upper = 100
l_lower  :: Double
l_lower = 0
a_upper :: Double
a_upper = 98.24941490526
a_lower :: Double
a_lower = (-86.188001161251748)
b_upper :: Double
b_upper = 94.487327241851375
b_lower :: Double
b_lower = (-107.9)

-- conversion vals
epsilon :: Double
epsilon = 0.008856
kappa :: Double
kappa  = 903.3

-- XYZ REFS
ref_X :: Double
ref_X = 95.047
ref_Y :: Double
ref_Y = 100.000
ref_Z :: Double
ref_Z = 108.883

convertToXYZ :: Colour -> XYZ
convertToXYZ (RGB r g b) =
  let compander n
        | i > 0.04045 = ((i + 0.055) / 1.055) ** 2.4
        | otherwise = i / 12.92
        where i = (fromIntegral n) / 255.0
      (rC, gC, bC) = (100 * (compander r), 100 * (compander g), 100 * (compander b))
      x = (rC * 0.4124 + gC * 0.3576 + bC * 0.1805) 
      y = (rC * 0.2126 + gC * 0.7152 + bC * 0.0722)
      z = (rC * 0.0193 + gC * 0.1192 + bC * 0.9505)
  in XYZ x y z
convertToXYZ (LAB l a b) =
  let y' = (l + 16) / 116
      x' = (a / 500) + y'
      z' = y' - (b / 200)
      helperXZ xz
        | (xz ** 3) > epsilon = xz ** 3
        | otherwise = ((116 * xz) - 16) / kappa
      helperY y
        | y > (epsilon * kappa) = ((y + 16) / 116) ** 3
        | otherwise = y / kappa
      x = helperXZ x'
      y = helperY y'
      z = helperXZ z'
  in XYZ (x * ref_X) (y * ref_Y) (z * ref_Z)


xyzToRGB :: XYZ -> Colour
xyzToRGB (XYZ x y z) =
  let x' = x / 100
      y' = y / 100
      z' = z / 100
      r' = x' * 3.2406 + y' * (-1.5372) + z' * (-0.4986)
      g' = x' * (-0.9689) + y' *  1.8758 + z' *  0.0415
      b' = x' * 0.0557 + y' * (-0.2040) + z' * 1.0570
      helper n
        | n > 0.0031308 = floor(1.055 * (n ** (1/2.4)) - 0.055)
        | otherwise = floor(n * 12.92)
  in RGB (helper r') (helper g') (helper b')

xyzToLAB :: XYZ -> Colour
xyzToLAB (XYZ x y z) =
  let x' = x / ref_X
      y' = y / ref_Y
      z' = z / ref_Z
      helper n
        | n > epsilon = n ** (1/3.0)
        | otherwise = ((903.3 * n) + 16) / 116
      l' = helper x'
      a' = helper y'
      b' = helper z'
  in LAB (116 * a' - 16) (500 * (l' - a')) (200 * (a' - b'))

convertToRGB :: Colour -> Colour
convertToRGB (LAB l a b) =
  let xyz = convertToXYZ (LAB l a b)
  in clampColour(xyzToRGB xyz)
convertToRGB rgb = rgb

convertToLAB :: Colour -> Colour
convertToLAB (RGB r g b) =
  let xyz = convertToXYZ (RGB r g b)
  in clampColour(xyzToLAB xyz)
convertToLAB lab = lab

clamp :: Ord a => a -> a -> a -> a
clamp upper lower x
  | x < lower = lower
  | x > upper = upper
  | otherwise = x

clampColour :: Colour -> Colour
clampColour (RGB r g b) = 
  let clampRGB = clamp rgb_upper rgb_lower
  in RGB (clampRGB r) (clampRGB g) (clampRGB b)
clampColour (LAB l a b) = LAB (clamp l_upper l_lower l) (clamp a_upper a_lower a) (clamp b_upper b_lower b)
