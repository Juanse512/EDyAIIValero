data Color = RGB Int Int Int deriving Show
mezclar :: Color -> Color -> Color
mezclar (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (div (r1+r2) 2) (div (g1+g2) 2) (div (b1+b2) 2)