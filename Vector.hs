module Vector where

---- Vector
data Vector = Vector Float Float deriving (Eq)

type Position = Vector

vadd (Vector a b) (Vector c d) = Vector (a + c) (b + d)

vsub (Vector a b) (Vector c d) = Vector (a - c) (b - d)

vlen (Vector a b) = sqrt $ (a*a + b*b)

vscale (Vector a b) factor = Vector (a * factor) (b * factor)

instance Show Vector where
    show (Vector a b) = "[" ++ (show a) ++ ", " ++ (show b) ++ "]"
