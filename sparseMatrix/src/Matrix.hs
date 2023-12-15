{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Matrix where

import QuadTree

diagonalM bound x = 
  foldr (\b m -> insert m (Point b b) x) (Cell bound 0) [0..2^bound-1]

class Num a => Matrix matrix a where 
  add :: matrix a -> matrix a -> matrix a 

  sub :: matrix a -> matrix a -> matrix a

  neg :: matrix a -> matrix a 

  mult :: matrix a -> matrix a -> matrix a  

  transpose :: matrix a -> matrix a 

  scalarMult :: a -> matrix a -> matrix a 

instance (Eq bound, Integral bound, Eq a, Num a) => Matrix (QuadTree bound) a where 

