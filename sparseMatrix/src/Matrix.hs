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
  
  add (Cell b1 v1) (Cell b2 v2) = Cell b1 (v1 + v2)
  add (Cell b v) (Quad b' nw ne sw se) = add (Quad b (Cell (b-1) v) (Cell (b-1) v) (Cell (b-1) v) (Cell (b-1) v)) (Quad b' nw ne sw se)
  add quad@(Quad _ _ _ _ _) cell@(Cell _ _) = add cell quad
  add (Quad b nw1 ne1 sw1 se1) (Quad _ nw2 ne2 sw2 se2) = Quad b (add nw1 nw2) (add ne1 ne2) (add sw1 sw2) (add se1 se2)

  sub m1 m2 = add m1 (neg m2)

  neg (Cell b v) = Cell b (-v)
  neg (Quad b nw ne sw se) = Quad b (neg nw) (neg ne) (neg sw) (neg se)

  mult (Cell b1 x) m2 = scalarMult x m2
  mult m1 (Cell b2 y) = scalarMult y m1
  mult (Quad b1 nw1 ne1 sw1 se1) (Quad b2 nw2 ne2 sw2 se2)
    | b1 == b2 = let
      nw = add (mult nw1 nw2) (mult ne1 sw2)
      ne = add (mult nw1 ne2) (mult ne1 se2)
      sw = add (mult sw1 nw2) (mult se1 sw2)
      se = add (mult sw1 ne2) (mult se1 se2)
      in Quad b1 nw ne sw se
    | otherwise = error "Matrixmult: Bounds do not match."
  mult _ _ = error "Matrixmult: Mismatch in QuadTree dimensions."
  mult (Quad 0 _ _ _ _) (Quad 0 _ _ _ _) = cell 0 0
  mult _ (Quad 0 _ _ _ _) = cell 0 0
  mult (Quad 0 _ _ _ _) _ = cell 0 0
  mult (Cell 0 _) _ = cell 0 0
  mult _ (Cell 0 _) = cell 0 0

  transpose (Cell b x) = cell b x
  transpose (Quad b nw ne sw se) = Quad b (transpose nw) (transpose sw) (transpose ne) (transpose se)

  scalarMult s (Cell b1 x) = cell b1 (s * x)
  scalarMult s (Quad b1 nw1 ne1 sw1 se1) = Quad b1 (scalarMult s nw1) (scalarMult s ne1) (scalarMult s sw1) (scalarMult s se1)
