{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Test.Matrix.Gen where 

import Matrix 
import QuadTree

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genNat n = Gen.int (Range.constant 0 n)
genInt n = Gen.int (Range.constantFrom 0 (-n) n)

genMatrix b maxInt
  | b <= 0 = genCell 
  | otherwise = do 
      Gen.recursive
        Gen.choice
        [ -- non-recursive generators
          genCell
        ]
        [ -- recursive generators
          genQuad
        ]
  where 
    genCell = do 
      x <- genInt maxInt
      return $ Cell b x 
    genQuad = do 
      let genSmaller = genMatrix (b - 1) maxInt
      nw <- genSmaller 
      ne <- genSmaller 
      sw <- genSmaller 
      se <- genSmaller 
      return $ Quad b nw ne sw se

