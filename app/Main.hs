
module Main where
import Test.QuickCheck
import RegExpLibrary

instance Arbitrary RegExp where
  arbitrary = sized arbRegExp

arbRegExp :: Int -> Gen RegExp
arbRegExp 0 = elements [Empty, Epsilon, Literal 'a']
arbRegExp n = oneof
  [ elements [Empty, Epsilon, Literal 'a']
  , resize (n `div` 2) $ do
      r1 <- arbRegExp (n `div` 2)
      r2 <- arbRegExp (n `div` 2)
      elements [Concat r1 r2, Union r1 r2, Star r1]
  ]


-- Property: Reverse of Reverse is the original expression
prop_ReverseDoubleNegation :: RegExp -> Bool
prop_ReverseDoubleNegation r = reverseRegExp (reverseRegExp r) == r

-- Property: Derivative of Star is Star of Derivative
-- prop_StarDerivative :: Char -> RegExp -> Bool

-- Property: Matching reversed string is the same as reversing the pattern
prop_MatchReversed :: RegExp -> String -> Bool
prop_MatchReversed r s = match r (reverse s) == match (reverseRegExp r) s



main :: IO ()
main = do
  putStrLn "Running Property-Based Tests..."
  quickCheck prop_ReverseDoubleNegation
  quickCheck prop_MatchReversed
