import Test.Hspec
import RegExpLibrary
import Test.QuickCheck


main :: IO ()
main = do
  -- Property-Based Tests
  putStrLn "Running Property-Based Tests..."
  quickCheck prop_ReverseDoubleNegation
  quickCheck prop_MatchReversed
  hspec $ do
    describe "nullable" $ do
      it "returns False for Empty" $
        nullable Empty `shouldBe` False

      it "returns True for Epsilon" $
        nullable Epsilon `shouldBe` True

      it "returns False for Literal" $
        nullable (Literal 'a') `shouldBe` False

      it "returns True for Concatenation of nullable regexps" $
        nullable (Concat Epsilon Epsilon) `shouldBe` True

      it "returns False for Concatenation of non-nullable regexps" $
        nullable (Concat Epsilon (Literal 'a')) `shouldBe` False

      it "returns True for Union of nullable regexps" $
        nullable (Union Epsilon Empty) `shouldBe` True

      it "returns False for Union of non-nullable regexps" $
        nullable (Union (Literal 'a') (Literal 'b')) `shouldBe` False

      it "returns True for Star of any regexp" $
        nullable (Star (Literal 'a')) `shouldBe` True

    describe "deriv" $ do
      it "returns Empty for Empty" $
        deriv 'a' Empty `shouldBe` Empty

      it "returns Empty for Epsilon" $
        deriv 'a' Epsilon `shouldBe` Empty

      it "returns Epsilon for matching Literal" $
        deriv 'a' (Literal 'a') `shouldBe` Epsilon

      it "returns Empty for non-matching Literal" $
        deriv 'a' (Literal 'b') `shouldBe` Empty

      it "returns Union of derivs for Union" $
        deriv 'a' (Union (Literal 'a') (Literal 'b')) `shouldBe` Union Epsilon Empty

      it "returns Concatenation of deriv and Star for Star" $
        deriv 'a' (Star (Literal 'a')) `shouldBe` Concat Epsilon (Star (Literal 'a'))

    describe "match" $ do
      it "returns True for matching regexp and string" $
        match (Literal 'a') "a" `shouldBe` True

      it "returns False for non-matching regexp and string" $
        match (Literal 'a') "b" `shouldBe` False

      it "returns True for matching regexp and empty string" $
        match Epsilon "" `shouldBe` True

      it "returns True for matching regexp and string with nullable parts" $
        match (Concat Epsilon (Literal 'a')) "a" `shouldBe` True

      it "returns False for non-matching regexp and string with nullable parts" $
        match (Concat Epsilon (Literal 'a')) "b" `shouldBe` False

      it "returns True for matching regexp and string with union of nullable parts" $
        match (Union Epsilon Empty) "" `shouldBe` True

      it "returns False for non-matching regexp and string with union of non-nullable parts" $
        match (Union (Literal 'a') (Literal 'b')) "" `shouldBe` False

      it "returns True for matching regexp and string with star of any regexp" $
        match (Star (Literal 'a')) "aaa" `shouldBe` True

      it "returns False for non-matching regexp and string with star of any regexp" $
        match (Star (Literal 'a')) "aab" `shouldBe` False



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


