module RegExpLibrary where

import Data.Set (Set)
import qualified Data.Set as Set


-- Core Language
data RegExp
  = Empty
  | Epsilon
  | Literal Char
  | Concat RegExp RegExp
  | Union RegExp RegExp
  | Intersect RegExp RegExp
  | Star RegExp
  deriving (Eq, Show)

-- User-Facing Combinators
infixl 5 <+>
(<+>) :: RegExp -> RegExp -> RegExp
(<+>) = Union

infixl 6 <&>
(<&>) :: RegExp -> RegExp -> RegExp
(<&>) = Intersect

infixl 7 <.>
(<.>) :: RegExp -> RegExp -> RegExp
(<.>) = Concat


starWildcard :: RegExp -> RegExp
starWildcard = Star

range :: Int -> Int -> RegExp -> RegExp
range minR maxR r
  | minR <= 0 = Epsilon <+> (r <.> starRange (minR + 1) maxR r)
  | otherwise = r <.> starRange minR maxR r

starRange :: Int -> Int -> RegExp -> RegExp
starRange minR maxR r
  | minR <= 0 = Epsilon <+> (r <.> starRange 0 (maxR - 1) r)
  | minR == 1 = starWildcard r <+> (r <.> starRange 0 (maxR - 1) r)
  | otherwise = starWildcard (r <.> starRange (minR - 1) (maxR - 1) r)

union :: RegExp -> RegExp -> RegExp
union Empty r2 = r2
union r1 Empty = r1
union r1 r2 = Union r1 r2

concatenate :: RegExp -> RegExp -> RegExp
concatenate Empty _ = Empty
concatenate _ Empty = Empty
concatenate Epsilon r = r
concatenate r Epsilon = r
concatenate Epsilon Epsilon = Epsilon
concatenate r1 r2 = Concat r1 r2


complement :: RegExp -> RegExp
complement Empty = Star Epsilon
complement Epsilon = Star Empty
complement (Literal c) = Literal c
complement (Concat r1 r2) = complement r1 <.> complement r2
complement (Union r1 r2) = complement r1 <&> complement r2
complement (Star r) = Concat (complement r) (complement r)

reverseRegExp :: RegExp -> RegExp
reverseRegExp Empty = Empty
reverseRegExp Epsilon = Epsilon
reverseRegExp (Literal c) = Literal c
reverseRegExp (Concat r1 r2) = reverseRegExp r2 <.> reverseRegExp r1
reverseRegExp (Union r1 r2) = reverseRegExp r1 <+> reverseRegExp r2
reverseRegExp (Star r) = Star (reverseRegExp r)

-- Properties
nullable :: RegExp -> Bool
nullable Empty = False
nullable Epsilon = True
nullable (Literal _) = False
nullable (Concat r1 r2) = nullable r1 && nullable r2
nullable (Union r1 r2) = nullable r1 || nullable r2
nullable (Star _) = True

deriv :: Char -> RegExp -> RegExp
deriv _ Empty = Empty
deriv _ Epsilon = Empty
deriv c (Literal c') = if c == c' then Epsilon else Empty
deriv c (Concat r1 r2) = (deriv c r1 <.> r2) <+> (if nullable r1 then deriv c r2 else Empty)
deriv c (Union r1 r2) = deriv c r1 <+> deriv c r2
deriv c (Star r) = deriv c r <.> Star r

-- Matcher using Brzozowski Derivatives
match :: RegExp -> String -> Bool
match r s = nullable (foldl (flip deriv) r s)
