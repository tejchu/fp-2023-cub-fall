# RegExp Properties

## Identity Law for Union

**Property:** The union of a regular expression `r` and the empty set (`∅`) is equal to `r`.

**Implementation:** `r ∪ ∅ = r` and `∅ ∪ r = r`

## Absorption Law for Concatenation

**Property:** The concatenation of a regular expression `r` and the union of two regular expressions `r` and `s` is equal to `r`.

**Implementation:** `r ⋅ (r ∪ s) = r` and `(r ∪ s) ⋅ r = r`

## Idempotent Law for Union

**Property:** The union of a regular expression `r` with itself is equal to `r`.

**Implementation:** `r ∪ r = r`

## Distributive Law for Union and Concatenation

**Property:** The concatenation of a regular expression `r` with the union of two regular expressions `s` and `t` is equal to the union of the concatenation of `r` and `s` with the concatenation of `r` and `t`.

**Implementation:** `r ⋅ (s ∪ t) = r ⋅ s ∪ r ⋅ t`

