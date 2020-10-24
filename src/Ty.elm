module Ty exposing
  ( Ty(..)
  )

type Ty
 = Bool
 | I32
 | Arrow Ty Ty
 | Custom String
