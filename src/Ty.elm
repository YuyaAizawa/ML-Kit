module Ty exposing
  ( Ty(..)
  , toString
  )

type Ty
 = Bool
 | I32
 | Arrow Ty Ty
 | Ext (List Ty) Ty
 | Custom String

toString : Ty -> String
toString ty =
  case ty of
    Bool ->
      "Bool"

    I32 ->
      "I32"

    Arrow t1 t2 ->
      let
        lhs = case t1 of
          Arrow _ _ -> "(" ++ toString t1 ++ ")"
          Ext _ _ -> "(" ++ toString t1 ++ ")"
          _ -> toString t1
      in
        lhs ++ " -> " ++ toString t2

    Ext args ret ->
      "[" ++ (args |> List.map toString |> String.join ", ") ++ "]-> " ++ toString ret

    Custom str ->
      str
