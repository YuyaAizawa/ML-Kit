module KNormalTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Dict
import Ty exposing (Ty)
import Absyn exposing (Exp)
import KNormal exposing (Term, Effect)



suite : Test
suite =
  describe "all"
  [ test "add" <|
      \_ ->
        let
          ast = Absyn.App (Absyn.Var "add") [ Absyn.Int 1, Absyn.Int 2 ]
          env = Dict.singleton "add" (KNormal.External, (Ty.Arrow Ty.I32 (Ty.Arrow Ty.I32 Ty.I32)))
        in
          Expect.pass
          --KNormal.g env { fresh = 0 } ast
          --  |> Expect.all
          --    [ \( tm,  _, _ ) -> tm |> Expect.equal (KNormal.Int 0)
          --    , \(  _, ty, _ ) -> ty |> Expect.equal Ty.Bool
          --    ]
  ]
