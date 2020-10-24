module AbsynTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Ty exposing (Ty)
import Absyn exposing (parseExp, parseType, Exp(..))



suite : Test
suite =
  describe "all"
  [ let
      a = Ty.Custom "A"
      arr = Ty.Arrow
    in
      describe "Type"
      [ test "A" <|
        \_ ->
          parseType "A"
            |> Expect.equal (Ok a)

      , test "A->A" <|
        \_ ->
          parseType "A->A"
            |> Expect.equal (Ok (arr a a))

      , test "A->A->A" <|
        \_ ->
          parseType "A->A->A"
            |> Expect.equal (Ok (arr a (arr a a)))

      , test "A->(A->A)" <|
        \_ ->
          parseType "A->(A->A)"
            |> Expect.equal (Ok (arr a (arr a a)))

      , test "(A->A)->A" <|
        \_ ->
          parseType "(A->A)->A"
            |> Expect.equal (Ok (arr (arr a a) a))
      ]
  , describe "Exp"
    [ test "1" <|
      \_ ->
        parseExp "1"
          |> Expect.equal (Ok (Int 1))

    , test "add" <|
      \_ ->
        parseExp "add 1 2"
          |> Expect.equal (Ok (App (Var "add") [ Int 1, Int 2 ]))

    , test "apply" <|
      \_ ->
        parseExp "nade mofu funi nyan"
          |> Expect.equal (Ok (App (Var "nade") [ Var "mofu", Var "funi", Var "nyan" ]))
    ]
  ]