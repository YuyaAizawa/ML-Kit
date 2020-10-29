module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onInput)

import Ty exposing (Ty)
import Absyn
import KNormal
import Closure

import Dict
import Set

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL --

type alias Model =
  { source : String
  }

init : () -> (Model, Cmd Msg)
init _ =
  (
    { source = ""
    }
  , Cmd.none
  )



-- UPDATE --

type Msg
  = UpdateSource String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateSource source ->
      ( { model | source = source }
      , Cmd.none
      )

-- VIEW --

view : Model -> Html Msg
view model =
  let
    env =
      Dict.empty
        |> Dict.insert "add" ( KNormal.Kernel, Ty.Ext [ Ty.I32, Ty.I32 ] Ty.I32 )
        |> Dict.insert "sub" ( KNormal.Kernel, Ty.Ext [ Ty.I32, Ty.I32 ] Ty.I32 )
        |> Dict.insert "mul" ( KNormal.Kernel, Ty.Ext [ Ty.I32, Ty.I32 ] Ty.I32 )
        |> Dict.insert "eq"  ( KNormal.Kernel, Ty.Ext [ Ty.I32, Ty.I32 ] Ty.Bool )

    absyn =
      model.source
        |> Absyn.parseExp

    knormal =
      absyn
        |> Result.andThen (\exp -> KNormal.g env exp 0)
        |> Result.map (\( tm, ty, _ ) -> ( tm, ty ))

    closure =
      knormal
        |> Result.map (\( tm, _ ) -> Closure.g Dict.empty Set.empty tm [])
  in
    Html.div []
    [ inputView
    , absynView absyn
    , kNormalView knormal
    , closureView closure
    ]

inputView : Html Msg
inputView =
  Html.textarea
  [ onInput UpdateSource
  , Attr.rows 4
  ]
  []

absynView : Result String Absyn.Exp -> Html Msg
absynView r =
  let
    str = case r of
      Ok ast ->
        Absyn.toString ast

      Err cause ->
        cause
  in
    Html.div []
    [ Html.h2 []
      [ Html.text "Absyn"
      ]
    , Html.pre []
      [ Html.code []
        [ Html.text str
        ]
      ]
    ]

kNormalView : Result String ( KNormal.Term, Ty ) -> Html Msg
kNormalView r =
  let
    str = case r of
      Ok ( term, _ ) ->
        KNormal.toString term

      Err cause ->
        cause
  in
    Html.div []
    [ Html.h2 []
      [ Html.text "K-Normal"
      ]
    , Html.pre []
      [ Html.code []
        [ Html.text str
        ]
      ]
    ]

closureView : Result String ( Closure.Term, List Closure.FunDef ) -> Html Msg
closureView r =
  let
    str = case r of
      Ok ( term, toplevel ) ->
        let
          funs =
            toplevel
              |> List.map Closure.funDefToString
              |> String.join "\n"

          main_ =
            term |>
              Closure.termToString
        in
          funs ++ "\n\n" ++ main_

      Err cause ->
        cause
  in
    Html.div []
    [ Html.h2 []
      [ Html.text "Closure"
      ]
    , Html.pre []
      [ Html.code []
        [ Html.text str
        ]
      ]
    ]



-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none