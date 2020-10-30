module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onInput)

import Ty exposing (Ty)
import Absyn
import ANormal

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
        |> Dict.insert "add" ( ANormal.Kernel, Ty.Ext [ Ty.I32, Ty.I32 ] Ty.I32 )
        |> Dict.insert "sub" ( ANormal.Kernel, Ty.Ext [ Ty.I32, Ty.I32 ] Ty.I32 )
        |> Dict.insert "mul" ( ANormal.Kernel, Ty.Ext [ Ty.I32, Ty.I32 ] Ty.I32 )
        |> Dict.insert "eq"  ( ANormal.Kernel, Ty.Ext [ Ty.I32, Ty.I32 ] Ty.Bool )

    absyn =
      model.source
        |> Absyn.parseExp

    anormal =
      absyn
        |> Result.andThen (\exp -> ANormal.g env exp 0)
        |> Result.map (\( tm, ty, _ ) -> ( tm, ty ))

  in
    Html.div []
    [ inputView
    , absynView absyn
    , aNormalView anormal
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

aNormalView : Result String ( ANormal.Exp, Ty ) -> Html Msg
aNormalView r =
  let
    str = case r of
      Ok ( term, _ ) ->
        ANormal.toString term

      Err cause ->
        cause
  in
    Html.div []
    [ Html.h2 []
      [ Html.text "A-Normal"
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