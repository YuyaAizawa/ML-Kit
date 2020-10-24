module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onInput)

import Ty
import Absyn
import KNormal

import Dict

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
  [ inputView model
  , outputView model
  ]
    |> Html.div []

inputView : Model -> Html Msg
inputView model =
  Html.textarea
  [ onInput UpdateSource
  , Attr.rows 4
  ]
  []

outputView : Model -> Html Msg
outputView model =
  let
    env =
      Dict.empty
        |> Dict.insert "add" (KNormal.Kernel, Ty.Arrow Ty.I32 (Ty.Arrow Ty.I32 Ty.I32))
        |> Dict.insert "sub" (KNormal.External, Ty.Arrow Ty.I32 (Ty.Arrow Ty.I32 Ty.I32))

    out =
      model.source
        |> Absyn.parseExp
        |> Result.map (KNormal.g env {fresh = 0})
        |> Result.map (\( tm, _, _ ) -> KNormal.toString tm)
        |> Result.withDefault "parse error"
  in
    Html.pre []
    [ Html.code []
      [ Html.text out
      ]
    ]



-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none