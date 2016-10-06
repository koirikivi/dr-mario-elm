import Collage exposing (collage, rect, filled, move)
import Color exposing (rgb)
import Dict
import Element exposing (Element, toHtml)
import Html exposing (Html)
import Html.App as App
import Keyboard


-- MODEL

type alias Model =
  { rectX : Float
  , rectY : Float
  }

initialModel : Model
initialModel =
  { rectX = 20
  , rectY = 20
  }


-- VIEW

scene : Model -> Element
scene model =
  collage 200 200
    [ rect 20 20
        |> filled (rgb 174 38 238)
        |> move (model.rectX, model.rectY)
    ]

view : Model -> Html msg
view model =
  toHtml (scene model)


-- MESSAGES

type Msg
  = KeyMsg Key

type Key
  = Up
  | Down
  | Left
  | Right
  | OtherKey Int


-- UPDATE


update :  Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyMsg key ->
      ( handleKeypress model key, Cmd.none)

handleKeypress : Model -> Key -> Model
handleKeypress model key =
  let
    speed = 10
  in
    case key of
      Up ->
        { model | rectY = model.rectY + speed }
      Down ->
        { model | rectY = model.rectY - speed }
      Left ->
        { model | rectX = model.rectX - speed }
      Right ->
        { model | rectX = model.rectX + speed }
      OtherKey _ ->
        model


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs (\k -> KeyMsg (codeToKey k))
    ]

codeToKey : Keyboard.KeyCode -> Key
codeToKey code =
  let
    keyEventMap =
      Dict.fromList
        [ ( 38, Up )
        , ( 40, Down )
        , ( 37, Left )
        , ( 39, Right )
        ]
    key = Dict.get code keyEventMap
  in
    case key of
      Just k ->
        k
      Nothing ->
        OtherKey code


-- MAIN

main : Program Never
main =
  App.program
    { init = ( initialModel, Cmd.none )
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
