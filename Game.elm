import Collage exposing (collage, rect, filled, move, toForm)
import Dict
--import Debug
import Element exposing (Element, toHtml)
import Html exposing (Html)
import Html.App as App
import Keyboard
import Time exposing (Time, second)

import Board
import Graphics


-- MODEL

type alias Model =
  { rectX : Float
  , rectY : Float
  , board : Board.Board
  }

initialModel : Model
initialModel =
  { rectX = 15
  , rectY = 85
  , board = Board.testBoard
  }


-- VIEW

scene : Model -> Element
scene model =
  collage 400 500
    [ toForm (Graphics.drawBoard model.board)
    --, rect 20 20
    --    |> filled (rgb 174 38 238)
    --    |> move (model.rectX, model.rectY)
    ]

view : Model -> Html msg
view model =
  toHtml (scene model)


-- MESSAGES

type Msg
  = KeyPressed Key
  | UpdateBoard Float

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
    KeyPressed key ->
      ( handleKeypress model key, Cmd.none)
    UpdateBoard time ->
      let
        --_ = Debug.log "updating board" time
        foo = "foo"
      in
        ( { model | board = Board.update model.board }, Cmd.none )

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
    [ Keyboard.downs (\k -> KeyPressed (codeToKey k))
    , Time.every second UpdateBoard
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
