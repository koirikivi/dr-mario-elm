import Collage exposing (collage, rect, filled, move, toForm)
import Debug
import Element exposing (Element, toHtml)
import Html exposing (Html)
import Html.App as App
import Keyboard
import Maybe exposing (withDefault, oneOf)
import Time exposing (Time, second)

import Board exposing (Board)
import Graphics
import PlayerPill exposing (PlayerPill, RotationDirection(..))


-- MODEL

type alias Model =
  { pill : Maybe PlayerPill.PlayerPill
  , board : Board.Board
  }

initialModel : Model
initialModel =
  { pill = Just PlayerPill.testPill
  , board = Board.testBoard
  }


-- VIEW

view : Model -> Html msg
view model =
  let
    boardForms = 
      [ toForm (Graphics.drawBoard model.board) ]
    pillForms =
      case model.pill of
        Nothing -> []
        Just pill -> [ toForm (Graphics.drawPill pill) ]
  in
    toHtml
      ( collage 400 500 (boardForms ++ pillForms)
      )


-- MESSAGES

type Msg
  = KeyPressed Key
  | GameTick Float

type Key
  = Up
  | Down
  | Left
  | Right
  | RotateLeft
  | RotateRight
  | OtherKey Int


-- UPDATE


update :  Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyPressed key ->
      ( handleKeypress model key, Cmd.none)
    GameTick time ->
      ( updateGame model, Cmd.none )

updateGame : Model -> Model
updateGame model =
  model
    |> updateBoard
    |> updatePill

updateBoard : Model -> Model
updateBoard model =
  { model | board = Board.update model.board }

updatePill : Model -> Model
updatePill model =
  case model.pill of
    Nothing -> model
    Just pill ->
      let
        newState = tryMove (PlayerPill.move (0, -1)) model
      in
        case newState of
          Just model' ->
            model'
          Nothing ->
            { model | board = applyPill pill model.board
                    , pill = Nothing }

applyPill : PlayerPill -> Board -> Board
applyPill pill board =
  PlayerPill.toBlocks pill
    |> List.foldl (\(pos, block) -> Board.set pos block) board

handleKeypress : Model -> Key -> Model
handleKeypress model key =
  let
    moveLeft = PlayerPill.move (-1, 0)
    moveRight = PlayerPill.move (1, 0)
    moveDown = PlayerPill.move (0, -1)
    rotateCW = PlayerPill.rotate ClockWise
    rotateCCW = PlayerPill.rotate CounterClockWise
  in
    case key of
      Left ->
        tryMoves [ moveLeft ] model
      Right ->
        tryMoves [ moveRight ] model
      Up ->  -- TODO: nope
        tryMoves [ PlayerPill.move (0, 1) ] model
      Down ->
        tryMoves [ moveDown ] model
      -- TODO: These two don't seem quite right
      RotateLeft ->
        tryMoves [ rotateCW, rotateCW >> moveRight ] model
      RotateRight ->
        tryMoves [ rotateCCW, rotateCCW >> moveLeft ] model
      OtherKey keyCode ->
        Debug.log "keycode" keyCode |> \a -> model

tryMoves : List (PlayerPill -> PlayerPill) -> Model -> Model
tryMoves moves model =
  moves
    |> List.map (\move -> tryMove move model)
    |> oneOf
    |> withDefault model

tryMove : (PlayerPill -> PlayerPill) -> Model -> Maybe Model
tryMove move model =
  case model.pill of
    Nothing -> Nothing
    Just pill ->
      let
        pill' = move pill
        (pos1, pos2) = PlayerPill.coordinates pill'
      in
        if (Board.canBeMovenThrough pos1 model.board
            && Board.canBeMovenThrough pos2 model.board) then
          Just { model | pill = Just pill' }
        else
          Nothing

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs (\k -> KeyPressed (codeToKey k))
    , Time.every second GameTick
    ]

codeToKey : Keyboard.KeyCode -> Key
codeToKey code =
  case code of
    38 -> Up
    40 -> Down
    37 -> Left
    39 -> Right
    90 -> RotateLeft  -- z
    88 -> RotateRight -- x
    c  -> OtherKey c


-- MAIN

main : Program Never
main =
  App.program
    { init = ( initialModel, Cmd.none )
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
