module PlayerPill exposing (
  PlayerPill,
  RotationDirection(..),
  testPill,
  toBlocks,
  move,
  coordinates,
  rotate
  )
import Board exposing (Color(..), Connection(..))
import Common exposing (Position)


-- DATA

type alias PlayerPill =
  { position : Position
  , colors : (Color, Color)
  , rotation : Rotation
  }

type Rotation
  = Flat
  | Degrees90
  | Degrees180
  | Degrees270

type RotationDirection
  = ClockWise
  | CounterClockWise


-- UTILS

testPill : PlayerPill
testPill = { position = (4, 12), colors = (Red, Blue), rotation = Flat }

rotate : RotationDirection -> PlayerPill -> PlayerPill
rotate direction pill =
  { pill | rotation = (nextRotation direction pill.rotation) }

move : Position -> PlayerPill -> PlayerPill
move (dx, dy) pill =
  let
    (x, y) = pill.position
  in
    { pill | position = (x + dx, y + dy) }

nextRotation : RotationDirection -> Rotation -> Rotation
nextRotation direction rotation =
  case direction of
    ClockWise ->
      case rotation of
        Flat       -> Degrees90
        Degrees90  -> Degrees180
        Degrees180 -> Degrees270
        Degrees270 -> Flat
    CounterClockWise ->
      case rotation of
        Flat       -> Degrees270
        Degrees90  -> Flat
        Degrees180 -> Degrees90
        Degrees270 -> Degrees180

-- first coord is always color1, second color2
coordinates : PlayerPill -> (Position, Position)
coordinates pill =
  let
    (x, y) = pill.position
  in
    case pill.rotation of
      Flat       -> ((x    , y    ), (x + 1, y    ))
      Degrees90  -> ((x    , y    ), (x    , y - 1))
      --Degrees180 -> ((x + 1, y    ), (x    , y    ))
      Degrees180 -> ((x    , y    ), (x - 1, y    ))
      --Degrees270 -> ((x    , y - 1), (x    , y    ))
      Degrees270 -> ((x    , y    ), (x    , y + 1))

connections : PlayerPill -> (Connection, Connection)
connections pill =
    case pill.rotation of
      Flat       -> ( Right, Left )
      Degrees90  -> ( Down, Up )
      Degrees180 -> ( Left, Right )
      Degrees270 -> ( Up, Down )

toBlocks : PlayerPill -> List (Position, Board.Block)
toBlocks pill =
  let
    (pos1, pos2) = coordinates pill
    (conn1, conn2) = connections pill
    (color1, color2) = pill.colors
  in
    [ (pos1, Board.Pill color1 conn1 False)
    , (pos2, Board.Pill color2 conn2 False)
    ]
