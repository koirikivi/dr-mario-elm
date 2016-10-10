module Board exposing (..)
import Array exposing (Array)
import Maybe exposing (andThen, withDefault)


-- DATA

type alias Board =
  Array Row

type alias Row =
  Array Block

type Block
  = Empty
  | Virus Color
  -- color, connected, falling
  | Pill Color Direction Bool

type Color
  = Red
  | Blue
  | Yellow

type Direction
  = Up
  | Down
  | Left
  | Right
  | None

type alias Position =
  (Int, Int)


-- UTILS

blankBoard : Board
blankBoard =
  let
    blankRow = Array.initialize 8 (always Empty)
  in
    Array.initialize 17 (always blankRow)

testBoard : Board
testBoard =
  blankBoard
    |> set (1, 1) (Pill Red None False)
    |> set (1, 2) (Pill Yellow None False)
    |> set (4, 5) (Pill Blue None False)
    |> set (4, 6) (Pill Red None False)
    |> set (0, 0) (Pill Yellow None False)
    |> set (6, 3) (Virus Yellow)

set : Position -> Block -> Board -> Board
set (x, y) block board =
  let
    row' = Array.get y board `andThen` (Just << Array.set x block)
  in
    case row' of
      Nothing ->
        board
      Just row'' ->
        Array.set y row'' board

-- return the block in x, y, out of bounds == empty
-- (Debug.crash would arguably be better)
get : Position -> Board -> Block
get (x, y) board =
  (Array.get y board `andThen` Array.get x)
    |> withDefault Empty

positionedBlocks : Board -> List (Position, Block)
positionedBlocks board =
  Array.toIndexedList board
    |> List.map (\(y, row) ->
      Array.toIndexedList row
        |> List.map (\(x, block) -> ((x, y), block)))
    |> List.concat

-- create a new board by iterating blocks bottom-up
foldb : ((Position, Block) -> Board -> Board) -> Board -> Board
foldb callback board =
  positionedBlocks board
    |> List.foldl callback board

-- GAME LOGIC

update : Board -> Board
update board =
  board
    |> applyGravity

applyGravity : Board -> Board
applyGravity board =
  board
    |> foldb markFalling
    |> foldb moveFalling

markFalling : (Position, Block) -> Board -> Board
markFalling ((x, y), block) board =
  case block of
    Pill color connected falling ->
      let
        falling' =
          case connected of
            Left ->
              (canBeFallenThrough (x, y - 1) board
                && canBeFallenThrough (x - 1, y - 1) board)
            Right ->
              (canBeFallenThrough (x, y - 1) board
                && canBeFallenThrough (x + 1, y - 1) board)
            _ ->
              canBeFallenThrough (x, y - 1) board
      in
        set (x, y) (Pill color connected falling') board
    _ ->
      board

moveFalling : (Position, Block) -> Board -> Board
moveFalling ((x, y), block) board =
  case block of
    Pill color connected True ->
      board
        |> set (x, y - 1) (Pill color connected True)
        |> set (x, y) Empty
    _ ->
      board

canBeFallenThrough : Position -> Board -> Bool
canBeFallenThrough (x, y) board =
  if y < 0 then
    False
  else
    case (get (x, y) board) of
      Empty ->
        True
      Pill _ _ True ->
        True
      _ ->
        False
