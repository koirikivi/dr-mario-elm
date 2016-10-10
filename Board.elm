module Board exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import Maybe exposing (andThen, withDefault)
import Common exposing (Position)


-- DATA

type alias Board =
  Array Row

type alias Row =
  Array Block

type Block
  = Empty
  | Virus Color
  -- color, connected, falling
  | Pill Color Connection Bool

type Color
  = Red
  | Blue
  | Yellow

type Connection
  = Up
  | Down
  | Left
  | Right
  | None


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
    |> set (6, 5) (Pill Yellow None False)
    |> set (6, 6) (Pill Yellow None False)
    |> set (6, 7) (Pill Yellow None False)

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

get : Position -> Board -> Maybe Block
get (x, y) board =
  Array.get y board `andThen` Array.get x

blocks : Board -> List Block
blocks board =
  positionedBlocks board
    |> List.map (\((x, y), block) -> block)

positionedBlocks : Board -> List (Position, Block)
positionedBlocks board =
  Array.toIndexedList board
    |> List.map (\(y, row) ->
      Array.toIndexedList row
        |> List.map (\(x, block) -> ((x, y), block)))
    |> List.concat

-- create a new board by iterating indexed blocks bottom-up
foldb : ((Position, Block) -> Board -> Board) -> Board -> Board
foldb callback board =
  positionedBlocks board
    |> List.foldl callback board

isFalling : Block -> Bool
isFalling block =
  case block of
    Pill _ _ falling ->
      falling
    _ ->
      False

-- GAME LOGIC

update : Board -> Board
update board =
  board
    |> applyGravity
    |> (
      if List.any isFalling (blocks board) then
        identity
      else
        applyCombos
      )

applyGravity : Board -> Board
applyGravity board =
  board
    |> foldb markFalling
    |> foldb moveFalling

applyCombos : Board -> Board
applyCombos board =
  let
    colorPositions =
      positionedBlocks board
        |> List.filterMap (\(pos, block) ->
          case block of
            -- only blocks that are not falling are counted
            Pill color _ False ->
              Just (pos, color)
            Virus color ->
              Just (pos, color)
            _ ->
              Nothing
          )
    combos = detectCombos colorPositions
  in
    combos
      |> List.concat
      |> List.foldl (\position board -> set position Empty board) board

-- this is not yet final..
-- - combos on same row or column may be detected twice
-- - combos is a misnomer, perhaps
detectCombos : List (Position, Color) -> List (List Position)
detectCombos colorPositions =
  case colorPositions of
    [] ->
      []
    (pos, color)::remaining ->
      let
        colorMap = Dict.fromList remaining
        up = detectNearbyCombo (0, 1) pos color colorMap
        right = detectNearbyCombo (1, 0) pos color colorMap
        -- we proceed from up-right from the bottom-left corner,
        -- so some of this can be skipped
        --down = detectNearbyCombo (0, -1) pos color colorMap
        --left = detectNearbyCombo (-1, 0) pos color colorMap
        --horizontal = List.concat [ up, [pos], down ]
        --vertical = List.concat [ left, [pos], right ]
        vertical = pos :: up
        horizontal = pos :: right
      in
        ( if List.length vertical >= 4 then [ vertical ] else []
        ++ if List.length horizontal >= 4 then [ horizontal ] else []
        ++ detectCombos remaining
        )


detectNearbyCombo : Position -> Position -> Color -> Dict Position Color -> List Position
detectNearbyCombo (dirX, dirY) (posX, posY) color colorMap =
  let
    (newX, newY) = (dirX + posX, dirY + posY)
  in
    case Dict.get (newX, newY) colorMap of
      Just newColor ->
        if newColor == color then
            (newX, newY) :: detectNearbyCombo (dirX, dirY) (newX, newY) color colorMap
        else
          []
      Nothing ->
        []

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
  case (get (x, y) board) of
    Nothing ->
      False
    Just block ->
      case block of
        Empty ->
          True
        Pill _ _ True ->
          True
        _ ->
          False

canBeMovenThrough : Position -> Board -> Bool
canBeMovenThrough (x, y) board =
  case (get (x, y) board) of
    Nothing ->
      False
    Just block ->
      case block of
        Empty ->
          True
        _ ->
          False
