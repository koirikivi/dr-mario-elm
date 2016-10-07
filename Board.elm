module Board exposing (..)
import Collage exposing (collage, rect, filled, move, Form, Shape, toForm, rotate)
import Color exposing (rgb)
import Dict
import Element exposing (Element, toHtml)
import List


-- MODEL

type alias Board =
  { blocks : BlockDict }

type alias BlockDict =
  Dict.Dict (Int, Int) Block

blankBoard : Board
blankBoard =
  { blocks =
      Dict.fromList
        [ ((1, 1), (Block Pill Red False None))
        , ((1, 2), (Block Pill Yellow False None))
        , ((4, 5), (Block Pill Blue False None))
        , ((4, 6), (Block Pill Red False None))
        , ((0, 0), (Block Pill Yellow False None))
        ]
  }

boardDimensions : { width: Int, height: Int }
boardDimensions =
  { width = 8
  , height = 17
  }

type alias Block =
  { blockType : Type
  , color : Color
  , falling : Bool
  , connected : Direction
  }

type Type
  = Pill
  | Virus

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


-- UPDATE

update : Board -> Board
update board =
  let
    blocks = board.blocks
      |> applyGravity
  in
    { board | blocks = blocks }

applyGravity : BlockDict -> BlockDict
applyGravity blocks =
  let
    lowestFirst = \((x, y), block) -> y
    conditionallyMoveDown = \((x, y), block) ->
      if y == 0 then
        ((x,y), { block | falling = False })
      else
        case Dict.get (x, y - 1) blocks of
          Just _ ->
            ((x,y), { block | falling = False })
          Nothing->
            ((x, y - 1), { block | falling = True })
  in
    blocks
      |> Dict.toList
      --|> List.sortBy lowestFirst
      |> List.map conditionallyMoveDown
      |> Dict.fromList


-- VIEW

blockSize : Int
blockSize = 20

boardSize : { width : Int, height : Int }
boardSize =
  { width = blockSize * boardDimensions.width
  , height = blockSize * boardDimensions.height
  }

draw : Board -> Element
draw board =
  collage boardSize.width boardSize.height
    ( -- bg
      [ rect (toFloat boardSize.width) (toFloat boardSize.height)
        |> filled (rgb 225 225 225)
      ]
      ++
      -- pills and virii
      List.map drawBlock (Dict.toList board.blocks)
    )

-- move a block to the bottom-right corner
resetPos : Form -> Form
resetPos shape =
  let
    boardLeft = toFloat boardSize.width / 2
    boardBottom = toFloat boardSize.height / 2
    left = boardLeft - (toFloat blockSize / 2)
    bottom = boardBottom - (toFloat blockSize / 2)
  in
    shape
      |> move (-left, -bottom)

drawBlock : ((Int, Int), Block) -> Form
drawBlock ((x, y), block) =
  let
    color = case block.color of
      Red ->    rgb 225  75  75
      Blue ->   rgb  75  75 225
      Yellow -> rgb 225 225  75
  in
    rect (toFloat blockSize) (toFloat blockSize)
      |> filled color
      |> resetPos
      |> move (toFloat (x * blockSize), toFloat (y * blockSize))
      |> if block.falling then
          rotate (degrees 10)
        else
          \a -> a  --no-op
