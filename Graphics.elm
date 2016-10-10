module Graphics exposing (..)
import Board exposing (Board, Block(..), Position)
import Collage exposing (collage, rect, filled, move, Form, Shape, toForm, rotate)
import Color exposing (rgb)
import Element exposing (Element, toHtml)

blockSize : Int
blockSize = 20

boardSize : { width : Int, height : Int }
boardSize =
  { width = blockSize * 8
  , height = blockSize * 17
  }

drawBoard : Board -> Element
drawBoard board =
  collage boardSize.width boardSize.height
    ( -- bg
      [ rect (toFloat boardSize.width) (toFloat boardSize.height)
        |> filled (rgb 225 225 225)
      ]
      ++
      -- pills and virii
      List.filterMap drawBlock (Board.positionedBlocks board)
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

drawBlock : (Position, Block) -> Maybe Form
drawBlock ((x, y), block) =
  case block of
    Empty ->
      Nothing
    Virus color ->
      Just (drawTile x y color)
    Pill color connected falling ->
      Just (drawTile x y color
        |> if falling then
            rotate (degrees 10)
          else
            \a -> a  --no-op
        )

drawTile : Int -> Int -> Board.Color -> Form
drawTile x y color =
  rect (toFloat blockSize) (toFloat blockSize)
    |> filled (convertColor color)
    |> resetPos
    |> move (toFloat (x * blockSize), toFloat (y * blockSize))


convertColor : Board.Color -> Color.Color
convertColor color =
  case color of
    Board.Red ->    rgb 225  75  75
    Board.Blue ->   rgb  75  75 225
    Board.Yellow -> rgb 225 225  75
