module Graphics exposing (..)
import Collage exposing (
  collage, rect, filled, move, Form, Shape, toForm, rotate, dashed, outlined)
import Color exposing (rgb)
import Element exposing (Element, toHtml)

import Board exposing (Board, Block(..), Connection(..))
import Common exposing (Position)
import PlayerPill exposing (PlayerPill)


blockSize : Int
blockSize = 20

boardSize : { width : Int, height : Int }
boardSize =
  { width = blockSize * 8
  , height = blockSize * 17
  }

drawPill : PlayerPill -> Element
drawPill pill =
    collage boardSize.width boardSize.height
      (List.filterMap drawBlock (PlayerPill.toBlocks pill))

drawBoard : Board -> Element
drawBoard board =
  collage boardSize.width boardSize.height
    ( -- bg
      [ rect (toFloat boardSize.width) (toFloat boardSize.height)
        |> filled (rgb 245 245 245)
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
      Just (
        circleTile
          |> setColor color
          |> setPos x y
      )
    Pill color connected falling ->
      let
        form = case connected of
          None -> rectTile |> setColor color
          Right -> roundedRect color
          Up -> roundedRect color |> rotate (degrees 90)
          Left -> roundedRect color |> rotate (degrees 180)
          Down -> roundedRect color |> rotate (degrees 270)
      in
        Just (
          form
            |> setPos x y
            |> if falling then
                rotate (degrees 10)
              else
                identity
          )

roundedRect : Board.Color -> Form
roundedRect color =
  collage blockSize blockSize
    [ circleTile
        |> setColor color
    , rect (toFloat blockSize) (toFloat blockSize)
        |> setColor color
        |> Collage.moveX (toFloat blockSize / 2)
    ]
    |> toForm

rectTile : Shape
rectTile =
  rect (toFloat blockSize) (toFloat blockSize)

circleTile : Shape
circleTile =
  Collage.circle (toFloat blockSize / 2)

drawTile : Int -> Int -> Board.Color -> Form
drawTile x y color =
  rectTile
    |> setColor color
    |> setPos x y

setColor : Board.Color -> Shape -> Form
setColor color shape =
  shape |> filled (convertColor color)

setPos : Int -> Int -> Form -> Form
setPos x y shape =
  shape
    |> resetPos
    |> move (toFloat (x * blockSize), toFloat (y * blockSize))

convertColor : Board.Color -> Color.Color
convertColor color =
  case color of
    Board.Red ->    rgb 225  75  75
    Board.Blue ->   rgb  75  75 225
    Board.Yellow -> rgb 225 225  75
