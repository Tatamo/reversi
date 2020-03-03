module Reversi

type Color =
    | Black
    | White

type Disk = Color option

let swap color =
    match color with
    | Black -> White
    | White -> Black

type Board = Disk [,]

let initPlacer x y =
    match (x, y) with
    | (3, 3)
    | (4, 4) -> Some(White)
    | (3, 4)
    | (4, 3) -> Some(Black)
    | _ -> None

let initBoard: Board = Array2D.init 8 8 initPlacer

type GameStatus =
    | InGame of Color
    | GameEnd of Color

type Game = Board * GameStatus

let initGame: Game = initBoard, InGame Black
