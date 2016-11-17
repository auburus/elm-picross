import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Json.Decode as Json
import Random
import Time

main = 
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Grid = Array (Array Status)
type alias Position =
  { row : Int
  , col : Int
  }
type alias Model =
  { grid : Grid
  , solution : Grid
  }

type Status
  = Undefined
  | Correct
  | Incorrect

init : (Model, Cmd Msg)
init =
  ( Model 
    (repeat 3 (repeat 3 Undefined))
    (repeat 3 (repeat 3 Undefined))
  , Random.generate InitSolution <| randomMatrix 3 3
  )



{-randomGrid : Int -> Int -> Grid
randomGrid rows cols =
  if rows == 0 || cols == 0
    then Array.empty
    else 
      Array.initialize rows
        ( \i ->
          Array.initialize cols
            ( \j -> 
              Random.map
                ( \b -> if b then Correct else Incorrect)
                Random.bool
            )
        )
        -}

-- UPDATE

type Msg
  = Click Int Int
  | RightClick Int Int
  | InitSolution Grid

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Click rowNum colNum ->
      ({ model | grid = (updateGrid leftClick (Position rowNum colNum) model.grid) }, Cmd.none)
    RightClick rowNum colNum -> 
      ( { model | grid = (updateGrid rightClick (Position rowNum colNum) model.grid) }, Cmd.none)
    InitSolution solution ->
      ({ model | solution = solution }, Cmd.none)


leftClick : Status -> Status
leftClick status =
  if status == Correct
    then Undefined
    else Correct

rightClick : Status -> Status
rightClick status =
  if status == Incorrect
    then Undefined
    else Incorrect


updateGrid : (Status -> Status) -> Position -> Grid -> Grid
updateGrid updateItem pos grid =
  Array.indexedMap (\i row ->
    if i /= pos.row 
       then row
       else Array.indexedMap (\j item -> 
         if j /= pos.col
            then item
            else updateItem item
       ) row
    ) grid

  
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model = 
  div
  [ style [("width", "300px"), ("background-color", "teal")]
  ]
  [ div []
    (printGrid model.grid)
  , div [ style [("height", "10px")]] []
  , div []
    (printGrid model.solution)
  ]

printGrid: Grid -> List (Html Msg)
printGrid grid =
  Array.toList (Array.indexedMap printRow grid)

printRow: Int -> Array Status -> Html Msg
printRow rowNum row =
  div [ class "row" ]
    (Array.toList (Array.indexedMap (printItem rowNum) row))

printItem : Int -> Int -> Status -> Html Msg
printItem rowNum colNum item = 
  div
    [ onClick (Click rowNum colNum)
    , onRightClick (RightClick rowNum colNum)
    , style [("cursor", "pointer"), ("display", "inline-block")]
    ]
    [ text (printStatus item)
    ]

printStatus : Status -> String
printStatus status =
  case status of
    Undefined   -> "0"
    Correct     -> "1"
    Incorrect   -> "-1"



-- HELPERS

rightClickOpt : Html.Events.Options
rightClickOpt =
  { stopPropagation = False
  , preventDefault = True
  }

onRightClick : msg -> Attribute msg
onRightClick message =
  onWithOptions "contextmenu" rightClickOpt (Json.succeed message)

-- RANDOM GENERATORS
randomStatus : Random.Generator Status
randomStatus =
  Random.map (\b -> if b then Correct else Incorrect) Random.bool

randomArray : Int -> Random.Generator (Array Status)
randomArray length = 
  Random.map Array.fromList <| Random.list length randomStatus

randomMatrix : Int -> Int -> Random.Generator (Grid)
randomMatrix rows cols =
  Random.map Array.fromList <| Random.list rows <| randomArray cols
