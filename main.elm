import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Json.Decode as Json

main = 
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Grid = Array (Array Status)
type alias Model =
  { grid : Grid
  }

type Status
  = Undefined
  | Correct
  | Incorrect


init : (Model, Cmd Msg)
init =
  (Model (repeat 3 (repeat 3 Undefined)), Cmd.none)

-- UPDATE

type Msg
  = Click Int Int
  | RightClick Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Click rowNum colNum ->
      ({ model | grid = (updateGrid leftClick rowNum colNum model.grid) }, Cmd.none)
    RightClick rowNum colNum -> 
      ( { model | grid = (updateGrid rightClick rowNum colNum model.grid) }, Cmd.none)

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


-- Given an update function
updateGrid : (Status -> Status) -> Int -> Int -> Grid -> Grid
updateGrid updateItem rowNum colNum grid =
  Array.indexedMap (\i row ->
    if i /= rowNum 
       then row
       else Array.indexedMap (\j item -> 
         if j /= colNum
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
    (printGrid model.grid)

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
    , style [("cursor", "pointer")]
    ]
    [ text (printStatus item)
    ]

printStatus : Status -> String
printStatus status =
  case status of
    Undefined   -> "0"
    Correct     -> "1"
    Incorrect   -> "-1"


rightClickOpt : Html.Events.Options
rightClickOpt =
  { stopPropagation = False
  , preventDefault = True
  }


-- HELPERS

onRightClick : msg -> Attribute msg
onRightClick message =
  onWithOptions "contextmenu" rightClickOpt (Json.succeed message)

onMiddleClick : msg -> Attribute msg
