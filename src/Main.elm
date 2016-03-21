module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Http
import Task
import StartApp
import Time exposing (since, second)

-- ACTIONS

type Action
  = QueryUpdate String
  | ReceiveQueryResults (Maybe (List Photo))
  | NoOp
  | PerformQuery
  
-- MODEL

type alias Model =
  { query : String
  , results: List Photo 
  }

initialModel : Model
initialModel =
  { query = ""
  , results = [] 
  }

-- EFFECTS

type alias Photo = 
  { farm : Int
  , id : String
  , server : String
  , secret : String
  }

makeFlickrUrl : String -> String
makeFlickrUrl query = 
  let firstPart = "https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=883149b474e6f2ef29b56c0d2bd64db0&tags="
      secondPart = "&format=json&nojsoncallback=1&sort=relevance"
  in firstPart ++ query ++ secondPart

performQuery query =
  Http.getString (makeFlickrUrl query) 
  |> Task.map parsePhotos
  |> Task.toMaybe 
  |> Task.map ReceiveQueryResults
  |> Effects.task

safePhotoDecoder : Result String (List Photo) -> (List Photo)
safePhotoDecoder result = 
  case result of
    Err msg -> 
      let _ = Debug.log "error" msg
      in []
    Ok photos ->
      photos

parsePhotos jsonString = safePhotoDecoder (Json.decodeString photoDecoder jsonString)

photoDecoder : Json.Decoder (List Photo)
photoDecoder = 
  Json.at ["photos", "photo"] 
  <| Json.list 
  <| Json.object4 Photo ("farm" := Json.int) ("id" := Json.string) ("server" := Json.string) ("secret" := Json.string)

photoUrl : Photo -> String
photoUrl photo = "https://farm" ++ (toString photo.farm) ++ ".staticflickr.com/" ++ photo.server ++ "/" ++ photo.id ++ "_" ++ photo.secret ++ ".jpg"

-- UPDATE

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case (Debug.log "action" action) of 
    
    PerformQuery ->
      ( model, performQuery model.query )
    
    QueryUpdate query -> 
      ( { model | query = query }, Effects.none )
      
    ReceiveQueryResults results ->
      let result = Maybe.withDefault [] results
      in ( { model | results = result }, Effects.none )

    NoOp -> ( model, Effects.none )

-- VIEW

searchBar : Signal.Address Action -> Model -> Html
searchBar address model =
  input 
  [ placeholder "Search query..."
  , type' "text"
  , autofocus True
  , value model.query
  , on "input" targetValue (Signal.message address << QueryUpdate)
  , class "col col-10 field rounded-left x-group-item"
  ]
  []

searchButton : Signal.Address Action -> Model -> Html
searchButton address model = 
  button 
  [ type' "submit"
  , onClick address PerformQuery 
  , class "col col-2 btn btn-primary rounded-right"
  ]
  [ text "Search" ]

searchForm : Signal.Address Action -> Model -> Html
searchForm address model = 
  Html.form 
  [ class "block clearfix"
  , onWithOptions 
      "submit" 
      { stopPropagation = True, preventDefault = True } 
      Json.value 
      (\_ -> Signal.message address PerformQuery)
  ]
  [ (searchBar address model)
  , (searchButton address model)
  ]

viewPhoto : Photo -> Html
viewPhoto photo = 
  img 
  [ src (photoUrl photo)
  , width 150
  , height 150 
  ] 
  []

resultsBox : Signal.Address Action -> Model -> Html
resultsBox address model =
  div []
      ( List.map viewPhoto model.results )

view : Signal.Address Action -> Model -> Html
view address model =
  let _ = Debug.log "model" model
  in 
    div
    []
    [ (searchForm address model)
    , (resultsBox address model)
    ]

-- START APP

init : ( Model, Effects Action )
init =
  ( initialModel, Effects.none )

app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , inputs = []
    , update = update
    , view = view
    }

main : Signal.Signal Html
main =
  app.html

port runner : Signal (Task.Task Never ())
port runner =
  app.tasks
