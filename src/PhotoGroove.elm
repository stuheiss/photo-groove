module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, id, name, src, title, type_)
import Html.Events exposing (onClick)
import Random
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)

type ThumbnailSize
  = Small
  | Medium
  | Large

type Status
  = Loading
  | Loaded (List Photo) String
  | Errored String

type alias Photo =
  { url : String
  , size : Int
  , title : String
  }

photoDecoder : Decoder Photo
photoDecoder =
  succeed Photo
    |> required "url" string
    |> required "size" int
    |> optional "title" string "(untitled)"

buildPhoto : String -> Int -> String -> Photo
buildPhoto url size title =
  { url = url, size = size, title = title }

type alias Model =
  {  status : Status
  ,  chosenSize : ThumbnailSize
  }

urlPrefix : String
urlPrefix =
  "http://elm-in-action.com/"

initialModel : Model
initialModel =
  { status = Loading
  , chosenSize = Medium
  }

initialCmd : Cmd Msg
initialCmd =
  Http.get
    { url = "http://elm-in-action.com/photos/list.json"
    , expect = Http.expectJson GotPhotos (list photoDecoder)
    }

view : Model -> Html Msg
view model =
  div [ class "content" ] <|
    case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model.chosenSize
      Loading ->
        [ text "Loading..." ]
      Errored errorMessage ->
        [ text ("Error: " ++ errorMessage)]

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
      [ h1 [] [ text "Photo Groove" ]
      , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
      , h3 [] [ text "Thumbnail size:" ]
      , div [ id "choose-size" ]
        (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
      , div [ id "thumbnails", class (sizeToClass chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
      , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
      ]

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
      [ src (urlPrefix ++ thumb.url)
      , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
      , classList [ ( "selected", selectedUrl == thumb.url ) ]
      , onClick (ClickedPhoto thumb.url)
      ]
      []

sizeToClass : ThumbnailSize -> String
sizeToClass size = sizeToString size

sizeToString : ThumbnailSize -> String
sizeToString size =
  case size of
    Small ->
      "small"
    Medium ->
      "med"
    Large ->
      "large"

viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
  label []
    [ input [ type_ "radio", name "size", checked (chosenSize == size), onClick (ClickedSize size) ] []
    , text (sizeToString size)
    ]

type Msg
  = ClickedPhoto String
  | ClickedSize ThumbnailSize
  | ClickedSurpriseMe
  | GotRandomPhoto Photo
  | GotPhotos (Result Http.Error (List Photo))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotPhotos (Ok photos) ->
      case photos of
        first :: rest ->
          ( { model | status = Loaded photos first.url }, Cmd.none )
        [] ->
          ( { model | status = Errored "0 photos found" }, Cmd.none )
    GotPhotos (Err _) ->
      ( { model | status = Errored "Server error!" }, Cmd.none )
    GotRandomPhoto photo ->
      ( { model | status = selectUrl photo.url model.status }, Cmd.none )
    ClickedPhoto url ->
      ( { model | status = selectUrl url model.status }, Cmd.none )
    ClickedSurpriseMe ->
      case model.status of
        Loaded (firstPhoto::otherPhotos) _ ->
          Tuple.pair model <|
            Random.generate GotRandomPhoto <|
            Random.uniform firstPhoto otherPhotos
        Loading ->
          ( model, Cmd.none )
        Errored _ ->
          ( model, Cmd.none )
        Loaded [] _ ->
          ( model, Cmd.none )
    ClickedSize size ->
      ( { model | chosenSize = size }, Cmd.none )

selectUrl : String -> Status -> Status
selectUrl url status =
  case status of
    Loaded photos _ ->
      Loaded photos url
    Loading ->
      status
    Errored _ ->
      status

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( initialModel, initialCmd )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
