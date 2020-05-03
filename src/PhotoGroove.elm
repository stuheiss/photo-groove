module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random

type ThumbnailSize
  = Small
  | Medium
  | Large

type Status
  = Loading
  | Loaded (List Photo) String
  | Errored String

type alias Photo = { url : String }

type alias Model = { status : Status, chosenSize : ThumbnailSize }

urlPrefix : String
urlPrefix =
  "http://elm-in-action.com/"

initialModel : Model
initialModel =
  { status = Loading
  , chosenSize = Medium
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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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
        Errored erorMessage ->
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
    Errored errorMessage ->
      status

main : Program () Model Msg
main =
  Browser.element
    { init = \flags -> ( initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }
