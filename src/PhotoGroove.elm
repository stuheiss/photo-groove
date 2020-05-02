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
type alias Photo = { url : String }
type alias Model = { photos : List Photo, selectedUrl : String, chosenSize : ThumbnailSize }

photoArray : Array Photo
photoArray = Array.fromList initialModel.photos

urlPrefix : String
urlPrefix =
  "http://elm-in-action.com/"

initialModel : Model
initialModel =
  {
    photos =
      [ { url = "1.jpeg" }
      , { url = "2.jpeg" }
      , { url = "3.jpeg" }
      ]
  , selectedUrl = "1.jpeg"
  , chosenSize = Medium
  }

view : Model -> Html Msg
view model =
  div [ class "content" ]
      [ h1 [] [ text "Photo Groove" ]
      , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
      , h3 [] [ text "Thumbnail size:" ]
      , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
      , div [ id "thumbnails", class (sizeToClass model.chosenSize) ]
        (List.map (viewThumbnail model.selectedUrl) model.photos)
      , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ model.selectedUrl)
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

getPhotoUrl : Int -> String
getPhotoUrl index =
  case Array.get index photoArray of
    Just photo ->
      photo.url
    Nothing ->
      ""

randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
  Random.int 0 (Array.length photoArray - 1)

type Msg
  = ClickedPhoto String
  | ClickedSize ThumbnailSize
  | ClickedSurpriseMe
  | GotSelectedIndex Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotSelectedIndex index ->
      ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )
    ClickedPhoto url ->
      ( { model | selectedUrl = url }, Cmd.none )
    ClickedSurpriseMe ->
      ( model, Random.generate GotSelectedIndex randomPhotoPicker )
    ClickedSize size ->
      ( { model | chosenSize = size }, Cmd.none )

main : Program () Model Msg
main =
  Browser.element
    { init = \flags -> ( initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }