port module PhotoGroove exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (checked, class, classList, id, name, src, title, type_)
import Html.Events exposing (on, onClick)
import Random
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode

type ThumbnailSize
  = Small
  | Medium
  | Large

type Status
  = Loading
  | Loaded (List Photo) String
  | Errored String

port setFilters : FilterOptions -> Cmd msg

port activityChanges : (String -> msg) -> Sub msg

type alias FilterOptions =
  { url : String
  , filters : List { name : String, amount : Float }
  }

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
  { status : Status
  , activity : String
  , chosenSize : ThumbnailSize
  , hue : Int
  , ripple : Int
  , noise : Int
  , sparkle : Int
  }

urlPrefix : String
urlPrefix =
  "http://elm-in-action.com/"

initialModel : Model
initialModel =
  { status = Loading
  , activity = ""
  , chosenSize = Medium
  , hue = 0
  , ripple = 0
  , noise = 0
  , sparkle = 0
  }

initialCmd : Cmd Msg
initialCmd =
  Http.get
    { url = urlPrefix ++ "photos/list.json"
    , expect = Http.expectJson GotPhotos (list photoDecoder)
    }

view : Model -> Html Msg
view model =
  div [ class "content" ] <|
    case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model
      Loading ->
        [ text "Loading..." ]
      Errored errorMessage ->
        [ text ("Error: " ++ errorMessage)]

viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
  div [ class "filter-slider" ]
    [ label [] [ text name ]
    , rangeSlider
      [ Attr.max "11"
      , Attr.property "val" (Encode.int magnitude)
      , onSlide toMsg
      ]
      []
    , label [] [ text (String.fromInt magnitude) ]
    ]

viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
  [ h1 [] [ text "Photo Groove" ]
  , button
    [ onClick ClickedSurpriseMe ]
    [ text "Surprise Me!" ]
  , div [ class "activity" ] [ text model.activity ]
  , div [ class "filters" ]
    [ viewFilter SlidHue "Hue" model.hue
    , viewFilter SlidRipple "Ripple" model.ripple
    , viewFilter SlidNoise "Noise" model.noise
    , viewFilter SlidSparkle "Sparkle" model.sparkle
    ]
  , h3 [] [ text "Thumbnail size:" ]
  , div [ id "choose-size" ]
    (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
  , div [ id "thumbnails", class (sizeToClass model.chosenSize) ]
    (List.map (viewThumbnail selectedUrl) photos)
  , canvas [ id "main-canvas", class "large" ] []
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
  | GotActivity String
  | GotPhotos (Result Http.Error (List Photo))
  | SlidHue Int
  | SlidRipple Int
  | SlidNoise Int
  | SlidSparkle Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotActivity activity ->
      ( { model | activity = activity }, Cmd.none )
    GotPhotos (Ok photos) ->
      case photos of
        first :: rest ->
          applyFilters { model | status = Loaded photos first.url }
        [] ->
          ( { model | status = Errored "0 photos found" }, Cmd.none )
    GotPhotos (Err _) ->
      ( { model | status = Errored "Server error!" }, Cmd.none )
    GotRandomPhoto photo ->
      applyFilters { model | status = selectUrl photo.url model.status }
    ClickedPhoto url ->
      applyFilters { model | status = selectUrl url model.status }
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
    SlidHue hue ->
      applyFilters { model | hue = hue }
    SlidRipple ripple ->
      applyFilters { model | ripple = ripple }
    SlidNoise noise ->
      applyFilters { model | noise = noise }
    SlidSparkle sparkle ->
      applyFilters { model | sparkle = sparkle }

applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
  case model.status of
    Loaded photos selectedUrl ->
      let
        filters =
          [ { name = "Hue", amount = toFloat model.hue / 11 }
          , { name = "Ripple", amount = toFloat model.ripple / 11 }
          , { name = "Noise", amount = toFloat model.noise / 11 }
          , { name = "Sparkle", amount = toFloat model.sparkle / 11 }
          ]
        url =
          urlPrefix ++ "large/" ++ selectedUrl
      in
      ( model, setFilters { url = url, filters = filters } )
    Loading ->
      ( model, Cmd.none )
    Errored errorMessage ->
      ( model, Cmd.none )

selectUrl : String -> Status -> Status
selectUrl url status =
  case status of
    Loaded photos _ ->
      Loaded photos url
    Loading ->
      status
    Errored _ ->
      status

main : Program Float Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Float -> ( Model, Cmd Msg )
init flags =
  let
    activity = "Initializing Pasta v" ++ String.fromFloat flags
  in
    ( { initialModel | activity = activity }, initialCmd )

subscriptions : Model -> Sub Msg
subscriptions _ =
  activityChanges GotActivity

rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
  node "range-slider" attributes children

onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
  at [ "detail", "userSlidTo" ] int
    |> Json.Decode.map toMsg
    |> on "slide"
