module PhotoGroove exposing (main)

import Browser
import Html exposing (div, h1, img, text, Html, button, input, h3, label)
import Html.Attributes exposing(..)
import Html.Events exposing (onClick)
import Random

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
    case model.status of
        Loaded photos selectedUrl -> 
            viewLoaded photos selectedUrl model.chosenSize
        Loading ->
            []
        Errored errorMessage ->
            [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button
    [ onClick ClickedSurpriseMe ]
    [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
    (List.map (viewThumbnail selectedUrl) photos)
    , img 
    [ class "large"
    , src (urlPrefix ++ "large/" ++ selectedUrl)
    ]
    []
    ]


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        Loading ->
            status
        Errored errorMessage ->
            status



viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img 
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ] 
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
    [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
    , text (sizeToString size)
    ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> 
            "small"
        Medium ->
            "med"
        Large ->
            "large"


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }
initialModel : Model
initialModel = 
    { status = Loading
    , chosenSize = Medium
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status =  selectUrl url model.status }, Cmd.none )
        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                    |> Random.generate GotRandomPhoto
                    |> Tuple.pair model
                Loaded [] _ ->
                    ( model, Cmd.none )
                Loading ->
                    ( model, Cmd.none )
                Errored errorMessage ->
                    ( model, Cmd.none )
        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )
        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )


main : Program () Model Msg
main = 
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }

