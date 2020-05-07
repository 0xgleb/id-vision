module Main exposing (..)

import Browser
import Date
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { personalDetails : Maybe PersonalDetails
    , error : Maybe Http.Error
    }


type alias PersonalDetails =
    { surname : Maybe String
    , givenNames : Maybe String
    , dateOfBirth : Maybe Date.Date
    , documentType : DocumentType
    }


type DocumentType
    = NationalIdentityCard
    | Unknown


documentToString : DocumentType -> String
documentToString docType =
    case docType of
        NationalIdentityCard ->
            "National Identity Card"

        Unknown ->
            "Unknown"


personalDetailsDecoder : D.Decoder PersonalDetails
personalDetailsDecoder =
    D.map4 PersonalDetails
        (D.field "surname" <| D.nullable D.string)
        (D.field "givenNames" <| D.nullable D.string)
        (D.field "dateOfBirth" <| D.nullable dateDecoder)
        (D.field "documentType" <| documentTypeDecoder)


dateDecoder : D.Decoder Date.Date
dateDecoder =
    D.andThen
        (\string ->
            case Date.fromIsoString string of
                Ok date ->
                    D.succeed date

                Err error ->
                    D.fail error
        )
        D.string


documentTypeDecoder : D.Decoder DocumentType
documentTypeDecoder =
    D.map
        (\string ->
            case string of
                "NationalIdentityCard" ->
                    NationalIdentityCard

                _ ->
                    Unknown
        )
        D.string


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Nothing, Cmd.none )



-- UPDATE


type Msg
    = ImageRequested
    | ImageSelected File
    | GotAnalysisResponse (Result Http.Error PersonalDetails)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageRequested ->
            ( model
            , Select.file [ "image/png", "image/jpg" ] ImageSelected
            )

        ImageSelected file ->
            ( { model | personalDetails = Nothing }
            , extractData file
            )

        GotAnalysisResponse result ->
            case result of
                Ok personalDetails ->
                    ( { model | personalDetails = Just personalDetails }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = Just error }
                    , Cmd.none
                    )



-- Commands


extractData : File.File -> Cmd Msg
extractData file =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/"
        , body = Http.multipartBody [ Http.filePart "image" file ]
        , expect = Http.expectJson GotAnalysisResponse personalDetailsDecoder
        , timeout = Just 60000
        , tracker = Nothing
        }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ImageRequested ] [ text "Choose file" ]
        , viewPersonalDetails model.personalDetails
        ]


viewPersonalDetails : Maybe PersonalDetails -> Html Msg
viewPersonalDetails dets =
    let
        maybeShow prefix maybeText =
            case maybeText of
                Just justText ->
                    [ p [] [ text <| String.append prefix justText ] ]

                Nothing ->
                    []
    in
    div [] <|
        case dets of
            Just personalDetails ->
                List.concat
                    [ maybeShow "Surname: " personalDetails.surname
                    , maybeShow "Given names: " personalDetails.givenNames
                    , maybeShow "Date of birth: " (Maybe.map Date.toIsoString personalDetails.dateOfBirth)
                    , [ p []
                            [ text <|
                                String.append "Document type: " <|
                                    documentToString personalDetails.documentType
                            ]
                      ]
                    ]

            Nothing ->
                []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
