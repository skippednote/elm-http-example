module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type', placeholder, src)
import Html.App as App
import Http
import Json.Decode as Json exposing ((:=))
import Task
import List


-- App


main =
    App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



--- Model


type alias RedditPost =
    { title : String
    }


type alias Model =
    { subreddit : String
    , response : List RedditPost
    , error : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [] "", Cmd.none )



-- Update


type Msg
    = InputUpdate String
    | Get
    | FetchPass (List RedditPost)
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUpdate val ->
            ( { model | subreddit = val }, Cmd.none )

        Get ->
            ( model, getSubReddit model.subreddit )

        FetchPass response ->
            ( { model | response = response }, Cmd.none )

        FetchFail err ->
            case err of
                Http.BadResponse code message ->
                    ( { model | error = message }, Cmd.none )

                _ ->
                    ( { model
                        | error = "Failed"
                        , response = []
                      }
                    , Cmd.none
                    )



-- View


view : Model -> Html Msg
view model =
    div []
        [ input [ type' "text", onInput InputUpdate, placeholder "SubReddit" ] []
        , button [ onClick Get ] [ text "Go" ]
        , p [] [ text model.error ]
        , ul [] (List.map subbreddit model.response)
        ]


subbreddit response =
    li []
        [ h3 [] [ text response.title ]
        ]



-- HTTP


getSubReddit : String -> Cmd Msg
getSubReddit subreddit =
    let
        url =
            "https://www.reddit.com/r/" ++ subreddit ++ ".json"
    in
        Task.perform FetchFail FetchPass (Http.get decodeRedditPost url)


decodeRedditPost =
    Json.at [ "data", "children" ] decodeList


decodeList =
    Json.list decodeSubReddit


decodeSubReddit =
    Json.at [ "data" ] decodeInnerData


decodeInnerData =
    Json.object1 RedditPost
        ("title" := Json.string)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
