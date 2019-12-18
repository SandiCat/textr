module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Element
import Http


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { poem : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "", requestPoem )

requestPoem : Cmd Msg
requestPoem =
    Http.get
        { url = "/static/doc1.txt"
        , expect = Http.expectString GotPoem
        }


type Msg
    = GotPoem (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPoem (Ok poem) ->
            ( { model | poem = poem }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Timesafe"
    , body =
        Element.paragraph []
            [ Element.text model.poem ]
            |> Element.layout []
            |> List.singleton
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
