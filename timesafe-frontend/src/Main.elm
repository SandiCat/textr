module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Element
import Generated.Api as Api
import Generated.Fruit as Fruit
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
    { fruits : List Fruit.Fruit
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [], Cmd.map (Result.toMaybe >> GotFruits) Api.getFruits )


type Msg
    = GotFruits (Maybe (List Fruit.Fruit))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFruits (Just fruits) ->
            ( { model | fruits = fruits }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Timesafe"
    , body =
        Element.column [ Element.padding 20, Element.spacing 10 ]
            (List.map
                (\fruit ->
                    Element.row [ Element.spacing 20, Element.width Element.fill ]
                        [ Element.text fruit.name
                        , fruit.sugarContent
                            |> Maybe.map String.fromFloat
                            |> Maybe.withDefault "N/A"
                            |> Element.text
                            |> Element.el [ Element.alignRight ]
                        ]
                )
                model.fruits
            )
            |> Element.layout [ ]
            |> List.singleton
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
