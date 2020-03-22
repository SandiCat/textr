module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (Element)
import Element.Events as Events
import Generated.Api as Api
import Generated.Gender as Gender
import Generated.Login as Login exposing (Login)
import Generated.Post as Post exposing (Post)
import Generated.Sex as Sex
import Generated.UserID as UserID exposing (UserID)
import Html.Attributes
import Http
import Ionicon
import Task
import Util


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { post : Maybe Post
    , windowSize :
        { height : Int
        , width : Int
        }
    , loginStatus : LoginStatus
    }


type LoginStatus
    = NotAsked
    | Failure Http.Error
    | Success


loginResultToMsg result =
    case result of
        Err ( httpError, _ ) ->
            LoginComplete <| Just httpError

        Ok _ ->
            LoginComplete Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    ( { post = Nothing
      , windowSize =
            { height = 100
            , width = 100
            }
      , loginStatus = NotAsked
      }
    , Cmd.batch
        [ UserID.UserID 1
            |> Login
            |> Api.postLogin
            |> Cmd.map loginResultToMsg
        , Task.perform
            (\vp ->
                Resize
                    (floor vp.viewport.width)
                    (floor vp.viewport.height)
            )
            Browser.Dom.getViewport
        ]
    )


type Msg
    = GotPost (Maybe Post)
    | NextPost
    | Resize Int Int
    | LoginComplete (Maybe Http.Error)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPost maybePost ->
            ( { model | post = maybePost }
            , Cmd.none
            )

        NextPost ->
            ( { model | post = Nothing }
            , Cmd.map (Result.toMaybe >> Util.joinMaybe >> GotPost) Api.getNext_post
            )

        Resize w h ->
            ( { model | windowSize = { width = w, height = h } }
            , Cmd.none
            )

        LoginComplete (Just error) ->
            ( { model | loginStatus = Failure error }
            , Cmd.none
            )

        LoginComplete Nothing ->
            ( { model | loginStatus = Success }
            , Cmd.map (Result.toMaybe >> Util.joinMaybe >> GotPost) Api.getNext_post
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Timesafe"
    , body =
        [ Element.layout []
            (Element.row
                [ Element.px model.windowSize.width
                    |> Element.maximum 600
                    |> Element.width
                , Element.px model.windowSize.height
                    |> Element.maximum 600
                    |> Element.height
                , Element.explain Debug.todo
                , Element.centerX
                ]
                [ Element.column []
                    [ viewMenu
                    , case model.loginStatus of
                        Failure error ->
                            Element.text <| Debug.toString error

                        Success ->
                            Element.text "logged in, somehow!"

                        NotAsked ->
                            Element.text "didnt ask yet"
                    ]
                , case model.post of
                    Just post ->
                        Element.el
                            [ Element.width Element.fill
                            , Element.fill
                                |> Element.maximum 900
                                |> Element.height
                            ]
                            (Element.row
                                [ Element.centerX
                                , Element.height Element.fill
                                , Element.width Element.fill

                                -- this is required to get the scrollbar to work https://discourse.elm-lang.org/t/elm-ui-parent-element-grows-to-encompass-children-instead-of-scrolling/5032/5
                                , Element.clip
                                , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
                                ]
                                [ viewPost post
                                , Element.column
                                    -- VOTE
                                    [ Element.width <| Element.px 50
                                    , Element.centerY
                                    , Events.onClick NextPost
                                    ]
                                    [ Util.defaultIcon Ionicon.arrowUpA
                                    , Util.defaultIcon Ionicon.arrowDownA
                                    ]
                                ]
                            )

                    Nothing ->
                        Element.el
                            [ Element.width Element.fill
                            ]
                            (Element.el [ Element.centerX ]
                                (Element.text "~ dust ~")
                            )
                ]
            )
        ]
    }


viewMenu : Element msg
viewMenu =
    Element.column
        [ Element.alignLeft
        , Element.centerY
        ]
        [ Element.text "Browse"
        , Element.text "Create post"
        , Element.text "Settings"
        , Element.text "About"
        ]


viewPost : Post -> Element msg
viewPost post =
    Element.column
        [ Element.explain Debug.todo
        , Element.height Element.fill
        , Element.fill
            |> Element.maximum 600
            |> Element.width
        ]
        [ Element.el
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.scrollbarY
            ]
            (Element.paragraph []
                [ Element.text post.body ]
            )
        , Element.row
            [ Element.alignBottom
            , Element.height <| Element.px 75
            , Element.width Element.fill
            , Element.padding 5
            ]
            [ Element.text post.nickname
            , Element.column [ Element.alignRight ]
                [ Element.text <| String.fromInt <| post.age
                , Element.text <| viewGender post.gender
                ]
            ]
        ]


viewGender : Gender.Gender -> String
viewGender gender =
    case gender of
        Gender.Cis Sex.Male ->
            "Male"

        Gender.Cis Sex.Female ->
            "Female"

        Gender.TransTo Sex.Male ->
            "FTM"

        Gender.TransTo Sex.Female ->
            "MTF"

        Gender.Other (Just genderName) ->
            genderName

        Gender.Other Nothing ->
            "N/A"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize Resize
