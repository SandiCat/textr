module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (Element)
import Element.Events as Events
import Generated.Api as Api
import Generated.Post as Post exposing (Post)
import Html.Attributes
import Http
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { posts : List Post
    , windowSize :
        { height : Int
        , width : Int
        }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = []
      , windowSize =
            { height = 100
            , width = 100
            }
      }
    , Cmd.batch
        [ Cmd.map (Result.toMaybe >> GotPosts) Api.getPosts
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
    = GotPosts (Maybe (List Post))
    | NextPost
    | Resize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPosts maybePosts ->
            ( { model | posts = Maybe.withDefault [] maybePosts }
            , Cmd.none
            )

        NextPost ->
            ( { model
                | posts = List.tail model.posts |> Maybe.withDefault []
              }
            , Cmd.none
            )

        Resize w h ->
            ( { model | windowSize = { width = w, height = h } }
            , Cmd.none
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
                [ Element.column
                    -- MENU
                    [ Element.alignLeft
                    , Element.centerY
                    ]
                    [ Element.text "Browse"
                    , Element.text "Create post"
                    , Element.text "Settings"
                    , Element.text "About."
                    ]
                , case model.posts of
                    post :: _ ->
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
                                ]
                                [ viewPost post
                                , Element.column
                                    -- VOTE
                                    [ Element.width <| Element.px 50
                                    , Element.explain Debug.todo
                                    , Element.centerY
                                    , Events.onClick NextPost
                                    ]
                                    [ Element.text "^"
                                    , Element.text "v"
                                    ]
                                ]
                            )

                    [] ->
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


viewPost : Post -> Element msg
viewPost post =
    Element.column
        [ Element.explain Debug.todo
        , Element.height Element.fill
        , Element.fill
            |> Element.maximum 600
            |> Element.width

        -- , Element.height <| Element.px 400
        -- , Element.height <| Element.px 400
        ]
        [ Element.el
            [ Element.clip
            , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            , Element.height Element.fill
            , Element.width Element.fill
            ]
            (Element.el
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.scrollbarY
                ]
                (Element.paragraph []
                    [ Element.text post.body ]
                )
            )
        , Element.row
            [ Element.alignBottom
            , Element.height <| Element.px 75
            ]
            [ Element.text post.nickname
            , Element.text "22M4R"
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize Resize
