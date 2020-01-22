module ElmUI exposing (..)

import Element exposing (..)
import Html exposing (Html)


main : Html a
main =
    layout []
        (row
            [ width <| px 500
            , height <| px 500
            ]
            [ row
                [ width fill
                , height fill
                ]
                [ row
                    [ width fill
                    , height fill
                    , explain Debug.todo
                    ]
                    [ el
                        [ width fill
                        , height fill
                        , explain Debug.todo
                        , scrollbarX
                        ]
                        (paragraph [] [ text longBoi ])
                    , el [] <| text "hey"
                    ]
                ]
            ]
        )


longBoi =
    """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur vel est ultricies, venenatis ex eget, fermentum ipsum. Donec nec elementum nulla. Nam egestas dignissim nisi eu tincidunt. Quisque ac lectus porttitor, ornare eros a, ultricies ligula. Sed a dignissim orci, sed consequat lacus. Pellentesque rutrum nibh eu iaculis iaculis. Nam non lorem sit amet leo eleifend efficitur quis sit amet neque. Vestibulum fermentum interdum ante nec gravida. Vestibulum malesuada lorem ac libero lacinia luctus. Vivamus volutpat, est at commodo pretium, velit enim dictum turpis, ut ornare lacus ante a libero. Vestibulum volutpat, mauris elementum aliquet pharetra, magna sem sodales dui, eu convallis enim nunc non justo. Curabitur pretium dictum arcu, eget efficitur mauris aliquet ac. In dolor dui, aliquet at placerat a, ultrices sed sem. Phasellus sit amet semper eros, ac consequat felis.

Donec sodales ligula purus, sit amet luctus felis dignissim fringilla. Nullam accumsan sollicitudin hendrerit. Nulla viverra sit amet leo id congue. Maecenas semper pharetra justo. Integer congue placerat ipsum, vitae finibus ante vehicula sed. Nulla facilisi. Vivamus ullamcorper aliquam ipsum, sit amet elementum nulla lobortis sed. Curabitur nisi justo, malesuada quis tortor ac, tincidunt vulputate justo. Cras commodo dui et leo tincidunt, cursus vestibulum nulla egestas. Etiam sit amet rutrum eros, mollis iaculis felis. Mauris vulputate dolor eu justo ornare sodales. Suspendisse gravida porta augue, ac dignissim nisl. Maecenas dictum odio risus. Cras sagittis justo neque, vitae euismod turpis commodo eu.

Fusce eleifend quam ut magna dignissim pretium. Praesent sed luctus ante. Mauris viverra enim a aliquam placerat. Maecenas tincidunt aliquam tortor sed volutpat. Duis hendrerit egestas dapibus. Quisque quis consectetur magna. Etiam risus nibh, consequat sed massa at, finibus accumsan turpis. Suspendisse id elit sed nisi scelerisque placerat. Fusce enim ligula, lacinia ac fermentum quis, pretium vitae purus. Mauris velit neque, euismod quis accumsan vel, elementum a velit. Suspendisse in tristique ipsum. Maecenas sapien dolor, vestibulum sed velit sit amet, luctus sollicitudin mi. Donec sapien est, ornare a bibendum at, cursus ut purus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas.

Nam convallis convallis felis at aliquet. Nunc elit ex, sollicitudin at nibh vitae, eleifend porttitor ex. Interdum et malesuada fames ac ante ipsum primis in faucibus. Pellentesque id turpis nisl. Curabitur pellentesque sit amet eros eget imperdiet. Curabitur ipsum nulla, facilisis et suscipit vitae, lobortis ultrices metus. Vestibulum ac ornare purus. Curabitur tempor congue massa tempus semper. Nullam ut enim quis mi tempus sollicitudin. Mauris mollis ante vitae orci ultricies interdum. Nullam quis dolor varius, interdum nibh sit amet, pellentesque justo. Phasellus at libero faucibus, vestibulum sapien id, pellentesque dolor. Fusce vitae ante iaculis, lacinia turpis vel, posuere ex. In ultrices ullamcorper fermentum. Donec rutrum vestibulum nisi quis semper. Aliquam ante massa, mollis et accumsan a, convallis in ipsum.
"""
