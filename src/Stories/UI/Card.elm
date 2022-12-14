module Stories.UI.Card exposing (..)

import Html exposing (Html)
import Storybook.Story exposing (Story)
import UI.Card


main : Story () msg
main =
    Storybook.Story.stateless
        { view = view
        }


view : Html msg
view =
    Html.div []
        [ UI.Card.view <| UI.Card.card [ Html.text "Text" ]
        ]
