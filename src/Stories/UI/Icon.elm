module Stories.UI.Icon exposing (main)

import Html exposing (Html)
import Storybook.Story exposing (Story)
import UI.Icon


main : Story () msg
main =
    Storybook.Story.stateless
        { view = view
        }


view : Html msg
view =
    Html.div []
        [ UI.Icon.view UI.Icon.unisonMark
        , UI.Icon.view UI.Icon.patch
        , UI.Icon.view UI.Icon.dataConstructor
        , UI.Icon.view UI.Icon.abilityConstructor
        , UI.Icon.view UI.Icon.ability
        , UI.Icon.view UI.Icon.test
        , UI.Icon.view UI.Icon.doc
        , UI.Icon.view UI.Icon.docs
        , UI.Icon.view UI.Icon.term
        , UI.Icon.view UI.Icon.type_
        , UI.Icon.view UI.Icon.search
        , UI.Icon.view UI.Icon.caretDown
        ]
