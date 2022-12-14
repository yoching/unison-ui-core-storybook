module Stories.Icon.UnisonMark exposing (main)

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
    UI.Icon.view UI.Icon.unisonMark
