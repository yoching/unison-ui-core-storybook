module Stories.UI.StatusBanner exposing (..)

import Html exposing (Html)
import Storybook.Story exposing (Story)
import UI.StatusBanner


main : Story () msg
main =
    Storybook.Story.stateless
        { view = view
        }


view : Html msg
view =
    Html.div []
        [ UI.StatusBanner.bad "Bad"
        , UI.StatusBanner.good "Good"
        , UI.StatusBanner.info "Info"
        , UI.StatusBanner.working "Working"
        ]
