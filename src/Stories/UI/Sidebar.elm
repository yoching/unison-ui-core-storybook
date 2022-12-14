module Stories.UI.Sidebar exposing (..)

import Html exposing (Html)
import Storybook.Story exposing (Story)
import UI.Sidebar


main : Story () msg
main =
    Storybook.Story.stateless
        { view = view
        }


view : Html msg
view =
    sideBar


sideBar : Html msg
sideBar =
    UI.Sidebar.viewSection
        { title = "This is sidebar"
        , titleButton = Nothing
        , content = []
        , scrollable = False
        }
