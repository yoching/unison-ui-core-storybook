module Stories.UI.Button exposing (..)

import Html exposing (Html)
import Storybook.Story exposing (Story)
import UI.Button


main : Story () Msg
main =
    Storybook.Story.stateless
        { view = view
        }

type Msg
    = UserClicked

view : Html Msg
view =
    Html.div [] [
        UI.Button.github "Some text"
            |> UI.Button.view,
        UI.Button.button UserClicked "Some button" 
            |> UI.Button.active
            |> UI.Button.view,
        UI.Button.button UserClicked "Some button" 
            |> UI.Button.decorativeBlue
            |> UI.Button.large
            |> UI.Button.view,
        UI.Button.button UserClicked "Some button" 
            |> UI.Button.decorativeBlue
            |> UI.Button.medium
            |> UI.Button.view,
        UI.Button.button UserClicked "Some button" 
            |> UI.Button.critical
            |> UI.Button.small
            |> UI.Button.view
    ]