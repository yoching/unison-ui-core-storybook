module Stories.UI.Banner exposing (..)

import Html exposing (Html)
import Storybook.Story exposing (Story)
import Helper exposing (col)
import UI.Banner as B
import UI.Click as C


main : Story () Msg
main =
    Storybook.Story.stateless
        { view = view
        }

type Msg
    = UserClicked

elements: List (B.Banner Msg)
elements = [
    B.info "Info",
    B.promotion "Promotion ID" "Content" (C.ExternalHref "https://unison-lang.org") "Cta label"
    ]

view : Html Msg
view =
    (elements |> List.map (B.view))
    |> col []