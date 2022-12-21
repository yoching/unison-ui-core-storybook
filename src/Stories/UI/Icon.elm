module Stories.UI.Icon exposing (..)

import Html exposing (Html)
import Storybook.Story exposing (Story)
import Helper exposing (col)
import UI.Icon as I


main : Story () Msg
main =
    Storybook.Story.stateless
        { view = view
        }

type Msg
    = UserClicked

icons: List (I.Icon msg)
icons = [
    I.unisonMark,
    I.patch,
    I.dataConstructor,
    I.abilityConstructor,
    I.ability,
    I.test,
    I.doc,
    I.docs,
    I.term,
    I.type_,
    I.search,
    I.caretDown,
    I.caretLeft,
    I.caretRight,
    I.caretUp,
    I.arrowDown,
    I.arrowLeft,
    I.arrowRight,
    I.arrowUp,
    I.arrowLeftUp,
    I.checkmark,
    I.chevronDown,
    I.chevronUp,
    I.chevronLeft,
    I.chevronRight,
    I.browse,
    I.folder,
    I.folderOutlined,
    I.intoFolder,
    I.hash,
    I.plus,
    I.warn,
    I.x,
    I.dot,
    I.largeDot,
    I.dots,
    I.dash,
    I.github,
    I.twitter,
    I.slack,
    I.download,
    I.upload,
    I.list,
    I.tags,
    I.tagsOutlined,
    I.clipboard,
    I.user,
    I.cog,
    I.chest,
    I.pencilRuler,
    I.exitDoor,
    I.documentCertificate,
    I.certificate,
    I.leftSidebarOn,
    I.leftSidebarOff,
    I.bulb,
    I.heart,
    I.heartOutline,
    I.star,
    I.starOutline,
    I.rocket,
    I.eye,
    I.eyeSlash,
    I.unfoldedMap,
    I.padlock,
    I.bug,
    I.tada,
    I.cloud,
    I.questionmark,
    I.keyboardKey,
    I.presentation,
    I.presentationSlash,
    I.box,
    I.wireframeGlobe,
    I.mapPin,
    I.mail,
    I.graduationCap
    ]

view : Html Msg
view =
    (icons |> List.map (I.view))
    |> col []