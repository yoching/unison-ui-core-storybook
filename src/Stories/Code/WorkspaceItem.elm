module Stories.Code.WorkspaceItem exposing (..)

import Code.Definition.Source as S
import Code.Syntax exposing (..)
import Code.Workspace.WorkspaceItem exposing (Item, Msg, WorkspaceItem(..), decodeList, fromItem)
import Dict
import Helper exposing (col)
import Html exposing (Html)
import Json.Decode exposing (decodeString)
import ReferenceHelper exposing (sampleReference)
import Storybook.Story exposing (Story)
import UI.Click
import UI.ViewMode


main : Story () Msg
main =
    Storybook.Story.stateless
        { view = view
        }


view : Html Msg
view =
    let
        decodeResult : Result Json.Decode.Error (List Item)
        decodeResult =
            let
                result =
                    decodeString (decodeList sampleReference) incrementGetDefinitionResponse
            in
            result
    in
    case decodeResult of
        Err error ->
            col [] [ Html.text (Json.Decode.errorToString error) ]

        Ok source ->
            List.map
                (\item ->
                    item |> viewItem
                )
                source
                |> col []


viewItem : Item -> Html Code.Workspace.WorkspaceItem.Msg
viewItem item =
    let
        workspaceItem =
            fromItem sampleReference item
    in
    Code.Workspace.WorkspaceItem.view { activeTooltip = Nothing, summaries = Dict.empty } UI.ViewMode.Regular workspaceItem True


incrementGetDefinitionResponse : String
incrementGetDefinitionResponse =
    """
    {
        "termDefinitions": {
            "#t7g50rohbm1c45qnvv2fiaupft4qqoduakqhj6k8fcmh1n75d4spgma4gvu1r6ip0nbn8dhv5vue4imeopnug13rnooft0abqq3uqgg": {
                "termNames": [
                    "increment"
                ],
                "bestTermName": "increment",
                "defnTermTag": "Plain",
                "termDefinition": {
                    "tag": "UserObject",
                    "contents": [
                        {
                            "annotation": {
                                "contents": "increment",
                                "tag": "HashQualifier"
                            },
                            "segment": "increment"
                        },
                        {
                            "annotation": {
                                "tag": "TypeAscriptionColon"
                            },
                            "segment": " :"
                        },
                        {
                            "annotation": null,
                            "segment": " "
                        },
                        {
                            "annotation": {
                                "contents": "##Nat",
                                "tag": "TypeReference"
                            },
                            "segment": "Nat"
                        },
                        {
                            "annotation": null,
                            "segment": " "
                        },
                        {
                            "annotation": {
                                "tag": "TypeOperator"
                            },
                            "segment": "->"
                        },
                        {
                            "annotation": null,
                            "segment": " "
                        },
                        {
                            "annotation": {
                                "contents": "##Nat",
                                "tag": "TypeReference"
                            },
                            "segment": "Nat"
                        },
                        {
                            "annotation": null,
                            "segment": "\\n"
                        },
                        {
                            "annotation": {
                                "contents": "increment",
                                "tag": "HashQualifier"
                            },
                            "segment": "increment"
                        },
                        {
                            "annotation": null,
                            "segment": " "
                        },
                        {
                            "annotation": {
                                "tag": "Var"
                            },
                            "segment": "input"
                        },
                        {
                            "annotation": {
                                "tag": "BindingEquals"
                            },
                            "segment": " ="
                        },
                        {
                            "annotation": null,
                            "segment": "\\n"
                        },
                        {
                            "annotation": null,
                            "segment": "  "
                        },
                        {
                            "annotation": {
                                "tag": "UseKeyword"
                            },
                            "segment": "use "
                        },
                        {
                            "annotation": {
                                "tag": "UsePrefix"
                            },
                            "segment": "Nat"
                        },
                        {
                            "annotation": null,
                            "segment": " "
                        },
                        {
                            "annotation": {
                                "tag": "UseSuffix"
                            },
                            "segment": "+"
                        },
                        {
                            "annotation": null,
                            "segment": "\\n"
                        },
                        {
                            "annotation": null,
                            "segment": "  "
                        },
                        {
                            "annotation": {
                                "tag": "Var"
                            },
                            "segment": "input"
                        },
                        {
                            "annotation": null,
                            "segment": " "
                        },
                        {
                            "annotation": {
                                "contents": "##Nat.+",
                                "tag": "TermReference"
                            },
                            "segment": "+"
                        },
                        {
                            "annotation": null,
                            "segment": " "
                        },
                        {
                            "annotation": {
                                "tag": "NumericLiteral"
                            },
                            "segment": "1"
                        }
                    ]
                },
                "signature": [
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "TypeReference"
                        },
                        "segment": "Nat"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "TypeOperator"
                        },
                        "segment": "->"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "TypeReference"
                        },
                        "segment": "Nat"
                    }
                ],
                "termDocs": []
            }
        },
        "typeDefinitions": {},
        "missingDefinitions": []
    }
    """


natGtGetDefinitionResponse : String
natGtGetDefinitionResponse =
    """
{
    "termDefinitions": {
        "##Nat.>": {
            "termNames": [
                "base.Nat.gt",
                "base.Nat.>",
                "lab.lib.base.Nat.>",
                "lab.lib.base.Nat.gt"
            ],
            "bestTermName": "Nat.gt",
            "defnTermTag": "Plain",
            "termDefinition": {
                "tag": "BuiltinObject",
                "contents": [
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "TypeReference"
                        },
                        "segment": "Nat"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "TypeOperator"
                        },
                        "segment": "->"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "TypeReference"
                        },
                        "segment": "Nat"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "TypeOperator"
                        },
                        "segment": "->"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Boolean",
                            "tag": "TypeReference"
                        },
                        "segment": "Boolean"
                    }
                ]
            },
            "signature": [
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "TypeOperator"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "TypeOperator"
                    },
                    "segment": "->"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Boolean",
                        "tag": "TypeReference"
                    },
                    "segment": "Boolean"
                }
            ],
            "termDocs": []
        }
    },
    "typeDefinitions": {},
    "missingDefinitions": []
}
"""
