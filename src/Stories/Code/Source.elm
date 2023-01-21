module Stories.Code.Source exposing (..)

import Code.Definition.Source as S
import Code.Definition.Term as Term
import Code.Definition.Type as T
import Code.FullyQualifiedName as F
import Code.Hash
import Code.Syntax exposing (..)
import Code.Workspace.WorkspaceItem exposing (Item, WorkspaceItem(..), decodeList, decodeTermDetails, fromItem)
import Helper exposing (col)
import Html exposing (Html, samp)
import Json.Decode exposing (decodeString)
import List.Nonempty as NEL
import ReferenceHelper exposing (sampleReference)
import Storybook.Story exposing (Story)
import UI.Click
import UI.Icon exposing (term)
import UI.ViewMode


main : Story () Msg
main =
    Storybook.Story.stateless
        { view = view
        }


type Msg
    = HoverStart
    | HoverEnd
    | WorkspaceMessage Code.Workspace.WorkspaceItem.Msg



-- view : Html Msg
-- view =
--     S.view S.Plain (S.Type T.Builtin)


natTypeReference : SyntaxType
natTypeReference =
    TypeReference
        (Code.Hash.unsafeFromString "##Nat")
        (Maybe.Just (F.fromString "##Nat"))


booleanTypeReference : SyntaxType
booleanTypeReference =
    TypeReference
        (Code.Hash.unsafeFromString "Boolean")
        (Maybe.Just (F.fromString "##Boolean"))


natGtSignatureSyntax : Syntax
natGtSignatureSyntax =
    [ SyntaxSegment natTypeReference "Nat"
    , SyntaxSegment Blank " "
    , SyntaxSegment TypeOperator "->"
    , SyntaxSegment Blank " "
    , SyntaxSegment natTypeReference "Nat"
    , SyntaxSegment Blank " "
    , SyntaxSegment TypeOperator "->"
    , SyntaxSegment Blank " "
    , SyntaxSegment booleanTypeReference "Boolean"
    ]
        |> NEL.fromList
        |> Maybe.withDefault
            (NEL.singleton
                (SyntaxSegment NumericLiteral "foo")
            )
        |> Syntax


natGtTermDefinitionSyntax : Syntax
natGtTermDefinitionSyntax =
    [ SyntaxSegment natTypeReference "Nat"
    , SyntaxSegment Blank " "
    , SyntaxSegment TypeOperator "->"
    , SyntaxSegment Blank " "
    , SyntaxSegment natTypeReference "Nat"
    , SyntaxSegment Blank " "
    , SyntaxSegment TypeOperator "->"
    , SyntaxSegment Blank " "
    , SyntaxSegment booleanTypeReference "Boolean"
    ]
        |> NEL.fromList
        |> Maybe.withDefault
            (NEL.singleton
                (SyntaxSegment NumericLiteral "foo")
            )
        |> Syntax


builtinTerm : S.Source
builtinTerm =
    S.Term
        (F.fromString "Nat.gt")
        (Term.Builtin (Term.TermSignature natGtSignatureSyntax))


incrementSignatureSyntax : Syntax
incrementSignatureSyntax =
    [ SyntaxSegment Blank " "
    , SyntaxSegment TypeOperator "->"
    , SyntaxSegment Blank " "
    ]
        |> NEL.fromList
        |> Maybe.withDefault
            (NEL.singleton
                (SyntaxSegment NumericLiteral "foo")
            )
        |> Syntax


incrementDefinitionSyntax : Syntax
incrementDefinitionSyntax =
    [ SyntaxSegment (HashQualifier "increment") "increment"
    , SyntaxSegment TypeAscriptionColon " :"
    , SyntaxSegment Blank " "
    , SyntaxSegment
        (TypeReference
            (Code.Hash.unsafeFromString "abc")
            (Maybe.Just (F.fromString "##Nat"))
        )
        "Nat"
    , SyntaxSegment Blank " "
    , SyntaxSegment TypeOperator "->"
    , SyntaxSegment Blank " "
    , SyntaxSegment
        (TypeReference
            (Code.Hash.unsafeFromString "abc")
            (Maybe.Just (F.fromString "##Nat"))
        )
        "Nat"
    , SyntaxSegment Blank "\n"
    , SyntaxSegment
        (HashQualifier "increment")
        "increment"
    , SyntaxSegment Blank " "
    , SyntaxSegment Var "input"
    , SyntaxSegment BindingEquals " ="
    , SyntaxSegment Blank "\n"
    , SyntaxSegment Blank "  "
    , SyntaxSegment UseKeyword "use "
    , SyntaxSegment UsePrefix "Nat"
    , SyntaxSegment Blank " "
    , SyntaxSegment UseSuffix "+"
    , SyntaxSegment Blank "\n"
    , SyntaxSegment Blank "  "
    , SyntaxSegment Var "input"
    , SyntaxSegment Blank " "
    , SyntaxSegment
        (TermReference (Code.Hash.unsafeFromString "abc")
            (Maybe.Just (F.fromString "##Nat"))
        )
        "+"
    , SyntaxSegment Blank " "
    , SyntaxSegment NumericLiteral "1"
    ]
        |> NEL.fromList
        |> Maybe.withDefault
            (NEL.singleton
                (SyntaxSegment NumericLiteral "foo")
            )
        |> Syntax


incrementTerm : S.Source
incrementTerm =
    S.Term
        (F.fromString "increment")
        (Term.Source
            (incrementSignatureSyntax |> Term.TermSignature)
            incrementDefinitionSyntax
        )


makeTerm : String -> Syntax -> Syntax -> S.Source
makeTerm fqnString termSignatureSyntax sourceSyntax =
    S.Term
        (F.fromString fqnString)
        (Term.Source
            (termSignatureSyntax |> Term.TermSignature)
            sourceSyntax
        )


makeTerm2 : String -> Term.TermSource -> S.Source
makeTerm2 fqnString termSource =
    S.Term
        (F.fromString fqnString)
        termSource


viewConfig : S.ViewConfig Msg
viewConfig =
    S.Rich
        { toClick = \_ -> UI.Click.Disabled
        , tooltip =
            { toHoverStart = \_ -> HoverStart
            , toHoverEnd = \_ -> HoverEnd
            , toTooltip = \_ -> Maybe.Nothing
            }
        }


sources : List S.Source
sources =
    [ incrementTerm

    -- , builtinTerm
    ]



-- view : Html Msg
-- view =
--     sources
--         |> List.map (S.view viewConfig)
--         |> col []


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
                    item |> viewItem |> ignoreWorkspaceMsg
                )
                source
                |> col []



-- col [] [ S.view viewConfig (makeTerm2 "increment" source) ]
-- JSON


viewItem : Item -> Html Code.Workspace.WorkspaceItem.Msg
viewItem item =
    let
        workspaceItem =
            fromItem sampleReference item
    in
    Code.Workspace.WorkspaceItem.view Nothing UI.ViewMode.Regular workspaceItem True


ignoreWorkspaceMsg : Html Code.Workspace.WorkspaceItem.Msg -> Html Msg
ignoreWorkspaceMsg original =
    Html.map WorkspaceMessage original


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


incrementTermDefinition : String
incrementTermDefinition =
    """
{
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
"""


incrementTermDefinitionContent : String
incrementTermDefinitionContent =
    """
 [
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


natGtTermDefinitionContents : String
natGtTermDefinitionContents =
    """
[
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
"""
