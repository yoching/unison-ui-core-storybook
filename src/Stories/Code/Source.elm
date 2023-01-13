module Stories.Code.Source exposing (..)

import Code.Definition.Source as S
import Code.Definition.Term as Term
import Code.Definition.Type as T
import Code.FullyQualifiedName as F
import Code.Hash
import Code.Syntax exposing (..)
import Code.Workspace.WorkspaceItem exposing (WorkspaceItem(..))
import Helper exposing (col)
import Html exposing (Html)
import Json.Decode exposing (decodeString)
import List.Nonempty as NEL
import Storybook.Story exposing (Story)
import UI.Click


main : Story () Msg
main =
    Storybook.Story.stateless
        { view = view
        }


type Msg
    = HoverStart
    | HoverEnd



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


incrementTerm2 : Syntax -> S.Source
incrementTerm2 syntax =
    S.Term
        (F.fromString "increment")
        (Term.Source
            (syntax |> Term.TermSignature)
            syntax
        )


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


someJSON : String
someJSON =
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


someJson2 : String
someJson2 =
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


view : Html Msg
view =
    let
        decodeResult : Result Json.Decode.Error Syntax
        decodeResult =
            decodeString Code.Syntax.decode someJson2
    in
    case decodeResult of
        Ok source ->
            col [] [ S.view viewConfig (incrementTerm2 source) ]

        Err error ->
            col [] [ Html.text (Json.Decode.errorToString error) ]
