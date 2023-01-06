module Stories.Code.Source exposing (..)

import Code.Definition.Source as S
import Code.Definition.Term as Term
import Code.Definition.Type as T
import Code.FullyQualifiedName as F
import Code.Hash
import Code.Syntax exposing (..)
import Helper exposing (col)
import Html exposing (Html)
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


view : Html Msg
view =
    sources
        |> List.map (S.view viewConfig)
        |> col []
