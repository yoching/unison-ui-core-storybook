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


sampleSyntax : Syntax
sampleSyntax =
    Syntax
        (NEL.fromList
            [ SyntaxSegment NumericLiteral "numeric-literal"
            , SyntaxSegment CharLiteral "char-literal"
            , SyntaxSegment TextLiteral "text-literal"
            , SyntaxSegment Blank " "
            , SyntaxSegment Var "x"
            , SyntaxSegment Unit "unit"
            , SyntaxSegment DocDelimiter "doc-delimiter"
            , SyntaxSegment ControlKeyword "if"
            ]
            |> Maybe.withDefault
                (NEL.singleton
                    (SyntaxSegment NumericLiteral "foo")
                )
        )


builtinTermSource : S.Source
builtinTermSource =
    S.Term
        (F.fromString "base.List.map")
        (Term.Builtin (Term.TermSignature sampleSyntax))


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


view : Html Msg
view =
    S.view viewConfig incrementTerm
