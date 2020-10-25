module Absyn exposing
  ( Exp(..)
  , Def(..)
  , parseExp
  , parseType
  )

import Ty exposing (Ty)
import Peg.Parser as Peg exposing (Parser)



type Exp
  = Bool Bool
  | Int Int
  | If Exp Exp Exp
  | Let Id Ty Exp Exp
  | Var Id
  | Letrec Def Exp
  | App Exp (List Exp)

type Def =
  Def Id Ty (List (Id, Ty)) Exp

type alias Id = String

parseExp : String -> Result String Exp
parseExp src =
  case Peg.parse src pExp of
    Nothing -> Err "Parse Error"
    Just ast -> Ok ast

parseType : String -> Result String Ty
parseType src =
  case Peg.parse src pType of
    Nothing -> Err "Parse Error"
    Just ty -> Ok ty

pSp =
  Peg.char (\c -> List.member c spChars)
    |> Peg.oneOrMore
spChars =
  [' ', '\t']

pOpSp = Peg.option pSp

pLParen = Peg.match "("
pRParen = Peg.match ")"
pColon  = Peg.match ":"
pEq     = Peg.match "="
pArrow  = Peg.match "->"
pIf     = Peg.match "if"
pThen   = Peg.match "then"
pElse   = Peg.match "else"
pLet    = Peg.match "let"
pIn     = Peg.match "in"
pLetrec = Peg.match "letrec"

keyword =
  [ "True"
  , "False"
  , "if"
  , "then"
  , "else"
  , "let"
  , "in"
  , "letrec"
  ]

pDigit =
  Peg.char Char.isDigit
pLower =
  Peg.char Char.isAlpha
pUpper =
  Peg.char Char.isUpper
pNameTail =
  Peg.char (\c -> Char.isAlphaNum c || c == '_')
    |> Peg.zeroOrMore

nameHelper phead ptail =
  Peg.seq2
  phead ptail
  (\hd tl -> String.fromList (hd :: tl))
    |> Peg.andThen (\name ->
      if List.member name keyword
      then Peg.fail
      else Peg.return name
    )

pVarName =
  nameHelper pLower pNameTail
pTypeName =
  nameHelper pUpper pNameTail

pSimpleType =
  Peg.choice
  [ \_ -> Peg.seq3 pLParen pType pRParen (\_ ty _ -> ty)
  , \_ -> pTypeName
      |> Peg.map (\name -> case name of
        "Bool" -> Ty.Bool
        "I32" -> Ty.I32
        _ -> Ty.Custom name
      )
  ]
pType =
  Peg.choice
  [ \_ -> Peg.seq3 pSimpleType pArrow pType (\l _ r -> Ty.Arrow l r)
  , \_ -> pSimpleType
  ]

pVarDecl =
  Peg.seq3
  pVarName pColon pType
  (\var _ ty -> ( var, ty ))

pVar =
  pVarName |> Peg.map Var

pTrue =
  Peg.match "True"
    |> Peg.map (always <| Bool True)

pFalse =
  Peg.match "False"
    |> Peg.map (always <| Bool False)

pInt =
  pDigit
    |> Peg.oneOrMore
    |> Peg.andThen (\chars ->
      case chars |> String.fromList |> String.toInt of
        Just i -> Peg.return <| Int i
        Nothing -> Peg.fail
    )

pTerm =
  Peg.choice
  [ \_ -> pTrue
  , \_ -> pFalse
  , \_ -> pInt
  , \_ -> pVar
  ]

pApp =
  Peg.seq2
  pTerm
  (Peg.oneOrMore (Peg.seq2 pSp pTerm (\_ t -> t)))
  App

pIfExp =
  Peg.intersperseSeq6 pSp
  pIf pExp pThen pExp pElse pExp
  (\_ c _ t _ f -> If c t f)

pLetExp =
  let
    pVarDef =
      Peg.intersperseSeq3 pOpSp pVarDecl pEq pExp
      (\( var, ty ) _ def -> ( var, ty, def ))
  in
    Peg.intersperseSeq4 pSp pLet pVarDef pIn pExp
    (\_ ( var, ty, def ) _ t -> Let var ty def t)

pLetrecExp =
  let
    pArgs =
      Peg.join pSp pVarDecl
        |> Peg.option
        |> Peg.map (\m -> case m of
          Nothing -> []
          Just l -> l
        )

    pFun =
      Peg.seq3 pVarDecl pSp pArgs
      (\( id, ty ) _ args -> ( id, ty, args ))
    pVarDef =
      Peg.intersperseSeq3 pOpSp pFun pEq pExp
      (\( id, ty, args ) _ body -> Def id ty args body)
  in
    Peg.intersperseSeq4 pSp pLetrec pVarDef pIn pExp
    (\_ def _ t -> Letrec def t)

pExp =
  Peg.choice
  [ \_ -> pIfExp
  , \_ -> pLetExp
  , \_ -> pLetrecExp
  , \_ -> pApp
  , \_ -> pTerm
  ]