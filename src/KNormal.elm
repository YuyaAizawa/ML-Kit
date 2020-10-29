module KNormal exposing
  ( Term(..)
  , Def(..)
  , Origin(..)
  , g --
  , toString
  )

import Absyn exposing (Exp)
import Ty exposing (Ty)

import Dict exposing (Dict)
import Result exposing (Result)

type Term
  = Int Int
  | If Id Term Term
  | Let Id Ty Term Term
  | Var Id
  | Letrec Def Term
  | App Id (List Id)
  | KnlApp Id (List Id) -- negate, cons, ...
  | ExtApp Id (List Id) -- sin, cache, ...

type Def =
  Def Id Ty (List ( Id, Ty )) Term

type alias Id = String

type alias Fresh = Int

type alias Env = Dict String (Origin, Ty)

type Origin
  = Internal
  | Kernel
  | External

type alias State = Fresh -> Result String ( Term, Ty, Fresh )


{-| Make k-normal term from environment, effects, and expression and return
with type and fresh id.
-}
g : Env -> Exp -> State
g env exp =
  case exp of
    Absyn.Bool bool ->
      let
        term = if bool then Int 1 else Int 0
      in
        return term Ty.I32

    Absyn.Int int ->
      return (Int int) Ty.I32

    Absyn.If condExp thenExp elseExp ->
      h env condExp (\condTm condTy ->
      insertLet condTm condTy (\condId ->
      h env thenExp (\thenTm thenTy ->
      h env elseExp (\elseTm elseTy ->
      return (If condId thenTm elseTm) thenTy
      ))))

    Absyn.Let name ty e1 e2 ->
      let
        env_ = env |> Dict.insert name (Internal, ty)
      in
        h env e1 (\tm1 ty1 ->
        h env_ e2 (\tm2 ty2 ->
        return (Let name ty tm1 tm2) ty2
        ))

    Absyn.Var name ->
      case env |> Dict.get name of
        Just (Internal, ty) ->
          return (Var name) ty

        _ ->
          throw ("var \""++name++"\" is missing")

    Absyn.Letrec (Absyn.Def id ty args e1) e2 ->
      let
        env_ =
          env
            |> Dict.insert id (Internal, ty)

        env__ =
          args
            |> List.foldl (\(id_, ty_) ->
              Dict.insert id_ (Internal, ty_)) env_
      in
        h env_ e2 (\tm2 ty2 ->
        h env__ e1 (\tm1 ty1 ->
        return (Letrec (Def id ty args tm1) tm2) ty2
        ))

    Absyn.App (Absyn.Var id) args ->
      case env |> Dict.get id of
        Just (Kernel, Ty.Ext _ ty) ->
          returnApp env KnlApp id args ty

        Just (External, Ty.Ext _ ty) ->
          returnApp env ExtApp id args ty

        _ ->
          returnApp_ env (Absyn.Var id) args

    Absyn.App fun args ->
      returnApp_ env fun args


returnApp : Env -> ( Id -> List Id -> Term ) -> Id -> List Exp -> Ty -> State
returnApp env mapper id args ty =
  let
    bind ids args_ =
      case args_ of
        [] ->
          return (mapper id (List.reverse ids)) ty

        hd :: tl ->
          h env hd (\tm_ ty_ ->
          insertLet tm_ ty_ (\id_ ->
          bind (id_ :: ids) tl
          ))
  in
    bind [] args

returnApp_ : Env -> Exp -> List Exp -> State
returnApp_ env exp args =
    h env exp (\tm ty ->
      case ty of
        (Ty.Arrow _ retTy) ->
          insertLet tm ty (\id ->
            returnApp env App id args retTy
          )

        _ -> throw "invalid apply"
    )

h : Env -> Exp -> (Term -> Ty -> State) -> State
h env exp fun =
  \eff ->
    g env exp eff
      |> Result.andThen (\(tm, ty, eff_) ->
        fun tm ty eff_
      )

return : Term -> Ty -> State
return term ty =
  \fresh -> Ok ( term, ty, fresh )

throw : String -> State
throw cause =
  \fresh -> Err cause

{-| Insert let term into k-normal term.

    let xxx = yyy in zzz

The first two argument are 'yyy' and its type. The third argument is the function takes 'xxx'
 and return 'zzz'.
-}
insertLet : Term -> Ty -> (Id -> State) -> State
insertLet term ty fun =
  \fresh0 ->
    let
      ( id , fresh1 ) = genId fresh0 ty
    in
      fun id fresh1
        |> Result.map (\(term_, ty_, fresh2 ) ->
          ( Let id ty term term_, ty_, fresh2 )
        )

genId : Fresh -> Ty -> ( Id, Fresh )
genId fresh ty =
  let
    prefix =
      case ty of
        Ty.Bool      -> "b"
        Ty.I32       -> "i"
        Ty.Arrow _ _ -> "f"
        Ty.Ext _ _   -> "e"
        Ty.Custom _  -> "c"

    name =
      fresh
        |> String.fromInt
        |> String.padLeft 4 '0'
        |> (\num -> prefix ++ num)

  in
    ( name, fresh + 1 )

toString term =
  let
    vd2s ( id, ty ) =
      Ty.toString ty ++ " " ++ id

    help ind tm =
      let
        indent = String.repeat ind "  "
      in
        case tm of
          Int int ->
            indent ++ (int |> String.fromInt) ++ "\n"

          If id t1 t2 ->
            indent ++ "if " ++ id ++ "\n" ++
            indent ++ "then\n" ++
            help (ind+1) t1 ++
            indent ++ "else\n" ++
            help (ind+1) t2

          Let id ty t1 t2 ->
            indent ++ "let " ++ vd2s (id, ty) ++ " =\n" ++
            help (ind+1) t1 ++
            indent ++ "in\n" ++
            help (ind+1) t2

          Var id ->
            indent ++ id ++ "\n"

          Letrec (Def id ty args t1) t2 ->
            indent ++ "letrec " ++ vd2s (id, ty) ++ "(" ++ (args |> List.map vd2s |> String.join ", ") ++ ") =\n" ++
            help (ind+1) t1 ++
            indent ++ "in\n" ++
            help (ind+1) t2

          App fun args ->
            indent ++ fun ++ "(" ++ String.join ", " args ++ ")\n"

          KnlApp fun args ->
            indent ++ "@" ++ fun ++ "@(" ++ String.join ", " args ++ ")\n"

          ExtApp fun args ->
            indent ++ "$" ++ fun ++ "$(" ++ String.join ", " args ++ ")\n"
  in
    help 0 term