module ANormal exposing
  ( Exp(..)
  , CExp(..)
  , Origin(..)
  , g --
  , toString
  )

import Absyn
import Ty exposing (Ty)

import Dict exposing (Dict)
import Result exposing (Result)

type Exp
  = CompExp CExp
  | Let Id Ty CExp Exp
  | Letrec Id Ty (List ( Id, Ty )) Exp Exp

type CExp
  = Int Int
  | If Id Exp Exp
  | Var Id
  | App Id (List Id)
  | KnlApp Id (List Id) -- negate, cons, ...
  | ExtApp Id (List Id) -- sin, cache, ...

type alias Id = String

type alias Env = Dict String (Origin, Ty)

type Origin
  = Internal
  | Kernel
  | External

type alias State = Fresh -> Result String ( Exp, Ty, Fresh )
type alias Fresh = Int

{-| Make k-normal term from environment and AST expression and return
as state-function. state-function takes fresh-ID to return pair of
k-normal term and fresh-ID.
-}
g : Env -> Absyn.Exp -> State
g env exp =
  case exp of
    Absyn.Bool bool ->
      let
        value = if bool then Int 1 else Int 0
      in
        return (CompExp value) Ty.I32

    Absyn.Int int ->
      return (CompExp (Int int)) Ty.I32

    Absyn.If condExp thenExp elseExp ->
      h env condExp (\condTm condTy ->
      insertLetIfNotVar condTm condTy (\condId ->
      h env thenExp (\thenTm thenTy ->
      h env elseExp (\elseTm elseTy ->
      return (CompExp (If condId thenTm elseTm)) thenTy
      ))))

    Absyn.Let name ty e1 e2 ->
      let
        env_ = env |> Dict.insert name (Internal, ty)
      in
        h env e1 (\tm1 ty1 ->
        case tm1 of
          CompExp cexp ->
            h env_ e2 (\tm2 ty2 ->
            return (Let name ty cexp tm2) ty2
            )

          _ ->
            insertLetIfNotVar tm1 ty1 (\id ->
            h env_ e2 (\tm2 ty2 ->
            return (Let name ty (Var id) tm2) ty2
            ))
        )

    Absyn.Var name ->
      case env |> Dict.get name of
        Just (Internal, ty) ->
          return (CompExp (Var name)) ty

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
        return (Letrec id ty args tm1 tm2) ty2
        ))

    Absyn.App (Absyn.Var id) args ->
      case env |> Dict.get id of
        Just (Kernel, Ty.Ext _ ty) ->
          returnApp env KnlApp id args ty

        Just (External, Ty.Ext _ ty) ->
          returnApp env ExtApp id args ty

        Just (Internal, _ ) ->
          returnApp_ env (Absyn.Var id) args

        _ ->
          throw ("function "++id++" is missing")

    Absyn.App fun args ->
      returnApp_ env fun args


returnApp : Env -> ( Id -> List Id -> CExp ) -> Id -> List Absyn.Exp -> Ty -> State
returnApp env mapper id args ty =
  let
    bind ids args_ =
      case args_ of
        [] ->
          insertLetIfNotVar (mapper id (List.reverse ids) |> CompExp) ty (\id_ ->
          return (CompExp (Var id_)) ty
          )

        hd :: tl ->
          h env hd (\tm_ ty_ ->
          insertLetIfNotVar tm_ ty_ (\id_ ->
          bind (id_ :: ids) tl
          ))
  in
    bind [] args

returnApp_ : Env -> Absyn.Exp -> List Absyn.Exp -> State
returnApp_ env exp args =
    h env exp (\tm ty ->
      case ty of
        (Ty.Arrow _ retTy) ->
          insertLetIfNotVar tm ty (\id ->
          returnApp env App id args retTy
          )

        _ ->
          throw "invalid apply"
    )

h : Env -> Absyn.Exp -> (Exp -> Ty -> State) -> State
h env exp fun =
  \eff ->
    g env exp eff
      |> Result.andThen (\(tm, ty, eff_) ->
        fun tm ty eff_
      )

return : Exp -> Ty -> State
return exp ty =
  \fresh -> Ok ( exp, ty, fresh )

throw : String -> State
throw cause =
  \fresh -> Err cause

{-| Insert let term into a-normal term.

    let xxx = yyy in zzz

The first two argument are 'yyy' and its type. The third argument is the function takes 'xxx'
 and return 'zzz'.
-}

insertLetIfNotVar : Exp -> Ty -> (Id -> State) -> State
insertLetIfNotVar exp ty fun =
  \fresh0 ->
    case exp of
      CompExp (Var id) ->
        fun id fresh0

      CompExp cexp ->
        let
          ( id , fresh1 ) = genId fresh0 ty
        in
          fun id fresh1
            |> Result.map (\(exp_, ty_, fresh2 ) ->
              ( Let id ty cexp exp_, ty_, fresh2 ))

      Let id ty_ cexp exp_ ->
        insertLetIfNotVar exp_ ty fun fresh0
          |> Result.map (\( exp__, _, fresh1 ) -> ( Let id ty_ cexp exp__, ty, fresh1 ))

      Letrec id ty_ args e1 e2 ->
        insertLetIfNotVar e2 ty fun fresh0
          |> Result.map (\( exp_, _, fresh1 ) -> ( Letrec id ty_ args e1 exp_, ty, fresh1))


insertLet : Exp -> Ty -> (Id -> State) -> State
insertLet tm ty fun =
  \fresh0 ->
    case tm of
      CompExp cexp ->
        let
          ( id , fresh1 ) = genId fresh0 ty
        in
          fun id fresh1
            |> Result.map (\(exp, ty_, fresh2 ) ->
              ( Let id ty cexp exp, ty_, fresh2 ))

      Let id ty_ cexp exp ->
        insertLet exp ty fun fresh0
          |> Result.map (\( exp_, _, fresh1 ) -> ( Let id ty_ cexp exp_, ty, fresh1 ))

      Letrec id ty_ args e1 e2 ->
        insertLet e2 ty fun fresh0
          |> Result.map (\( exp_, _, fresh1 ) -> ( Letrec id ty_ args e1 exp_, ty, fresh1))

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

toString : Exp -> String
toString term =
  let
    vd2s ( id, ty ) =
      Ty.toString ty ++ " " ++ id

    help ind exp =
      let
        indent = String.repeat ind "  "
      in
        case exp of
          CompExp cexp ->
            indent ++ helpc ind cexp ++ "\n"

          Let id ty t1 t2 ->
            indent ++ "let " ++ vd2s (id, ty) ++ " = " ++
            helpc (ind+1) t1 ++
            " in\n" ++
            help (ind) t2

          Letrec id ty args t1 t2 ->
            indent ++ "letrec " ++ vd2s (id, ty) ++ "(" ++ (args |> List.map vd2s |> String.join ", ") ++ ") =\n" ++
            help (ind+1) t1 ++
            indent ++ " in\n" ++
            help (ind+1) t2

    helpc ind cexp =
      let
        indent = String.repeat ind "  "
      in
        case cexp of
          Int int ->
            int |> String.fromInt

          If id t1 t2 ->
            "\n" ++
            indent ++ "if " ++ id ++ "\n" ++
            indent ++ "then\n" ++
            help (ind+1) t1 ++
            indent ++ "else\n" ++
            help (ind+1) t2

          Var id ->
            id

          App fun args ->
            fun ++ "(" ++ String.join ", " args ++ ")"

          KnlApp fun args ->
            "@" ++ fun ++ "@(" ++ String.join ", " args ++ ")"

          ExtApp fun args ->
            "$" ++ fun ++ "$(" ++ String.join ", " args ++ ")"
  in
    help 0 term