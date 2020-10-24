module KNormal exposing
  ( Term(..)
  , Effect
  , Origin(..)
  , g --
  , toString
  )

import Dict exposing (Dict)
import Absyn exposing (Exp)
import Ty exposing (Ty)

type Term
  = Int Int
  | If Id Term Term
  | Let Id Ty Term Term
  | Var Id
  | App Id (List Id)
  | KnlApp Id (List Id) -- negate, cons, ...
  | ExtApp Id (List Id) -- sin, cache, ...
  | Error String

type alias Id = String

type alias Effect =
  { fresh : Int
  }

type alias Env = Dict String (Origin, Ty)

type Origin
  = Internal
  | Kernel
  | External



{-| Make k-normal term from environment, effects, and expression and return
with type and fresh id.
-}
g : Env -> Effect -> Exp -> ( Term, Ty, Effect )
g env eff exp =
  case exp of
    Absyn.Bool bool ->
      let
        term = if bool then Int 1 else Int 0
      in
        ( term, Ty.I32, eff )

    Absyn.Int int ->
      ( Int int, Ty.I32, eff )

    Absyn.If condExp thenExp elseExp ->
      insertLet (g env eff condExp) (\eff1 condId ->
        let
          ( thenTm, thenTy, eff2 ) = g env eff1 thenExp
          ( elseTm, elseTy, eff3 ) = g env eff2 elseExp
        in
          ( If condId thenTm elseTm, thenTy, eff3 )
      )

    Absyn.Let name ty e1 e2 ->
      let
        ( tm1, ty1, eff1 ) = g env eff e1
        ( tm2, ty2, eff2 ) = g (env |> Dict.insert name (Internal, ty)) eff1 e2
      in
        ( Let name ty tm1 tm2, ty2, eff2 )

    Absyn.Var name ->
      case env |> Dict.get name of
        Just (Internal, ty) ->
          ( Var name, ty, eff )

        _ ->
          ( Error ("var \""++name++"\" is missing"), Ty.I32, eff )


    Absyn.App (Absyn.Var id) args ->
      case env |> Dict.get id of
        Just (Kernel, ty) ->
          makeSpecialApp KnlApp env eff id ty args

        Just (External, ty) ->
          makeSpecialApp ExtApp env eff id ty args

        _ ->
          makeApp env eff (Absyn.Var id) args

    Absyn.App fun args ->
      makeApp env eff fun args

makeSpecialApp : ( Id -> List Id -> Term ) -> Env -> Effect -> Id -> Ty -> List Exp -> ( Term, Ty, Effect )
makeSpecialApp mapper env eff id ty args =
  let
    bind eff_ ids args_ =
      case args_ of
        [] ->
          ( mapper id (List.reverse ids), ty, eff_ )

        hd :: tl ->
          insertLet (g env eff_ hd) (\eff__ id_ ->
            bind eff__ (id_ :: ids) tl
          )
  in
    bind eff [] args

makeApp : Env -> Effect -> Exp -> List Exp -> ( Term, Ty, Effect )
makeApp env eff exp args =
  case g env eff exp of
    ( _, Ty.Arrow _ retTy, _ ) as ge1 ->
      insertLet ge1 (\eff_ fun ->
        let
          bind eff__ ids args_ =
            case args_ of
              [] ->
                ( App fun (List.reverse ids), retTy, eff__ )

              hd :: tl ->
                insertLet (g env eff__ hd) (\eff___ id__ ->
                  bind eff___ (id__ :: ids) tl
                )
        in
          bind eff_ [] args
      )

    _ ->
      ( Error "invalid apply", Ty.I32, eff )



{-| Insert let term into k-normal term.

    let xxx = yyy in zzz

The first argument makes 'yyy'. The second argument is the function takes 'xxx'
 and return 'zzz'.
-}
insertLet : ( Term, Ty, Effect ) -> (Effect -> Id -> ( Term, Ty, Effect )) -> ( Term, Ty, Effect )
insertLet ( tm1, ty1, eff ) k =
  let
    ( id , eff_ ) = genId eff ty1
    ( tm2, ty2, eff__ ) = k eff_ id
  in
    ( Let id ty1 tm1 tm2, ty2, eff__ )

genId : Effect -> Ty -> ( Id, Effect )
genId eff ty =
  let
    prefix =
      case ty of
        Ty.Bool      -> "b"
        Ty.I32       -> "i"
        Ty.Arrow _ _ -> "f"
        Ty.Custom _  -> "c"

    name =
      eff.fresh
        |> String.fromInt
        |> String.padLeft 4 '0'
        |> (\num -> prefix ++ num)

  in
    ( name, { eff | fresh = eff.fresh + 1} )

toString term =
  let
    ty2s ty =
      case ty of
        Ty.Bool -> "Bool"
        Ty.I32 -> "I32"
        Ty.Arrow a b -> "("++ty2s a++"->"++ty2s b++")"
        Ty.Custom n -> n

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
            indent ++ "let " ++ ty2s ty ++ ":" ++ id ++ " =\n" ++
            help (ind+1) t1 ++
            indent ++ "in\n" ++
            help (ind+1) t2

          Var id ->
            indent ++ id ++ "\n"

          App fun args ->
            indent ++ fun ++ "(" ++ String.join ", " args ++ ")\n"

          KnlApp fun args ->
            indent ++ "@" ++ fun ++ "@(" ++ String.join ", " args ++ ")\n"

          ExtApp fun args ->
            indent ++ "$" ++ fun ++ "$(" ++ String.join ", " args ++ ")\n"

          Error e ->
            indent ++ "ERROR: " ++ e ++ "\n"
  in
    help 0 term