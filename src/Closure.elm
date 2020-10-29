module Closure exposing
  ( Term(..)
  , FunDef
  , g
  , funDefToString
  , termToString
  )

import Ty exposing (Ty)
import KNormal as K

import Dict exposing (Dict)
import Set exposing (Set)

type Term
  = Int Int
  | If Id Term Term
  | Let Id Ty Term Term
  | Var Id
  | MakeCls Id Ty Cls Term
  | AppCls Id (List Id)
  | AppDir Label (List Id)

type Cls = Cls Label (List Id) -- entry, runtime free variable

type alias FunDef =
  { name : Label
  , ty : Ty
  , args : List ( Id, Ty )
  , fv : List ( Id, Ty )
  , body : Term
  }

type alias Id = String
type alias Label = String


type alias State = List FunDef -> ( Term, List FunDef )

g : Dict Id Ty -> Set Id -> K.Term -> State
g env known term =
  case term of
    K.Int int ->
      return (Int int)

    K.If id e1 e2 ->
      h env known e1 (\t1 ->
      h env known e2 (\t2 ->
      return (If id t1 t2)
      ))

    K.Let id ty e1 e2 ->
      h env known e1 (\t1 ->
      h (env |> Dict.insert id ty) known e2 (\t2 ->
      return (Let id ty t1 t2)
      ))

    K.Var id ->
      return (Var id)

    K.Letrec (K.Def id ty args e1) e2 ->
      \tl ->
        let
          env_ = env |> Dict.insert id ty

          known_ = known |> Set.insert id |> Debug.log (id++".known")
          ( t1, tl_ ) = g (args |> List.foldl (\( i_, t_ ) -> Dict.insert i_ t_) env_) known_ e1 tl

          ids = List.map Tuple.first args
          zs =
            Set.diff (fv t1) (Set.fromList ids)
              |> Debug.log (id++".zs")

          ( known__, t1_, tl__ ) =
            if zs |> Set.isEmpty
            then ( known_, t1, tl_ )
            else
              let
                env__ =
                  List.foldl
                  (\( id_, ty_ ) -> Dict.insert id_ ty_)
                  env_
                  args
                ( t1__, tl___ ) = g env__ known e1 tl
              in
                ( known, t1__, tl___ )

          zs_ =
            Set.fromList (id :: ids)
              |> Set.diff (fv t1_)
              |> Set.toList

          zts =
            zs_
              |> List.map (\i_ -> env_ |> Dict.get i_ |> Maybe.withDefault (Ty.Custom "ERR") |> (\ty_ -> (i_, ty_)))

          def =
            { name = id
            , ty = ty
            , args = args
            , fv = zts
            , body = t1_
            }

          ( t2, tl____ ) = g env_ known__ e2 (def :: tl__)
        in
          if fv t2 |> Set.member id
          then ( MakeCls id ty (Cls id zs_) t2, tl____ )
          else ( t2, tl____ )

    K.App fun args ->
      if known |> Set.member fun
      then
        return (AppDir fun args)
      else
        return (AppCls fun args)

    K.KnlApp fun args ->
      return (AppDir ("__" ++ fun) args)

    K.ExtApp fun args ->
      return (AppDir ("_" ++ fun) args)


h : Dict Id Ty -> Set Id -> K.Term -> (Term -> State) -> State
h env known exp fun toplevel =
  let
    ( term, toplevel_ ) = g env known exp toplevel
  in
    fun term toplevel_

addTopLevel : Dict Id Ty -> Set Id -> K.Term -> (Term -> ( FunDef, State )) -> State
addTopLevel env known exp fun toplevel =
  let
    ( term, toplevel_ ) = g env known exp toplevel
    ( def, next ) = fun term
  in
    next <| def :: toplevel_

return : Term -> State
return t =
  \tl -> ( t, tl )


fv : Term -> Set Id
fv term =
  let
    s = Set.singleton
    u = Set.union
    r = Set.remove
    l = Set.fromList
  in
    case term of
      Int _          -> Set.empty
      If id t1 t2    -> s id |> u (fv t1) |> u (fv t2)
      Let id _ t1 t2 -> fv t1 |> u (fv t2 |> r id)
      Var id         -> s id
      MakeCls id _ (Cls _ fvs) t
                     -> l fvs |> u (fv t) |> r id
      AppCls id fvs  -> l (id :: fvs)
      AppDir _ fvs   -> l fvs

funDefToString : FunDef -> String
funDefToString def =
  Ty.toString def.ty ++ " " ++ def.name ++
    "(" ++ (def.args |> List.map vd2s |> String.join ", ") ++ ")" ++
    "[" ++ (def.fv |> List.map vd2s |> String.join ", ") ++ "] =\n" ++
      tm2s 1 def.body

termToString : Term -> String
termToString term =
  tm2s 0 term

tm2s : Int -> Term -> String
tm2s idt t =
  let
    indent = String.repeat idt "  "
  in
    case t of
      Int int ->
        indent ++ String.fromInt int ++ "\n"

      If id thenTm elseTm ->
        indent ++ "if " ++ id ++ "\n" ++
        indent ++ "then" ++ "\n" ++
          tm2s (idt+1) thenTm ++
        indent ++ "else " ++ "\n" ++
          tm2s (idt+1) elseTm

      Let id ty e1 e2 ->
        indent ++ "let " ++ vd2s ( id, ty )++ " =\n" ++
          tm2s (idt+1) e1 ++
        indent ++"in\n" ++
          tm2s (idt+1) e2

      Var id ->
        indent ++ id ++ "\n"

      MakeCls id ty (Cls label args) e ->
        indent ++ "MakeCls " ++ Ty.toString ty ++ " " ++ id ++ " = " ++ "(#" ++ id ++ "|" ++ String.join ", " args ++ ") in\n" ++
          tm2s (idt+1) e

      AppCls id args ->
        indent ++ "AppCls " ++ id ++ "(" ++ String.join ", " args ++ ")\n"

      AppDir label args ->
        indent ++ "AppDir " ++ label ++ "(" ++ String.join ", " args ++ ")\n"

vd2s ( id, ty ) =
  Ty.toString ty ++ " " ++ id