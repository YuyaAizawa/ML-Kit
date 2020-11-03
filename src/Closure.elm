module Closure exposing
  ( Exp(..)
  , CExp(..)
  , g
  , toString
  )

import Ty exposing (Ty)
import ANormal as A

import Dict exposing (Dict)
import Set exposing (Set)

type Exp
  = CompExp CExp
  | Let Id Ty CExp Exp
  | Letrec Id Ty (List ( Id, Ty )) Exp Exp

type CExp
  = Int Int
  | If Id Exp Exp
  | Var Id
  | MakeCls Label (List Id)
  | ProjCls Id Id
  | AppCls Id (List Id)
  | AppDir Label (List Id)

type alias Id = String
type alias Label = String

clsTyFromId id = Ty.Custom ("_Cls_"++id)

g : Dict Id Ty -> Set Id -> A.Exp -> Exp  -- type environment, known as function (not closure), a-normal expression
g env known exp =
  case exp of
    A.CompExp cexp ->
      CompExp (h env known cexp)

    A.Let id ty cexp exp_ ->
      let
        known_ = case cexp of
          A.Var id_ ->
            if known |> Set.member id_
            then known |> Set.insert id
            else known
          _ ->
            known

        env_ = env |> Dict.insert id ty
      in
        Let id ty (h env known cexp) (g env_ known_ exp_)

    A.Letrec id ty args e1 e2 ->
      let
        env_ = env |> Dict.insert id ty
        env__ = args |> List.foldl (\( id_, ty_ ) -> Dict.insert id_ ty_ ) env_
        known_ = known |> Set.insert id

        t1 = g env__ known_ e1
        ids = List.map Tuple.first args
        zs = Set.diff (fv t1) (Set.fromList ids)
      in
        if zs |> Set.isEmpty
        then -- just function
          Letrec id ty args t1 (g env_ known_ e2)
        else -- need closure
          let
            t1_ = g env__ known e1
            zs_ =
              Set.fromList (id :: ids)
                |> Set.diff (fv t1_)
                |> Set.toList
            t1__ = openCls env__ clsName zs_ t1_
            clsName = "_cls_"++id
            clsTy = clsTyFromId id
            t2 = g env_ known e2
          in
            Letrec
            clsName
            ty
            (( id, clsTy ) :: args)
            t1__
            (Let id clsTy (MakeCls clsName zs_) t2)

openCls : Dict Id Ty -> Id -> List Id -> Exp -> Exp
openCls env clsName args exp =
  case args of
    [] ->
      exp

    hd :: tl ->
      Let
      hd
      (env |> Dict.get hd |> Maybe.withDefault (Ty.Custom "ClsErr"))
      (ProjCls clsName hd)
      (openCls env clsName tl exp)

h : Dict Id Ty -> Set Id -> A.CExp -> CExp
h env known cexp =
  case cexp of
    A.Int int ->
      Int int

    A.If id e1 e2 ->
      If id (g env known e1) (g env known e2)

    A.Var id ->
      Var id

    A.App fun args ->
      if known |> Set.member fun
      then
        AppDir fun args
      else
        AppCls fun args

    A.KnlApp fun args ->
      AppDir ("__" ++ fun) args

    A.ExtApp fun args ->
      AppDir ("_" ++ fun) args

fv : Exp -> Set Id
fv exp =
  let
    s = Set.singleton
    u = Set.union
    d = Set.diff
    r = Set.remove
    l = Set.fromList
  in
    case exp of
      CompExp cexp -> fvc cexp
      Let id _ t1 t2 -> fvc t1 |> u (fv t2 |> r id)
      Letrec id _ args e1 e2 ->
        d (u (fv e1) (fv e2)) <| l (args |> List.map Tuple.first |> (::) id)

fvc : CExp -> Set Id
fvc cexp =
  let
    s = Set.singleton
    u = Set.union
    r = Set.remove
    l = Set.fromList
  in
    case cexp of
      Int _         -> Set.empty
      If id t1 t2   -> s id |> u (fv t1) |> u (fv t2)
      Var id        -> s id
      MakeCls _ fvs -> l fvs
      ProjCls id _   -> s id
      AppCls id fvs -> l (id :: fvs)
      AppDir _ fvs  -> l fvs

toString : Exp -> String
toString term =
  let
    vd2s ( id, ty ) =
      Ty.toString ty ++ " " ++ id

    help : Int -> Exp -> String
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

    helpc : Int -> CExp -> String
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

          MakeCls label args ->
            "MakeCls " ++ label ++ "[" ++ String.join ", " args ++ "]"

          ProjCls clsName id ->
            clsName ++ "." ++ id

          AppCls id args ->
            "AppCls " ++ id ++ "(" ++ String.join ", " args ++ ")"

          AppDir fun args ->
            "AppDir " ++ fun ++ "(" ++ String.join ", " args ++ ")"
  in
    help 0 term
