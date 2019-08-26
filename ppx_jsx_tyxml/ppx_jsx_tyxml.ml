open Ppxlib
module Result = Stdlib.Result


(*
let ( let* ) = Result.bind


let ocaml_reserved =
  [ "and"; "as"; "asr"; "assert"; "begin"; "class"; "constraint"; "do"; "done"
  ; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"
  ; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"
  ; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"
  ; "mod"; "module"; "open"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"
  ; "open!"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"
  ; "try"; "type"; "val"; "virtual"; "when"; "while"; "with" ]


let transform_attr_name name =
  let len = String.length name in
  let name_trim = String.sub name 0 (len - 1) in
  if name.[len - 1] = '_' && List.mem name_trim ocaml_reserved then
    "a_" ^ name_trim
  else
    "a_" ^ name
    *)


    (*
let extract_children ~loc attrs =
  let rec loop (children, other) = function
    | (Nolabel, [%expr ()]) :: [] ->
        (children, other)
    | (Nolabel, _) :: _ ->
        invalid_arg "JSX: found non-labelled argument before the last position"
    | [] ->
        invalid_arg "JSX: () not found in last position"
    | (Labelled "children", children) :: rest ->
        loop (Some children, other) rest
    | arg :: rest ->
        loop (children, arg :: other) rest
  in
  let children, rest = loop (None, []) attrs in
  match children with
  | Some children -> children, rest
  | None -> [%expr []], rest


let transform_attrs ~loc attrs =
  let _children, attrs = extract_children ~loc attrs in
  let attrs =
    attrs |> List.iter (function
      | Optional label, expr (* warning: tyxml doesn't really need these *)
      | Labelled label, expr ->
          let label = transform_attr_name label in

      | Nolabel, _ ->
          assert false
    )
  in
  attrs
  *)


    (*
(* ~a ~b ~c ~children () ->  *)
let transform_lowercase_args args =
  *)


let transform_expr expr =
  match expr.pexp_desc with
  (* <div /> -> div *)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident _; _ }; _ } as ident, args) ->
      (* let* args = transform_lowercase_args args in *)
      Result.Ok { expr with pexp_desc = Pexp_apply (ident, args) }

  (* <View /> -> View.make *)
  | Pexp_apply ({
      pexp_desc = Pexp_ident { txt = Ldot (module_path, "createElement"); loc = lid_loc };
      pexp_loc = id_loc;
      pexp_attributes = id_attrs
    }, args) ->
      let lid = { txt = Ldot (module_path, "view"); loc = lid_loc } in
      let ident = Ast_helper.Exp.ident ~loc:id_loc ~attrs:id_attrs lid in
      (* let* args = transform_uppercase_args args in *)
      Result.Ok { expr with pexp_desc = Pexp_apply (ident, args) }

  (* Fragment: <> foo </> -> [@JSX][foo] *)
  | Pexp_construct ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple _; _ })
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) ->
      Result.Ok expr

  | _ ->
      Result.Error "invalid expr"


let contains_jsx : attributes -> bool =
  List.exists (function
    | { txt = "JSX"; _ }, _ -> true
    | _ -> false
  )


let mapper = object
  inherit Ast_traverse.map as super

  method! expression expr =
    let expr = super#expression expr in
    if contains_jsx expr.pexp_attributes then begin
      match transform_expr expr with
      | Result.Ok expr -> expr
      | Result.Error _ -> expr
    end else
      expr
end


let () =
  Driver.register_transformation
    "ppx_jsx_tyxml"
    ~impl:mapper#structure
    ~intf:mapper#signature
    (*
    ~lint_impl:
    ~lint_intf:
    *)
