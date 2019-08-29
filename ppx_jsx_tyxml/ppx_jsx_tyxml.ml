open Ppxlib
module Result = Stdlib.Result
module Lint_error = Ppxlib.Driver.Lint_error


let ( let* ) = Result.bind


let lident ~loc id = { txt = Lident id; loc }


let list ~loc ls =
  let empty = Ast_helper.Exp.construct ~loc (lident ~loc "[]") None in
  List.fold_left (fun acc expr ->
    Ast_helper.(Exp.construct
      ~loc:expr.pexp_loc
      (lident ~loc:expr.pexp_loc "::")
      (Some (Exp.tuple ~loc:expr.pexp_loc [expr; acc]))
    )
  ) empty ls


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


let transform_labelled name expr =
  let open Ast_helper in
  let loc = expr.pexp_loc in
  let name = transform_attr_name name in
  let ident = Exp.ident ~loc (lident ~loc name) in
  Exp.apply ~loc ident [Nolabel, expr]


(* ~href:link ~ayy ~children () -> ~a:[a_href link; a_ayy ayy] children *)
let transform_lowercase_args ~loc args =
  let rec loop attrs children = function
    | (Nolabel, { pexp_desc = Pexp_construct ({ txt = Lident "()"; _}, None); _ }) :: [] ->
        Result.Ok (attrs, children)
    | (Nolabel, { pexp_loc; _ }) :: _ ->
        "found non-labelled argument before the last position"
        |> Lint_error.of_string pexp_loc |> Result.error
    | [] ->
        "() not found in last position"
        |> Lint_error.of_string loc |> Result.error
    | (Labelled "children", c) :: rest when children = None ->
        loop attrs (Some c) rest
    | (Labelled "children", { pexp_loc; _ }) :: _ ->
        "children declared more than once"
        |> Lint_error.of_string pexp_loc |> Result.error
    | (Labelled name, expr) :: rest ->
        let attr = transform_labelled name expr in
        loop (attr :: attrs) children rest
    | (Optional _name, _expr) :: _rest ->
        failwith "ayy"
  in
  let* attrs, children = loop [] None args in
  let attrs = list ~loc attrs in
  let children = match children with
  | Some children -> children
  | None -> Ast_helper.(Exp.construct ~loc (lident ~loc "[]") None)
  in
  let args = [(Labelled "a", attrs); (Nolabel, children)] in
  Result.Ok args


let transform_expr expr =
  match expr.pexp_desc with
  (* <div /> -> div *)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident _; _ }; _ } as ident, args) ->
      let* args = transform_lowercase_args ~loc:expr.pexp_loc args in
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
      Result.Error (Lint_error.of_string expr.pexp_loc "invalid expr")


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


let lint_folder = object
  inherit [Lint_error.t list] Ast_traverse.fold as super

  method! expression expr errors =
    let errors = super#expression expr errors in
    if contains_jsx expr.pexp_attributes then begin
      match transform_expr expr with
      | Result.Ok _ -> errors
      | Result.Error err -> err :: errors
    end else
      errors
end


let () =
  Driver.register_transformation
    "ppx_jsx_tyxml"
    ~impl:mapper#structure
    ~lint_impl:(fun impl -> lint_folder#structure impl [])
