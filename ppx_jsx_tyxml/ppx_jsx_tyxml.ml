open Ppxlib


let transform_apply call_expr _args =
  match call_expr.pexp_desc with
  (* <div /> *)
  | Pexp_ident { txt = Lident ident; loc } ->
      ignore loc;
      ignore ident;
      ()
  (* <View /> *)
  | Pexp_ident { txt = Ldot (module_path, "createElement"); loc } ->
      ignore module_path;
      ignore loc;
      ()
  | _ ->
      (* warning *)
      ()


let transform_expr = function
  | { pexp_desc = Pexp_apply (call_expr, args); _ } as apply ->
      transform_apply call_expr args;
      apply

  (* <> foo </> desugars to [@JSX][foo] *)
  | { pexp_desc =
      Pexp_construct ({ txt = Lident "::"; loc = _ }, None); _ }
  | { pexp_desc =
      Pexp_construct ({ txt = Lident "[]"; loc = _ }, Some { pexp_desc = Pexp_tuple _; _ })
    ; _ } as list_items ->
      list_items

  | expr ->
      (* this should be a warning! *)
      expr


let contains_jsx : attributes -> bool =
  List.exists (function
    | { txt = "JSX"; _ }, _ -> true
    | _ -> false
  )


let mapper = object
  inherit Ast_traverse.map as super

  method! expression expr =
    let expr = super#expression expr in
    if contains_jsx expr.pexp_attributes then
      transform_expr expr
    else
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
