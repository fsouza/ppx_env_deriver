open StdLabels
open Ppxlib
open Ast_builder.Default

let generate_impl ~ctxt:_ (_rec_flag, _type_declarations) = assert false

let generate_of_type_function { ptype_loc = loc; ptype_name; _ } =
  let env_assoc_list = [%type: (string * string) list] in
  psig_value ~loc
    {
      pval_name = { ptype_name with txt = "env_of_" ^ ptype_name.txt };
      pval_type =
        ptyp_arrow ~loc Nolabel
          (ptyp_constr ~loc { loc; txt = lident ptype_name.txt } [])
          env_assoc_list;
      pval_loc = loc;
      pval_attributes = [];
      pval_prim = [];
    }

let generate_of_env_function { ptype_loc = loc; ptype_name; _ } =
  let unit = [%type: unit] in
  psig_value ~loc
    {
      pval_name = { ptype_name with txt = ptype_name.txt ^ "_of_env" };
      pval_type =
        ptyp_arrow ~loc Nolabel unit
          (ptyp_constr ~loc { loc; txt = lident ptype_name.txt } []);
      pval_loc = loc;
      pval_attributes = [];
      pval_prim = [];
    }

let generate_functions_intf type_declaration =
  [
    generate_of_env_function type_declaration;
    generate_of_type_function type_declaration;
  ]

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.concat_map ~f:(function
       | { ptype_kind = Ptype_variant _ | Ptype_open; _ } ->
           Location.raise_errorf ~loc
             "cannot support environment variables on this type, please use \
              either an abstract type or a record type"
       | type_decl -> generate_functions_intf type_decl)

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let deriver =
  Deriving.add "env" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator
