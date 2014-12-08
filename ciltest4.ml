
open Printf
open Cil
open Util

module E = Errormsg

let files = ["test_macro.i"]

class visitor( target_vname, target_funcname: string*string) = object(self)
  inherit  nopCilVisitor
  val mutable varList : varinfo list = []
  val mutable funList: fundec list =[]
  val mutable memList: exp list = []

  method push_var v  =  varList <- v::varList

  method find_var v = 
      let rec helper t = 
        match t with
        | [] ->false
        |head::tail -> if(Util.equals head v) then true else helper tail
      in
      helper varList;

  method dedup = 
    let rec dedup_helper tmp_list = 
      match tmp_list with
      |[] -> []
      | head::tail -> if( List.mem head tail) then dedup_helper tail else
        head::(dedup_helper tail)
    in
    varList <- dedup_helper varList;
    funList <- dedup_helper funList;
    memList <- dedup_helper memList;

  method print_List = ()

  method print_type t =
    E.log "type: %a\n" d_type t;
  method print_const c = 
    E.log "const: %a\n" d_const c;
  method print_unop u =
    E.log "unop: ignore\n";
  method print_binop b = 
    E.log "binop: ignore\n";
  method print_string s = 
    E.log "string: %s\n" s;

  method introspect_exp (e:exp) = 
    let rec match_e e = 
      let rec print_lv lv = 
        (* E.log "in match lv: %a\n" d_lval lv; *)
        let match_lv_fst  t_lv = 
          match fst t_lv with
          | Var(v) -> E.log "var: %s\n" v.vname
          | Mem(m) -> match_e m
        in
        let  match_lv_snd t_lv = 
          let rec offset_helper o = 
            match o with
            |NoOffset -> ();
            |Field(f, t_o) -> E.log "field: %s\n" f.fname; offset_helper t_o;
            |Index(e, t_o) -> match_e e; offset_helper t_o;
          in
          offset_helper (snd t_lv);
        in
        match_lv_fst lv;
        match_lv_snd lv;
      in
      match e with
      | Const(c) -> self#print_const c;
      | SizeOf(t) -> self#print_type t;
      | Lval(lv) -> print_lv lv;
      | SizeOfE(e) ->match_e e;
      | AlignOfE(e) -> match_e e;
      | SizeOfStr(s) -> self#print_string s;
      | UnOp(u,e,t) -> 
          self#print_unop(u);  
          match_e e; 
          self#print_type t;
      | BinOp(b,e1,e2,t) ->
          self#print_binop(b); 
          match_e e1; 
          match_e e2;
          self#print_type t;
      | CastE(t,e) ->
          self#print_type t;
          match_e e;
      | AddrOf(lv) -> print_lv lv;
      | StartOf(lv) -> print_lv lv;
      | AlignOf(t) -> self#print_type t;
    in
    match_e e;

  method vinst( i: instr) :instr list visitAction = 
    match i with
    | Set( lv, e, loc) ->E.log "loc: %a\n" d_loc loc; self#introspect_exp e; DoChildren
    | Call(lv, e1, e2, loc) -> E.log "in vinst.Call: e1 %a loc %a\n" d_exp e1
    d_loc loc; DoChildren
    | _ -> E.log" in vinst._\n"; DoChildren


  (* method vfunc( fd: fundec) fundec visitAction = DoChildren *)
end

let () = let files = List.map( fun filename -> let f = Frontc.parse filename in
f() ) files in let file = Mergecil.merge files "test" in 
Rmtmps.removeUnusedTemps file;
Cfg.computeFileCFG file;

let vis = new visitor("test","test") 
in
ignore(visitCilFile (vis:> cilVisitor) file); 
