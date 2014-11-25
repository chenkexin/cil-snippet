
open Printf
open Cil
open Util

module E = Errormsg

let files = ["test1.i"; "main.i"]

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

  method vinst( i: instr) :instr list visitAction = 
    match i with
    | Set( lv, e, loc) -> E.log "in vinst.Set: right exp %a loc %a\n" d_exp e d_loc
    loc; E.log "\t lval: %a\n" d_lval lv;DoChildren
    | Call(lv, e1, e2, loc) -> E.log "in vinst.Call: e1 %a loc %a\n" d_exp e1
    d_loc loc; DoChildren
    | _ -> E.log" in vinst._\n"; DoChildren

  method voffs( o: offset): offset visitAction = 
    match o with
    | NoOffset -> DoChildren;
    | Field(f,o) -> E.log "in Voffset, the host: %s and the offset: %s\n"
    f.fcomp.cname f.fname; DoChildren;
    | Index(e,o) -> DoChildren;
    
  method vvdec( v: varinfo ): varinfo visitAction = 
    E.log "in vvdec: name: %s type:%a \n" v.vname d_type v.vtype ;DoChildren

  method vvrbl( v: varinfo): varinfo visitAction = 
    E.log "in vvrbl: %s\n" v.vname;DoChildren
  
  method lval( v:varinfo): varinfo visitAction = 
    E.log "in lval: %s\n" v.vname;DoChildren

  (* method vfunc( fd: fundec) fundec visitAction = DoChildren *)
end

let () = let files = List.map( fun filename -> let f = Frontc.parse filename in
f() ) files in let file = Mergecil.merge files "test" in 
Rmtmps.removeUnusedTemps file;
Cfg.computeFileCFG file;

let vis = new visitor("test","test") 
in
ignore(visitCilFile (vis:> cilVisitor) file); 
