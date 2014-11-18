open Printf
open Cil
open Util

module E = Errormsg

let files = [ "test1.i"; "main.i" ]
  
class instrVisitor (top_vname ,top_funcname:string*string) = object (self)
  inherit nopCilVisitor
  
(* the list holding possible variables *)
 val mutable varList : varinfo list = []
 val mutable  funList : exp list = []

  method get_var = varList

  method push_var (v) = varList <- v::varList
 
  method push_fun e = funList <- e::funList

  method find_var v =   
        let rec helper t =
          match t with 
          | [] -> false
              (* = : deep equation *)
              (* == : shallow equation *)
              (* if v.vname = "pkey" then true else false *)
          | head::tail -> 
              if (Util.equals head v)  then true
              else helper tail
        in
        helper varList;
  
  method dedup  = 
    (* varList <- List.sort_uniq (fun (v1) (v2)->if v1 = v2 then 0 else if v1 >v2
    then 1 else -1) varList; 
    funList <- List.sort_uniq (fun (v1) (v2)->if v1 = v2 then 0 else if v1 >v2
    then 1 else -1) funList *)
    let rec dedup_helper tmp_list = 
    match tmp_list with
    | [] -> []
    | head::tail -> if (List.mem head tail) then dedup_helper tail else
      head::(dedup_helper tail)
    in
    varList <- dedup_helper varList;
    funList <- dedup_helper funList;

  method print_funList = 
    let rec print_helper (funList: exp list)= 
      match funList with
      | [] -> ()
      | head::tail -> E.log "%a\t" d_exp head; print_helper tail
    in
    print_helper funList;
  
  method print_list = 
    let rec print_helper (varList: varinfo list)= 
      match varList with
      | [] -> ()
      | head::tail -> E.log "%s:%a \t" head.vname d_loc head.vdecl; print_helper tail
    in
    print_helper varList

  method vinst( i: instr) :instr list visitAction = 
      match i with
       | Set(lv, e, loc) -> DoChildren 
       | Call( lv, e1, e2, loc) -> DoChildren
       | _ -> SkipChildren
end     

(*------------------------------------ begin ------------------------------------*)

(* load all *.i file in openssl-file *)
 let input_file filename = 
  let chan = open_in filename in
  let lines = ref [] in
  try while true; do lines := input_line chan :: !lines done; []
  with
   End_of_file -> close_in chan; List.rev !lines


(* construct CIL data structure *)
let () =  (* let files = input_file "openssl-files" in *)
 let  files = List.map( fun filename -> let f = Frontc.parse filename in
f() ) files in let file = Mergecil.merge files "test" in
Rmtmps.removeUnusedTemps file;

Cfg.computeFileCFG file;

(* do the AST analysis *)
let vis = new instrVisitor("privatekey","PEM_read_bio_PrivateKey") 
in 
ignore(visitCilFile (vis:> cilVisitor) file);

(* print out the result *)
vis#dedup;
E.log "---------------Begin print varList-------------\n";
vis#print_list;
E.log "\n---------------Begin print funList-------------\n";
vis#print_funList;
E.log "\n---------------end---------------------------\n"
