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
 val mutable  memList : exp list = []

  method get_var = varList

  method push_var (v) = varList <- v::varList
 
  method push_fun e = funList <- e::funList

  method push_mem e = memList <- e::memList
  
  method find_var v target_list =   
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
        helper target_list;
  
  method find_mem v target_list =   
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
        helper target_list;

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
    memList <- dedup_helper memList;

  method print_funList = 
    let rec print_helper (funList: exp list)= 
      match funList with
      | [] -> ()
      | head::tail -> E.log "%a\t" d_exp head; print_helper tail
    in
    print_helper funList;
    E.log "\n----------------------------------------------\npointer:\n";
    print_helper memList;
  
  method print_list = 
    let rec print_helper (varList: varinfo list)= 
      match varList with
      | [] -> ()
      | head::tail -> E.log "%s:%a \t" head.vname d_loc head.vdecl; print_helper tail
    in
    print_helper varList

  method vinst( i: instr) :instr list visitAction = 
      match i with
       | Set(lv, e, loc) -> 
           let rec match_helper l = match fst l with 
           (* get the pointer, such as int**ret = NULL; *ret = XXX; *)
           | Mem(m) -> 
               let rec helper1 m = 
                 match m with
                 | Lval(lv) ->
                     let helper2 l = match fst l with
                     | Mem(m2) -> helper1 m2 
                     | Var(v) -> if v.vname <> top_vname then () else
                       self#push_mem m
                     in
                     helper2 lv
                 | _ -> ()
               in
               helper1 m
           | Var(v) -> if v.vname <> top_vname then () else self#push_var v;
           in
           match_helper lv;
         (* should check the rval and ignore the lval *) 
         let varVisitor = object
           inherit nopCilVisitor 
           method vvrbl v =  
               match fst lv with
              |Var(lv) -> if( self#find_var lv varList) then 
                begin 
                  self#push_var lv;
                  DoChildren;
                end else DoChildren;
              |Mem(m) ->  if (self#find_mem m memList) then  
                begin 
                  self#push_mem m;
                  DoChildren; 
                end else DoChildren;
             
         end in
         ignore(visitCilExpr ( varVisitor :> cilVisitor ) e);DoChildren
       | Call( lv, e1, e2, loc) ->
           (*check if parameters contains sensitive data *)
           let rec traverseE2_helper (e2_tmp: exp list) = 
            match e2_tmp with
            | [] -> ()
            | head::tail -> 
                let varVisitor = object 
                  inherit nopCilVisitor
                  method vvrbl v = 
                    match lv with
                    | Some c -> 
                        let helper = 
                          match fst c with
                        | Var(va) -> 
                            if(self#find_var v varList) then
                             begin 
                               self#push_var va; 
                               DoChildren;
                             end
                            else DoChildren;
                        | Mem(m) -> self#push_mem m; DoChildren
                        in 
                        helper 
                    | None -> if( self#find_var v varList) then self#push_fun e1;
                    DoChildren
                  end
                in
                ignore(visitCilExpr (varVisitor :> cilVisitor ) head);
                traverseE2_helper tail;
           in
           traverseE2_helper e2;
           let helper2 = 
           match e1 with
           | Lval(lv2 ) -> 
               let helper3 = 
               match fst lv2 with 
               | Mem(m) ->()  
               | Var(v) -> 
                 if v.vname = top_funcname 
                 then self#push_fun e1 
                 else if self#find_var v varList then self#push_var v;
               in helper3;
           | _ ->();
           in
           helper2;
           if self#find_mem e1 funList then 
             begin
             match lv with
             | Some c -> 
                 let helper4 = 
                   match fst c with
                   | Var(va) -> self#push_var va;
                   | Mem(m) -> self#push_mem m;
                 in 
                 helper4;DoChildren
            |None -> ();
            
             DoChildren; 
             end else DoChildren
       | _ -> SkipChildren
end     

let processInitial( tf :string ) (fd:fundec) (log : location) (vis: cilVisitor) :unit
=
  if fd.svar.vname <> tf then () else begin 
    ignore(visitCilFunction vis fd)
  end

(*------------------------------------ begin ------------------------------------*)

(* load all *.i file *)
let input_file filename = 
  let chan = open_in filename in
  let lines = ref [] in
  try while true; do lines := input_line chan :: !lines done; []
  with
   End_of_file -> close_in chan; List.rev !lines

(* construct CIL data structure *)
let () =  let files = input_file "openssl-files" in 
 let  files = List.map( fun filename -> let f = Frontc.parse filename in
f() ) files in let file = Mergecil.merge files "test" in
Rmtmps.removeUnusedTemps file;

Cfg.computeFileCFG file;

(* do the AST analysis *)
let vis = new instrVisitor("privatekey","PEM_read_bio_PrivateKey") in
 List.iter( 
   function
     |GFun ( fundec, loc) ->
    (* find PEM_read_bio_PrivateKey in Global variables *)
    (* the goal of this line is to initialize varList and memList *)
         E.log "func: %s\n" fundec.svar.vname;
         processInitial ("main") fundec loc (vis:>cilVisitor)
     | _ -> () ) file.globals;
ignore(visitCilFile (vis:> cilVisitor) file);

(* print out the result *)
vis#dedup;
E.log "---------------Begin print varList-------------\n";
vis#print_list;
E.log "\n---------------Begin print funList-------------\n";
vis#print_funList;
E.log "\n---------------end---------------------------\n"
