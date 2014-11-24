open Printf
open Cil
open Util

module E = Errormsg

let files = [ "test1.i"; "main.i" ]

type fun_arg_list_type = 
  { 
    fd: fundec;
    n_arg: int list
  }
type record = 
  {
    (* holds the function declaration which passes sensitive data *)
    mutable funList: fundec list;
    
    (* holds the variables which may contains sensitive data *)
    mutable varList: varinfo list;
    
    (* holds the function dec which passes sensitive data and if the n-th
     * argument is propagated *)
    mutable fun_arg_list: fun_arg_list_type list;
  }
  
class instrVisitor (class_type,last_record :string*record)= object (self)
  inherit nopCilVisitor
  
  (* the list holding possible variables *)
  val mutable varList : varinfo list = []

  (* maybe we don't need function list *)
  val mutable funList : fundec list = []

  val mutable fun_arg_list : fun_arg_list_type list = []

  (* the list holding  possible pointers *)
  val mutable pList : offset list = []

  method get_varList = varList

  method get_fun_arg_list = fun_arg_list

  method get_pList = pList

  method push_var (v) =(*E.log"pushed %s\n" v.vname;*) varList <- v::varList
 
  method push_fun e = funList <- e::funList; 

  method push_fun_arg_list arg_list = fun_arg_list <- arg_list::fun_arg_list;
  last_record.fun_arg_list <- fun_arg_list;

  method push_p p = pList <- p::pList;
  
  (* find if a varinfo exists in varList *)
  method find_var v =   
    if List.mem v varList then begin true  end else false

  (* find if an exp contains the pointer to {S} *)
  (* input: e exp 
   * output: true or false *)
  method find_exp e = 
     let rec match_e e = 
      let rec match_lv lv = 
          begin
          match fst lv with
          | Var(v) -> if List.mem v varList then true else false;
          | Mem(m) -> if List.mem (snd lv) pList then begin true end else match_e m;
          end
      in
        match e with
        | Lval(lv) -> match_lv lv;
        | SizeOfE(e) -> match_e e;
        | AlignOfE(e) -> match_e e;
        | UnOp( _, e, _) -> match_e e;
        | BinOp( _, e1, e2, _) -> match_e e1; match_e e2;
        | CastE( _, e) -> match_e e;
        | AddrOf(lv) -> match_lv lv;
        | StartOf(lv) -> match_lv lv;
        | _ -> false ;
    in
    match_e e

  (* find if a function's fd contains {S} *)
  (* input: function's e2 list 
   * output: the int list which contains the index of {S} in e2 list *)
  method find_s_in_fd( e2: exp list) = 
    let rec find_helper (e:exp list) (n:int) (result:int list)= 
      match e with
      | [] -> (*E.log" find_s_in_fd, length: %d\n"*) (List.length result);result;
      | head::tail -> if self#find_var ( self#find_var_in_exp head) then
        find_helper (tail) (n+1) (n::result) else find_helper (tail) (n+1)
        (result)
    in
    find_helper e2 0 [];

    (* push the argument into {S} according to index list of parameters in fd *)
  (* input: index list(int) of parameters and fd(fundex) of a function
   * output: unit *)
  method push_argument( index: int list) (e2: exp list) = 
    (*E.log "in push_argument \n";*)
    match index with
    | [] -> () (* nothing to push *)
    | head::tail -> let rec find_nth_in_exp exp n =
      match exp with
      | []-> () (* nothing to push *)
      | e_head::e_tail -> if n = 0 then begin (* push var in e_head *) (*E.log
      "e_head :%a \n" d_exp e_head; *)
        self#push_var (self#find_var_in_exp e_head) end else find_nth_in_exp
      e_tail (n-1)
    in
    (*E.log "in push_argument: index %d\n" head;*)
    find_nth_in_exp e2 head;

    (* dedup all lists and save these lists *)
  method get_result = 
    last_record.funList <- funList;
    last_record.varList <- varList;
    last_record.fun_arg_list <- fun_arg_list;

  (* dedup all lists *) 
  method dedup  = 
    let rec dedup_helper tmp_list = 
    match tmp_list with
    | [] -> []
    | head::tail -> if (List.mem head tail) then dedup_helper tail else
      head::(dedup_helper tail)
    in
    varList <- dedup_helper varList;
    funList <- dedup_helper funList;
    fun_arg_list <- dedup_helper fun_arg_list;
    pList <- dedup_helper pList;

    (* 
     * input: function name 
     * output: option of fundec related to that name
     *)
  method find_fundec_with_name( name:string ) = 
    let rec find_helper fd = 
      match fd with
      | [] -> None
      | head::tail -> if head.svar.vname = name then Some(head) else find_helper tail
    in 
    find_helper funList;

    (*
     * input: function argument( exp list) 
     *        arg name and type
     * output: the number list which represents the location of the argument in the
     * list *)
  method fun_arg_count (arg_list: exp list)  =
    let rec count_helper exp n result= 
      match exp with
      | [] -> (*E.log "in fun_arg_count: result length: %d\n" (List.length
      result);*)result 
      | head::tail ->
          let tmp_v = self#find_var_in_exp( head )
          in 
          if self#find_var tmp_v then 
            begin
              (*E.log "var: %s found\n" tmp_v.vname;*)
              (count_helper
              (tail) (n+1) (n::result));
              end
         else count_helper (tail)(n+1) result;
    in 
    count_helper (arg_list) (0) ([])

    (* input: an varinfo list e, an integer n
     * output: option of the n-th varinfo in the list *)
  method find_nth_in_list (v: varinfo list) ( n: int) = 
    let rec find_helper exp n = 
      match exp with 
      | [] -> None 
      | head::tail -> if n = 0 then begin
      Some(head) end else find_helper tail (n-1)
    in
    find_helper v n

    (* input: an lval
     * output: fst varinfo in the lval
     * *)
  method find_var_in_lval( l:lval) = 
    match fst l with
    |Var(v) -> v
    |Mem(m) -> self#find_var_in_exp m
 
 (* 
   * input: an exp e
   * output: fst varinfo in the exp *)
  method find_var_in_exp (e: exp) = 
    let rec match_e e = 
      let rec match_lv lv = 
        match fst lv with
        | Var(v) -> (* E.log "in find_var_in_exp: return varinfo %s\n" v.vname
        ;*) v
        | Mem(m) -> match_e m
      in
        match e with
        | Lval(lv) -> match_lv lv;
        | SizeOfE(e) -> match_e e;
        | AlignOfE(e) -> match_e e;
        | UnOp( _, e, _) -> match_e e;
        | BinOp( _, e1, e2, _) -> match_e e1; match_e e2;
        | CastE( _, e) -> match_e e;
        | AddrOf(lv) -> match_lv lv;
        | StartOf(lv) -> match_lv lv;
        | _ -> dummyFunDec.svar;
    in
    match_e e

  method vvdec( v: varinfo) :varinfo visitAction = 
   (*  E.log "in vvdec: %s\n" v.vname;*)
    let rec helper t =  
      match t with
      | TPtr( typ, addr ) -> helper typ;
      | TNamed(t_info, attr) -> (* E.log "in TNamed: %s\n" t_info.tname;*) ()
      | TComp(c_info, attr) -> 
          (*E.log "in TComp: %s\n" c_info.cname;*)
          if c_info.cname = class_type then self#push_var v else ();
      | _ -> ();
    in
    helper v.vtype;
    DoChildren;

  method vinst( i: instr) :instr list visitAction = 
      match i with
       | Set(lv, e, loc) -> 
         (*  E.log "in vinst SET(lv %a e:%a loc: %a\n" d_lval lv d_exp e d_loc
            loc;*)
           if self#find_var (self#find_var_in_exp e) || self#find_exp e 
           then begin
             (*E.log "    contains {S}\n"; *)
             (* should push the lval into lvalList *)
             (* this helper function should be refined base on a better
              * understanding of lval = lhost * loffset.
              * In another word, only check fst lv is not enough, offset matters
              * since the pointer may be the offset within an object *)
             let helper =
               let match_snd off =
                 match off with
                 | NoOffset -> false;
                 | Field(_,_) -> true;
                 (* if an lv is Var with index, it should be pushed into varList
                  * if an lv is Mem with index, how? same with Field 
                  * *)
                 | Index(_,_) -> true;
               in
               match fst lv with
               | Var(v) ->(* E.log "in var: %s\n" v.vname;*)self#push_var v;
               | Mem(m) -> if match_snd (snd lv) then self#push_p
               (snd lv) else begin 
                 let m_var = self#find_var_in_exp m
                 in
                 (*E.log" in mem without offset: %s\n" m_var.vname;*)
               self#push_var (self#find_var_in_exp m); end
               (* what if the lv is a pointer without offset? *)
             in
             helper;
             DoChildren
           end
           else
             begin
             DoChildren;
             end
       | Call( lv, e1, e2, loc) ->
       (* check e2(exp list), if e2 contains lval with sensitive varinfo *)
           let rec helper t_e2 =
             match t_e2 with
            | [] -> ()
            | head::tail -> 
                let arg_var = self#find_var_in_exp head
                and fun_var = self#find_var_in_exp e1 
                and n_arg = self#fun_arg_count e2 
                in
                if self#find_var arg_var
                then
                  begin
                    (* push fundec*n into fun_arg_list *)  
                      let rec helper_2 (tmp_n_arg:int list) = 
                        match tmp_n_arg with
                        |[] -> ()
                        |n::next -> 
                            fun_var.vname;
                          let rec fun_fd = self#find_fundec_with_name
                          fun_var.vname 
                          in
                          match fun_fd with
                          | None -> ()
                          | Some(fun_fd) ->
                            (* 1 *)
                              if List.mem {fd=fun_fd; n_arg=tmp_n_arg }
                              fun_arg_list then () else self#push_fun_arg_list { fd=fun_fd; n_arg =
                                tmp_n_arg };
                            (* 2 *)
                            let fun_arg = self#find_nth_in_list fun_fd.sformals n
                            in
                            (* print sformals *)
                            (*List.iter( function(t) -> E.log " %s " t.vname)
                             * fun_fd.sformals;*)
                            match fun_arg with
                            | None -> ()
                            | Some(v) -> (*E.log " push_var loc: %a\n" d_loc
                            loc ;*)self#push_var v;
                            helper_2 next;
                      in 
                      helper_2 n_arg;
                      (*E.log "\n";*)
                      (* 3 *)
                      match lv with
                      | None -> ()
                      | Some(lv) -> self#push_var (self#find_var_in_lval lv);      
                      (* 4 *)
                      (* E.log "***func: %a loc: %a\n" d_exp e1 d_loc loc;*)
                      self#push_argument n_arg e2;
                  end
                else helper tail;
           in
           helper e2;
           DoChildren
       | _ -> SkipChildren

  method print_func_argument( v_list: varinfo list) =
    let rec helper t_list = 
      match t_list with
      | [] -> (*E.log "\n"*)()
      | head::tail -> (*E.log "%s " head.vname;*) helper tail;
    in
    helper v_list

  method vfunc(fd :fundec): fundec visitAction = 
    self#push_fun fd;
    DoChildren

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
let () =  let files = input_file "openssl-files" in 
 let  files = List.map( fun filename -> let f = Frontc.parse filename in
f() ) files in let file = Mergecil.merge files "test" in
Rmtmps.removeUnusedTemps file;

Cfg.computeFileCFG file;

(* do the AST analysis *)
  let last_record = {funList=[]; fun_arg_list=[]; varList=[]} and last_record2 =
    {funList=[]; fun_arg_list=[]; varList=[]}
in
 let vis = new instrVisitor("rsa_st",last_record)
in
ignore(visitCilFile (vis:> cilVisitor) file);
vis#dedup;
vis#get_result;
while (List.length last_record.varList <> List.length last_record2.varList)
&&(List.length last_record.fun_arg_list <> List.length last_record2.fun_arg_list) 
do
  last_record2.fun_arg_list <- last_record.fun_arg_list;
  last_record2.varList <- last_record.varList; 
  ignore(visitCilFile (vis:> cilVisitor) file);
  vis#dedup;
  vis#get_result;
done;

(* print out the result *)
E.log "---------------Begin print varList-------------\n";
List.iter (function(head) -> E.log "%s:%a \n" head.vname d_loc head.vdecl) 
vis#get_varList;
E.log "\n--------------------fun arg ------------\n";
List.iter (function(e) -> E.log "\n%s \n" e.fd.svar.vname )
last_record.fun_arg_list;
E.log "\n------------------pointer list------------------\n";
List.iter (function(p) -> match p with Field(f,o) -> E.log "\n%s\n" f.fname | _
->()) vis#get_pList;
