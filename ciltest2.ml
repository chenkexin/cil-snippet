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
    mutable fun_arg_list: fundec list;

    mutable pList:offset list;

    mutable resultList: location list;
    mutable lvList: lval list;
  }
  
class instrVisitor (class_type,offset_name,snd_offset_name,last_record,oc
:string*string*string*record*out_channel)= object (self)
  inherit nopCilVisitor
  
  (* the list holding possible variables *)
  val mutable varList : varinfo list = last_record.varList;

  (* maybe we don't need function list *)
  val mutable funList : fundec list = []

  val mutable fun_arg_list : fun_arg_list_type list = []

  val mutable t_fun_arg_list : fundec list = []

  (* the list holding  possible pointers *)
  val mutable pList : offset list = last_record.pList;

  method get_varList = varList

  method get_fun_arg_list = fun_arg_list

  method get_funList = funList

  method get_pList = pList

  method get_t_fun_arg_list = t_fun_arg_list

  method push_var (v) =(*E.log"pushed %s\n" v.vname;*) varList <- v::varList
 
  method push_fun e = funList <- e::funList; 

  method push_fun_arg_list arg_list = fun_arg_list <- arg_list::fun_arg_list;
 (* last_record.fun_arg_list <- fun_arg_list; *)

  method push_p p = pList <- p::pList;
  
  method find_var v = if List.mem v varList then true else false

  (* find if a varinfo with off exists in varList *)
  method find_var_with_off v off=   
    let helper =
      match off with
      | NoOffset-> false;
      | _ -> true;
    in
    if (List.mem v varList) && (helper) then begin true  end else false

  (* find if an offset in pList *)
  method find_offset p =
    let helper p = 
      match p with
      | NoOffset -> false;
      | _ ->true;
    in
    if (List.mem p pList) && (helper p) then begin true end else false

  method find_lv (lv:lval) = 
    self#find_offset (snd lv)

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
      | head::tail -> 
          let c_var = self#find_var_in_exp head 
          in
          match c_var with
          | None -> []
          | Some(c_var ) -> 
          if self#find_var c_var
          then
          find_helper (tail) (n+1) (n::result) else find_helper (tail) (n+1) (result)
    in
    find_helper e2 0 [];

  method print_before_push_var c_var loc p_var = 
     E.log "push %s(dec loc:%a) in %a <- %s(dec loc:%a)\n" c_var.vname d_loc
     c_var.vdecl d_loc loc p_var.vname d_loc p_var.vdecl;
  
     (* push the argument into {S} according to fd *)
  (* input: index list(int) of parameters and fd(fundex) of a function
   * output: unit *)
  method push_argument( fd: fundec) (e2: exp list) (loc:location) = 
    (*E.log "in push_argument \n";*)
    let rec helper n v_list =
    match v_list with
    | [] -> () (* nothing to push *)
    | head::tail -> if self#find_var head 
      then 
        let c_var = (self#find_var_in_exp (List.nth e2 n)) 
        in
        match c_var with
        | None -> ()
        | Some(c_var) ->
        self#print_before_push_var c_var loc head; 
        self#push_var c_var;
      else helper (n+1) (tail);
    in
    helper 0 fd.sformals;

  (* dedup all lists and save these lists *)
  method get_result = 
    last_record.funList <- funList;
    last_record.varList <- varList;
    last_record.fun_arg_list <- t_fun_arg_list;
    last_record.pList <- pList;

   (* extract fun fd in fun_arg_list *)
   method extractor =
     let rec helper t_fun_arg  (result:fundec list)= 
      match t_fun_arg with
      | [] -> t_fun_arg_list <- result
      | head::tail -> helper (tail) (head.fd::result)
     in
     helper fun_arg_list [];
    
  (* This method check lvalue and do corresponding operations.
     * if lvalue is a var, then push var into sensitive list.
     * if lvalue is a memory reference, then check if the Mem(m) has offset, if
       * so, push the offset into the pList. If not, deal it as var. *)
    (* input: lvalue
     * loc: calling location
     * output: unit *)
  method process_set(lv: lval) loc =
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
      | Var(v) ->
          self#print_before_push_var v loc v;
          (*E.log "push var: %s \n" v.vname ;*) self#push_var v;
      | Mem(m) -> if match_snd (snd lv) then self#push_p
      (snd lv) else 
      begin 
      let m_var = self#find_var_in_exp m
      in
      match m_var with
      | None -> ()
      | Some( m_var ) ->
          self#print_before_push_var m_var loc m_var;
      self#push_var m_var; 
      end
    in
    helper;
 
  method find_lv_in_exp( e:exp) =
    let rec match_e e = 
      match e with
      | Lval(lv) -> Some(lv);
      | SizeOfE(e) -> match_e e;
      | AlignOfE(e) -> match_e e;
      | UnOp( _, e, _) -> match_e e;
      | BinOp( _, e1, e2, _) -> match_e e1; match_e e2;
      | CastE( _, e) -> match_e e;
      | AddrOf(lv) -> Some(lv);
      | StartOf(lv) -> Some(lv);
      | _ -> None;
    in
    match_e e

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
    t_fun_arg_list <- dedup_helper t_fun_arg_list;
    pList <- dedup_helper pList;

  (* 
   * input: function name 
   * output: option of fundec related to that name *)
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
  (* the function must recognize not only var_in_exp but also offset_in_exp 
   * this is the same with how Call do with l-value *)
  method fun_arg_count (arg_list: exp list)  =
    let rec count_helper exp n result= 
      match exp with
      | [] -> 
      result 
      | head::tail ->
          let tmp_v = self#find_var_in_exp( head ) and tmp_lv =
            self#find_lv_in_exp( head )
          in
          match tmp_lv with
          | None ->
              let local tmp_v =  
                 match tmp_v with
                 | None -> count_helper (tail) (n+1) result; 
                 | Some(v) -> 
                 if (self#find_var v)  then 
                 (count_helper (tail) (n+1) (n::result))
                 else count_helper (tail)(n+1) result;
              in 
              local tmp_v;
          | Some(lv) ->
              let local tmp_v =
               match tmp_v with 
              | None -> []
              | Some (v) -> 
              if (self#find_var v) || (self#find_lv lv) then count_helper (tail) (n+1) (n::result)
              else count_helper (tail)(n+1) result;
              in 
              local tmp_v;
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
   *)
  method find_var_in_lval( l:lval) = 
    match fst l with
    |Var(v) -> Some(v)
    |Mem(m) -> self#find_var_in_exp m

  method process_function_call (arg_var:varinfo) (fun_var:
    varinfo) (n_arg: int list) (lv:lval option) (e2:exp list) (loc:location)
    =
     let rec helper_2 (tmp_n_arg:int list) = 
       match tmp_n_arg with
       |[] -> ()
       |n::next -> 
           let rec fun_fd = self#find_fundec_with_name
         fun_var.vname 
       in
         match fun_fd with
         | None -> ()
         | Some(fun_fd) ->
             (* 4 *)
             self#push_argument fun_fd e2 loc;
           (* 1 *)
             if List.mem {fd=fun_fd; n_arg=tmp_n_arg }
             fun_arg_list then () else self#push_fun_arg_list { fd=fun_fd; n_arg =
               tmp_n_arg };
           (* 2 *)
           let fun_arg = self#find_nth_in_list fun_fd.sformals n
           in
           match fun_arg with
           | None -> ()
           | Some(v) -> (*E.log " push_var loc: %a\n" d_loc
           loc ;*)
           self#print_before_push_var v loc arg_var;
             self#push_var v;
           helper_2 next;
           in 
     helper_2 n_arg;
     (* 3 *)
     match lv with
     | None -> ()
     | Some(lv) -> self#process_set lv loc ; 
  (* 
  * input: an exp e
  * output: fst varinfo in the exp, it's option *)
  method find_var_in_exp (e: exp) = 
    let rec match_e e = 
      let rec match_lv lv = 
        match fst lv with
        | Var(v) -> (* E.log "in find_var_in_exp: return varinfo %s\n" v.vname
        ;*) Some(v)
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
        | _ -> None;
    in
    match_e e

    (* initial set*)
  method voffs( o:offset ) : offset visitAction = 
    let rec offset_helper (o:offset) = 
      match o with
    | NoOffset -> DoChildren;
    | Field(f,off) ->
        if (f.fcomp.cname = class_type) && ( f.fname = offset_name) 
        then
          begin
            if List.length varList = 0 
            then 
              begin
              self#push_p o;
              DoChildren;
              end
            else DoChildren;
          end
        else 
          DoChildren;
    | Index(i,o) -> DoChildren;
    in
    offset_helper o; 

  method vinst( i: instr) :instr list visitAction =  
    match i with
       | Set(lv, e, loc) ->
           let helper = 
           let c_var = self#find_var_in_exp e
           in 
           match c_var with
           | None -> DoChildren;
           | Some(c_var) ->
               let local c_var = 
                  if self#find_var c_var
                  then self#process_set lv loc
                  else
                  if self#find_exp e
                  then
                  self#process_set lv loc
                  else ()
               in 
               local c_var; 
               DoChildren;
           in 
           helper;
       | Call( lv, e1, e2, loc) ->
           (* check e2(exp list), if e2 contains lval with sensitive varinfo *)
           let rec helper t_e2 =
             match t_e2 with
            | [] -> ()
            | head::tail ->
                (* should first check if the exp contains {S} *)
                let arg_var = self#find_var_in_exp head
                and arg_lv = self#find_lv_in_exp head
                and fun_var = self#find_var_in_exp e1 
                and n_arg = self#fun_arg_count e2 
                in
                match arg_lv with
                | None -> (); 
                | Some(t_arg_lv) ->
                    let local = 
                    match arg_var with
                    | None -> ()
                    | Some(arg_var) ->
                       if (self#find_var arg_var) || (self#find_lv t_arg_lv)(* and should also check offsets *)
                    then
                      self#match_fun_var fun_var arg_var n_arg lv e2 loc
                       else helper tail;
                    in
                    local;
           in
           helper e2;
           DoChildren
       | _ -> SkipChildren

  method match_fun_var fun_var arg_var n_arg lv e2 loc = 
    match fun_var with
    | None -> ()
    | Some(fun_var)-> 
        self#process_function_call arg_var fun_var n_arg lv e2 loc
  
    method print_func_argument( v_list: varinfo list) =
    let rec helper t_list = 
      match t_list with
      | [] -> (*E.log "\n"*)()
      | head::tail -> (*E.log "%s " head.vname;*) helper tail;
    in
    helper v_list

  method vfunc(fd :fundec): fundec visitAction = 
    self#push_fun fd;
    let offsetVisitor = object
      inherit nopCilVisitor
      method voffs( o: offset) : offset visitAction = 
        if (List.mem o pList) && (self#find_offset o) 
        then
        self#push_fun_arg_list {fd = fd; n_arg = []};
      fd.svar.vname;DoChildren;
    end in
    ignore(visitCilBlock (offsetVisitor :> cilVisitor) fd.sbody );
    
    DoChildren

end     

(* this visitor should check p->d based on previous result*)
class sndVisitor( snd_offset_type,snd_offset_name,last_record,oc:string*string*record*out_channel) 
= object (self)
  inherit nopCilVisitor

  val mutable varList: varinfo list = []
  val mutable pList: offset list = []
  val mutable lvList: lval list =[]

  val mutable resultList : location list =[]

  method get_result = 
    last_record.varList <- varList;
    last_record.pList <- pList;
    last_record.lvList<- lvList;
  (* dedup all lists *) 
  method dedup  = 
    let rec dedup_helper tmp_list = 
    match tmp_list with
    | [] -> []
    | head::tail -> if (List.mem head tail) then dedup_helper tail else
      head::(dedup_helper tail)
    in
    varList <- dedup_helper varList;
    pList <- dedup_helper pList;
  
  method find_var_in_lv( l:lval) = 
    match fst l with
    |Var(v) -> Some(v)
    |Mem(m) -> self#find_var_in_exp m
  method push_var v = 
    varList <- v::varList;
  method push_p o = 
    pList <- o::pList;
  method push_result loc = 
    resultList <- loc::resultList;

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

  method find_var_in_lval (lv:lval) = 
    let rec local lv = 
      match fst lv with
      | Var(v) -> Some(v);
      | Mem(m) -> 
          let tmp = self#find_lval_in_exp m 
          in
          match tmp with
          | None -> None;
          | Some(tmp) -> local tmp;
    in
    local lv;

  method find_p_in_lval (lv:lval) = 
    let rec local lv = 
      match snd lv with
      | NoOffset -> None;
      | Field(f,o) -> Some(o);
      | Index( e, o) -> Some(o);
    in
    local lv;

  method find_lval_in_exp (e:exp) = 
    let rec match_e e = 
      match e with
      | Lval(lv) -> Some(lv);
      | SizeOfE(e) -> match_e e;
      | AlignOfE(e) -> match_e e;
      | UnOp( _, e, _) -> match_e e;
      | BinOp( _, e1, e2, _) -> match_e e1; 
      | CastE( _, e) -> match_e e;
      | AddrOf(lv) -> Some(lv);
      | StartOf(lv) -> Some(lv);
      | _ -> None;
    in
    match_e e;

    (* check if (snd lv) indicate (what)->d *)
  method local_field_helper f loc=
   match f with
   | NoOffset -> false;
   | Field(f, o) -> if (f.fname = snd_offset_name)
        then
         begin
           (*self#match_offset o loc;*)
           self#match_offset o loc;
           true;
         end
        else false;
   | Index(e, o) -> let tmp_v = self#find_var_in_exp e 
    in 
    match tmp_v with
    | Some(tmp_v) -> self#push_result loc; false;
    |None -> self#push_result loc; false;
  method match_offset o loc = 
   match o with
   | Index( e,o) -> self#push_result loc; true;
   | _ -> false;

  method local_var_off_helper off loc = 
    if List.mem off last_record.pList  
    then 
      begin
      self#local_field_helper off loc 
      end 
   else false

   method check_fst v loc=
     match v with
     | Var(v) ->  
         if List.mem v last_record.varList 
         then true
         else false;
     | Mem(m) ->
           (* should check if the exp is *->d 
            * such as: bn_div.c(278): 
              * d0 = sdiv->d[div_n-2]; *)
           (* *(sdiv->d + (div_n -1 )) *)
           let tmp = self#find_lval_in_exp m
         in
         let local = 
         match tmp with
         | Some(tmp) -> 
             (* fst tmp is Mem(sdiv) and snd tmp is (D) *)
             let match_sdiv = 
               match fst tmp with
               | Var(v) -> false;
               | Mem(m) -> 
                   let tmp = self#find_var_in_exp m
                   in
                   match tmp with
                   |Some(v) -> if List.mem v last_record.varList then begin
                     true;end else
                     false;
                   |None -> false;
             in
             if match_sdiv && self#local_field_helper (snd tmp) loc
             then 
               begin
                 let tmp = (self#find_var_in_lv tmp)
                 in 
                 match tmp with
                 | Some(v) -> self#push_var v;true;
                 | _ -> ();true;
               end
             else false;
         | None -> false;
         in
         local;
 
  (* check if exp contains the p->d *) 
  method process_exp tmp loc =
    let tmp_lv = self#find_lval_in_exp tmp 
    in       
    match tmp_lv with
           | Some(lv)->
               if (self#check_fst (fst lv) loc)  
               then 
                 begin
                   if (self#local_field_helper (snd lv) loc )
                   then 
                     begin 
                     (*self#push_lv lv;*)
                       let t_v = self#find_var_in_lv lv
                       in
                       match t_v with
                       | Some(v) ->
                           self#push_var v;
                       | None -> ();
                      end
                   else (); 
                 end
               else if (self#local_var_off_helper (snd lv) loc) 
                   then 
                     begin 
                       let t_v = self#find_var_in_lv lv
                       in
                       match t_v with
                       | Some(v) -> 
                          self#push_var v;
                       | None -> ();
                 end
                 else ();
           | None -> (); 

  
  method find_var_in_exp (e: exp) = 
    let rec match_e e = 
      let rec match_lv lv = 
        match fst lv with
        | Var(v) -> Some(v)
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
        | _ -> None;
    in
    match_e e

  method push_lv (lv:lval) = lvList <- lv::lvList;
  method get_varList = varList;  
  method get_pList = pList; 
  method get_lvList = lvList;


  method process_lv (lv:lval) loc =
      if (self#check_fst (fst lv) loc)  
      then 
        begin
          if (self#local_field_helper (snd lv) loc )
          then self#push_lv lv
        else (); 
        end
      else if (self#local_var_off_helper (snd lv) loc) 
          then self#push_lv lv
        else ();

  method vinst( i: instr) : instr list visitAction = 
    match i with
    | Set( lv, e, loc) ->
        self#process_exp e loc;
        self#process_lv lv loc;
        DoChildren;
    | Call(lv, e1, e2, loc) ->
        let local =  
        match lv with
        | Some(lv) ->DoChildren;
        |None -> DoChildren;
        in
        local;
    | _ -> DoChildren;
end


(* this class should check last process's result *)
(* last: p->d *)
(* this: should check d[i] *)
class checkArrayVisitor ( last_record,oc: record*out_channel ) = object (self)
  inherit nopCilVisitor
  val mutable array_varList : varinfo list =  []
  val mutable array_resultList : location list =  []
  val mutable tmpList: varinfo list = []


  method push_tmp t = tmpList <- t::tmpList;
  method push_v v = array_varList <- v::array_varList;
  method push_result r = array_resultList <- r::array_resultList;
  method get_result = 
    last_record.varList <- array_varList;
    last_record.resultList <- array_resultList;
  method get_varList = array_varList;
  method get_tmpList = tmpList;

  (* this is the helper function which checks if *(dst+i) *)
  method check_var_star_d e1 e2 =
  (*this is the check for *(dst) *)
  (*but we also need *(sdiv->d) *)
    let tmp_var = self#find_var_in_exp e1
     in   
     let check_e1_e2 = 
     match tmp_var with
     | Some(v) ->
         if List.mem v last_record.varList && ((self#check_type v "BIGNUM *") ||
         (self#check_type v "unsigned long *"))
         then self#check_e2 e2 
         else false;
     | None -> false;
     in
     check_e1_e2;

     (* check if the type is $s type *)
  method check_type (v:varinfo) (s:string) = 
    match v.vtype with
    | TComp(c,a) -> 
        if c.cname = s then begin(* E.log "comp name: %s\n" s;*) true end else false;
    | TNamed(t,a) -> 
        if t.tname = s then begin(* E.log "type name: %s\n" s;*) true end else false;
    | _ -> false;

     (* check if the snd exp inside an BinOp(_,e1,e2,_) has form of A + Const
      * this form is used in checking pointer operation 
      * such as p->d[i] or dst++. *)
  method check_e2 e2 =
      match e2 with
      | Const(_) ->true;
      | Lval(l) ->
          let tmp = 
            match fst l with
            | Var(v) ->true;
            | Mem(m) ->
                let mem_helper (o:offset) = 
                  match o with
                  | Field(f,o) -> let field_helper =
                    match f.ftype with
                    | TInt(c,a) ->true; 
                    | _->false;
                  in
                  field_helper;
                  | _->false;
                in
                mem_helper (snd l);
          in
          tmp;
      | BinOp(_,t_e1,t_e2,_) ->
          (* like d->[div_n + 1], the e2 again becomes an BinOp *)
          if (self#check_e2 t_e1) && (self#check_e2 t_e2) then true else false;
      | _ -> false;
  
      (* check if e1 is sdiv->d.
       * if the e1 is valid return true
       * else return false *)
  method check_e1 e1 = 
    (* e1 = sdiv->d *)
    let tmp_lv = self#find_lv_in_exp e1 
    in
    match tmp_lv with
    | Some(lv) -> 
        let helper lv = 
          match fst lv with
          | Var(v) -> true;
          | Mem(m)-> 
              let local_mem = 
                let tmp = self#find_lv_in_exp m
                in
                match tmp with
                | Some(tmp_lv) -> 
                    let local_lv = 
                    match fst tmp_lv with
                    | Var(v) -> 
                    if (List.mem v last_record.varList) then begin true end else false;
                    | Mem(mm) -> false;
                    in
                    local_lv;
                | None -> false;
              in
              if local_mem then 
                  if self#match_snd (snd lv) then true else false
              else false
        in
        helper lv;
    | None -> false;
  
  method check_lval_star_d e1 e2 = 
    if self#check_e1 e1 
    then 
       if self#check_e2 e2 then true else false
    else false;
 
 (* give a lval, judge if the lval is *(ap++) or *(sdiv->d) + n *)
  method match_exp (some_v:lval) loc =
        let helper2 v = 
        match v with
        | Mem(m) -> 
        (* this exp contains *tmp___3 *)
        let local = 
          match m with
          | BinOp(_,e1,e2,_) -> 
              (* e1 should be in {s}  and e2 should be a const *)
              if self#check_var_star_d e1 e2 
              then 
                  self#check_var_star_d e1 e2
              else
              self#check_lval_star_d e1 e2;
          | Lval(l)->
              let helper = 
                match fst l with
                | Var(v)-> false;
                | _ -> false;
              in
              helper;
              (*self#match_exp l loc; *)
          | _-> false;
        in
        local;
        | Var(v) ->false;
        in
        helper2 (fst some_v);

    (* check if snd part of an lval is ->d *)
  method match_snd (s:offset) = 
    match s with
    | NoOffset -> false;
    | Field(f,o) -> 
        if f.fname = "d" then begin true end else false;
    |Index(e,o)-> true;

  method find_var_in_exp (e: exp) = 
    let rec match_e e = 
      let rec match_lv lv = 
        match fst lv with
        | Var(v) -> Some(v)
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
        | _ -> None;
    in
    match_e e

   (* check if the exp is *p or p[i] *)
   (* should refine this one *)
  method check_exp e loc = 
    (*E.log "in check_exp %a %a\n" d_exp e d_loc loc;*)
    let tmp_lv = self#find_lv_in_exp e
    in
    match tmp_lv with
    | Some(tmp_lv) ->
        let some_local = 
        match fst tmp_lv with
        | Var(v) -> ();
        | Mem(m) ->
            let local = 
            match e with
            | BinOp(_,e1,e2,_) ->
              if self#check_var_star_d e1 e2 
              then  begin 
                E.log "?AA %a %a\n" d_exp e d_loc loc;
                self#push_result loc 
              end
              else
               if self#check_lval_star_d e1 e2
               then begin E.log "?AA %a %a\n" d_exp e d_loc loc; 
                self#push_result loc end 
            | _ ->();
            in local;
         self#check_lv tmp_lv loc;
        in
        some_local;
    | None -> ();
  
  method find_lv_in_exp( e:exp) =
    let rec match_e e = 
      match e with
      | Lval(lv) -> Some(lv);
      | SizeOfE(e) -> match_e e;
      | AlignOfE(e) -> match_e e;
      | UnOp( _, e, _) -> match_e e;
      | BinOp( _, e1, e2, _) -> match_e e1; 
      | CastE( _, e) -> match_e e;
      | AddrOf(lv) -> Some(lv);
      | StartOf(lv) -> Some(lv);
      | _ -> None;
    in
    match_e e

    (* check if lv is *p or p[i] *)
  method check_lv (tmp_lv:lval) loc =
    (*E.log "in check_lv: %a %a\n" d_lval tmp_lv d_loc loc; *)
        if (self#match_exp tmp_lv loc) 
        then begin 
          E.log "?AA %a %a\n" d_lval tmp_lv d_loc loc; 
                self#push_result loc end 
        else ();
  
  method vinst( i :instr) : instr list visitAction = 
    match i with
    | Set(lv, e, loc) ->
        (* check whether e or lv contains d[i] *)
        self#check_exp e loc;
        self#check_lv lv loc;
        DoChildren;
    | Call(lv, e1, e2, loc) ->
        let rec local (e:exp list) = 
        match e with
        | [] -> ();
        | head::tail -> 
            self#check_exp head loc; 
            local tail;
        in
        local e2;
        DoChildren;
    | _ -> DoChildren;

end

class funcSelector( last_record,oc: record*out_channel) = object (self)
  inherit nopCilVisitor  

  val mutable fundecList : (fundec list) = [];
 
  val mutable cur_fundec :fundec = dummyFunDec;

  method push_fundec f = fundecList <- f::fundecList;
 
  method getFundecList = fundecList;
  
  method dedup  = 
    let rec dedup_helper tmp_list = 
    match tmp_list with
    | [] -> []
    | head::tail -> if (List.mem head tail) then dedup_helper tail else
      head::(dedup_helper tail)
    in
    fundecList <- dedup_helper fundecList;
  
  method getBlock (b:block) =
    List.iter self#getInstrList b.bstmts

  (* search through the result set to see if the location in the result set *)
  (* return true if it is, false else. *)
  method search_loc (loc:location) =
    if List.mem loc last_record.resultList 
    then true else false;

  method getInstr (i: instr list) =
   List.iter (
     function(t_i) -> 
     match t_i with
    | Set(lv,e,loc) ->
        (* check if this loc contains in result set. *)
        if self#search_loc loc
        then 
          self#push_fundec cur_fundec
        else ();
    | Call(lv,e1,e2,loc) ->();
    | _ -> (); ) i 

  method getInstrList (st: stmt) = 
   match st.skind with
   | Instr i -> 
       self#getInstr i;
   | If( _, tb, fb, _) -> 
       self#getBlock tb;
       self#getBlock fb;
   | Switch(_,b,_,_) ->
       self#getBlock b;
   | Loop(b,_,_,_) ->
       self#getBlock b;
   | _ -> ();

  method vfunc (f:fundec) = 
    cur_fundec <- f;
    self#getBlock f.sbody;
    DoChildren;

end
(*------------------------------------ begin ------------------------------------*)

(* load all *.i file in openssl-file *)
 let input_file filename = 
  let chan = open_in filename 
  in
  let lines = ref [] 
  in
  try while true; do lines := input_line chan :: !lines done; []
  with
   End_of_file -> close_in chan; List.rev !lines

(* construct CIL data structure *)
let () =  
  let files = 
    input_file "openssl-files" 
  in  
  let  files = List.map( fun filename -> let f = Frontc.parse filename in f() ) files 
    in
    let file = Mergecil.merge files "test" 
    in
Rmtmps.removeUnusedTemps file;

Cfg.computeFileCFG file;

(* do the AST analysis *)
  let last_record = {funList=[]; fun_arg_list=[]; varList=[]; pList=[];
  resultList=[]; lvList = []} 
and last_record2 = {funList=[]; fun_arg_list=[]; varList=[]; pList=[];
resultList=[]; lvList = []}
  and oc = open_out "tmp-openssl-result-p.dat" 
  in
 let vis = new instrVisitor("rsa_st","p","d",last_record,oc)
in
E.logChannel := oc;
ignore(visitCilFile (vis:> cilVisitor) file);
ignore(visitCilFile (vis:> cilVisitor) file);
vis#extractor;
vis#dedup;
vis#get_result;
(* get the propagation of rsa->p *)
while (List.length last_record.varList != List.length last_record2.varList)
&&(List.length last_record.fun_arg_list != List.length last_record2.fun_arg_list) 
do
  last_record2.fun_arg_list <- last_record.fun_arg_list;
  last_record2.varList <- last_record.varList; 
  ignore(visitCilFile (vis:> cilVisitor) file);
  vis#extractor;
  vis#dedup;
  vis#get_result;
done;

vis#get_result;
let vis = new sndVisitor("BN_ULONG","d", last_record,oc)
in
(* get the reference from rsa->p to rsa->p->d *)
E.logChannel := oc;
ignore(visitCilFile (vis:>cilVisitor) file);
vis#dedup;
vis#get_result;
E.log "---------------Begin print varList -------------\n";
List.iter (function(v) -> E.log "phase 2: %a%s\n"  d_type v.vtype v.vname) vis#get_varList;

(* get the propagation of rsa->p->d *)
let vis = new instrVisitor("bignum_st","d","",last_record,oc)
in
while (List.length last_record.varList != List.length last_record2.varList)
&&(List.length last_record.fun_arg_list != List.length last_record2.fun_arg_list) 
do
  last_record2.fun_arg_list <- last_record.fun_arg_list;
  last_record2.varList <- last_record.varList; 
  ignore(visitCilFile (vis:> cilVisitor) file);
  vis#extractor;
  vis#dedup;
  vis#get_result;
done;
E.log "---------------Begin print varList 2-------------\n";
List.iter (function(v) -> E.log " phase 3: %s loc: %a\n" v.vname d_loc v.vdecl) last_record.varList;
vis#get_result;

(* check the last result(pointers) contains *p or p[i] *)
let vis = new checkArrayVisitor(last_record,oc)
in
ignore(visitCilFile(vis:>cilVisitor) file);

vis#get_result;

let vis = new funcSelector( last_record,oc) 
in
ignore(visitCilFile(vis:>cilVisitor) file);
vis#dedup;

E.log "---------------Begin print loc list-------------\n";
List.iter (function(v) -> E.log " fundec: %s \n" v.svar.vname)
vis#getFundecList;

