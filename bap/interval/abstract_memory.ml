open Core_kernel
open Bap.Std
open Common

(* This is an implementation based on the paper 'Field-Sensitive Value *)
(* Analysis of Embedded C Programs with Union Types and Pointer *)
(* Arithmetics'. *)
(* currently assumes: *)
(* - little endian architecture *)

(** Regions and basis maps *)
module Region = struct
  module T = struct
    type t = Global | Heap | Stack [@@deriving sexp, bin_io, compare]

    let equal r1 r2 : bool =
      match r1, r2 with
      | Global, Global
        | Heap, Heap
        | Stack, Stack -> true
      | _ -> false

    let to_string = function
      | Global -> "global"
      | Heap -> "heap"
      | Stack -> "stack"

    let pp x =
      Format.printf "%s" @@ to_string x
  end

  module Cmp = struct
    include T
    include Comparator.Make(T)
  end

  include Cmp
  
  module Set = struct
    type t = (Cmp.t, Cmp.comparator_witness) Set.t
    let empty : t = Set.empty (module Cmp)
    let singleton (r : Cmp.t) = Set.add empty r
    let from_region (r : T.t) = singleton r 
    (* let fold = Set.fold *)
    (* let map = Set.map (module Cmp) *)
  end
end

module BaseSetMap = struct
  include Map.Make_binable_using_comparator(String)
  module SS = Set.Make_binable_using_comparator(String)

  let bases_of_vars (vars : SS.t) bsm : Region.Set.t =
    SS.fold vars ~init:Region.Set.empty
      ~f:(fun acc v -> match find bsm v with
                       | Some bases -> Set.union acc bases
                       | None -> acc)
  
  let merge x y =
    fold y ~init:x ~f:(fun ~key ~data merged ->
        let finaldata = match find merged key with
          | Some baseset -> Base.Set.union baseset data
          | None -> data
        in
        set merged ~key ~data:finaldata)
end

(** Pointers *)
module Pointer(N : NumericDomain) = struct
  module T = struct
    type t = { region: Region.t;
               offs: Wrapping_interval.t;
               width: Wrapping_interval.t }
               [@@deriving sexp_of]

    let equal x y =
      let reg_eq = Region.equal x.region y.region in
      let offs_eq = Wrapping_interval.equal x.offs y.offs in
      let width_eq = Wrapping_interval.equal x.width y.width in
      reg_eq && offs_eq && width_eq

    let compare x y = if equal x y then 0 else -1

    let get_intvl : N.t -> Wrapping_interval.t =
      match N.get Wrapping_interval.key with
      | Some f -> f
      | None -> failwith "Couldn't extract interval information out of product domain, in module Pointer"

    let make ~region ~offs ~width : t = { region; offs; width }

    let region p = p.region
    let offs p = p.offs
    
    let width_in_bits p = p.width
    let bits_in_byte = Wrapping_interval.of_int 8
    let width_in_bytes p =
      let bitwidth = width_in_bits p in
      Wrapping_interval.div bitwidth bits_in_byte

    let all_widths_of_ptr {region; offs; width} =
      let possible_offs = [8; 16; 32; 64; 128; 256; 512] in
      List.map possible_offs ~f:(fun width ->
          let width = N.of_int ~width:64 width |> get_intvl in
          {region; offs; width})

    let of_regions ~regions ~offs ~width =
      List.map (Set.to_list regions) ~f:(fun region ->
          make ~region ~offs ~width)

    let to_string {region; offs; width} =
      let r = Region.to_string region in
      let o = Wrapping_interval.to_string offs in
      let w = Wrapping_interval.to_string width in
      sprintf "(%s, %s, %s)" r o w

    let pp ptr : unit =
      Format.printf "%s" @@ to_string ptr
  end

  include T
  include Comparator.Make(T)
end

(** Cells *)
module Cell(N : NumericDomain) = struct
  module T = struct
    module Pointer = Pointer(N)
    (* the new environment operates over ValDom.t and pointers (PtrT of ptr) *)
    
    
    (* it's important that a cell knows whether it holds a pointer or a scalar. *)
    (* for our model of memory, pointers are (region * offset) pairs, so taking *)
    (* e.g., the low 32 bits of a pointer doesn't type check *)
    type t = { region: Region.t;
               offs: Wrapping_interval.t;
               valtype: Common.cell_t;
               width: Wrapping_interval.t }
               [@@deriving sexp_of]

    let compare x y =
      let reg_eq = Region.equal x.region y.region in
      let offs_eq = Wrapping_interval.equal x.offs y.offs in
      let width_eq = Wrapping_interval.equal x.width y.width in
      let valtype_eq = match x.valtype, y.valtype with
        | Scalar, Scalar -> true
        | Ptr, Ptr -> true
        | _, _ -> false
      in
      if reg_eq && offs_eq && width_eq && valtype_eq
      then 0
      else -1

    let bits_per_byte = Wrapping_interval.of_int 8
    let width_in_bits c = c.width
    let width_in_bytes c =
      let width = width_in_bits c in
      Wrapping_interval.div width bits_per_byte

    let get_intvl : N.t -> Wrapping_interval.t =
      match N.get Wrapping_interval.key with
      | Some f -> f
      | None -> failwith "Couldn't extract interval information out of product domain, in module Cell"

    let string_of_valtype = function
      | Ptr -> "ptr"
      | Scalar -> "scalar"

    let make ~region ~offs ~width ~valtype : t =
      { region; offs; width; valtype }

    let ptr_of_t { region; offs; width; valtype } : Pointer.t =
      Pointer.make ~region ~offs ~width

    let t_of_ptr ?(valtype = Scalar) p : t =
      let region = Pointer.region p in
      let width = Pointer.width_in_bits p in
      let offs = Pointer.offs p in
      make ~region ~offs ~width ~valtype

    let equals_ptr cel ptr : bool =
      let reg = Pointer.region ptr in
      let offs = Pointer.offs ptr in
      let width = Pointer.width_in_bits ptr in
      Region.equal cel.region reg &&
        Wrapping_interval.could_be_true (Wrapping_interval.booleq cel.offs offs) &&
          Wrapping_interval.could_be_true (Wrapping_interval.booleq cel.width width)

    let name (m : t) : string =
      let reg_str = Region.to_string m.region in
      let off_str = Wrapping_interval.to_string m.offs in
      let width_str = Wrapping_interval.to_string m.width in
      sprintf "%s-%s-%s" reg_str off_str width_str

    let to_string m : string =
      let valtype_str = match m.valtype with
        | Ptr -> "ptr"
        | Scalar -> "scalar"
      in
      sprintf "%s-%s" (name m) valtype_str

    let pp (m : t) : unit =
      Format.printf "%s" @@ to_string m

    let overlaps_with_ptr cel ptr : bool =
      if not (Region.equal (Pointer.region ptr) cel.region)
      then false
      else
        let open Wrapping_interval in
        let one = of_int 1 in
        let ptr_base = Pointer.offs ptr in
        let ptr_end = add (Pointer.offs ptr) (Pointer.width_in_bytes ptr) in
        let ptr_end = sub ptr_end one in
        let cel_base = cel.offs in
        let cel_end = add cel.offs (width_in_bytes cel) in
        let cel_end = sub cel_end one in
        could_be_true (boolle ptr_base cel_base) &&
          could_be_true (boolle cel_base ptr_end) ||
          could_be_true (boolle ptr_base cel_end) &&
            could_be_true (boolle cel_end ptr_end)

    (** values are actually in big endian because we're in
        ocaml land, but we want to simulate little endian
        especially if there are overlapping reads/writes *)
    
  end

  module Cmp = struct
    include T
    include Comparator.Make(T)
  end

  include Cmp
  
  module Set = struct
    type t = (Cmp.t, comparator_witness) Set.t

    let empty : t = Set.empty (module Cmp)
    
    (* TODO: what if two cells have different type? *)
    let merge s1 s2 : t = Set.union s1 s2

    let of_list = Set.of_list (module Cmp)
    let add = Set.add

    let length = Set.length
    let union = Set.union

    let find = Set.find
    let filter = Set.filter
    let fold = Set.fold
    
    let to_list = Set.to_list
    let singleton = Set.singleton (module Cmp)
  end

  module Map = struct
    type t = (Pointer.t, Set.t, Pointer.comparator_witness) Map.t
    
    let empty = Map.empty (module Pointer)
    let set (m : t) = Map.set m
    let merge (m1 : t) (m2 : t) : t =
      let combine ~key v1 v2 = Set.merge v1 v2 in
      Map.merge_skewed m1 m2 ~combine
    let find : t -> Pointer.t -> Set.t option = Map.find
    let find_exn : t -> Pointer.t -> Set.t = Map.find_exn

    let fold : t -> init:'a -> f:(key:Pointer.t -> data:Set.t -> 'a -> 'a) -> 'a = Map.fold

    let add_cell ptr cell m =
      let new_cell_set = if Map.mem m ptr
                         then Set.add (find_exn m ptr) cell
                         else Set.singleton cell
      in
      Map.set m ~key:ptr ~data:new_cell_set

    let merge m1 m2 : t =
      let merge_helper ~key ~data prev =
        match find prev key with
        | Some cells -> set prev ~key ~data:(Set.union data cells)
        | None -> set prev ~key ~data
      in
      fold m2 ~init:m1 ~f:merge_helper

    let get_overlapping ptr (m : t) =
      Map.fold m ~init:Set.empty
        ~f:(fun ~key ~data acc ->
          let cellset = data in
          let overlap = Set.filter cellset
                          ~f:(fun c -> overlaps_with_ptr c ptr)
          in
          if Set.length overlap <> 0
          then Set.union overlap acc
          else acc)

    (** For a newly added_cell, and all of the cells it overlaps,
        overlap_cells, for each c in overlap_cells, add added_cell
        to c's set of overlapping ptrs (pointer overlap is a symmetric
        relation) *)
    let fix_overlap (added_cell : T.t) (overlap_cells : Set.t) (m : t) : t =
      Set.fold overlap_cells ~init:m
        ~f:(fun m cel ->
          let ptr = ptr_of_t cel in
          let old_cellset = find_exn m ptr in
          let new_cellset = Set.add old_cellset added_cell in
          set ~key:ptr ~data:new_cellset m)

    (* let get_overlap_correction_function (toadd : T.t) *)
    (*       (overlapping : T.t) (m : t) :  *)
         
    (* let add_cell ptr valtype m : t = *)
    (*   let ptr_base = Pointer.region ptr in *)
    (*   let ptr_offs = Pointer.offs ptr in *)
    (*   (\* TODO: this should check for other similar pointers of different *\) *)
    (*   (\* widths *\) *)
    (*   let all_ptrs = Pointer.all_widths_of_ptr ptr in *)
    (*   if Map.mem m ptr *)
    (*   then *)
    (*     begin *)
    (*       let base_str = Region.to_string ptr_base in *)
    (*       let offs_str = N.to_string ptr_offs in *)
    (*       failwith @@ *)
    (*         sprintf "Cell for ptr (%s, %s) already exists" base_str offs_str *)
    (*     end *)
    (*   else *)
    (*     let ptr_width = Pointer.width ptr in *)
    (*     let new_cell = T.make *)
    (*                      ~region:ptr_base *)
    (*                      ~offs:ptr_offs *)
    (*                      ~width:ptr_width *)
    (*                      ~valtype *)
    (*     in *)
    (*     let overlapping_cells = get_overlapping ptr m in *)
    (*     let withnew = Set.add overlapping_cells new_cell in *)
    (*     let new_m = Map.set m ~key:ptr ~data:withnew in *)
    (*     fix_overlap new_cell overlapping_cells new_m *)
  end
end

module BaseSet = Region.Set

module Make(N : NumericDomain)
       : (MemoryT with type v := N.t
                   and type regions := BaseSet.t
                   and type region := Region.t
                   and type valtypes := Common.cell_t) = struct
  module Env = NumericEnv(N)
  module C = Cell(N)
  module Ptr = Pointer(N)
  module CellSet = C.Set
  
  module SS = Set.Make_binable_using_comparator(String)

  type basemap = BaseSet.t BaseSetMap.t
  type cellset = CellSet.t
  type cellmap = C.Map.t
  type env = Env.t
  type t = { cells: cellmap; env: env; bases: basemap }
  type regions = Region.Set.t
  type valtypes = Common.cell_t

  let empty : t = { cells = C.Map.empty;
                    env = Env.empty;
                    bases = BaseSetMap.empty }

  let make_pointer = Ptr.make

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "Couldn't extract interval information out of product domain"

  let bap_size_to_absdom (sz : Size.t) : Wrapping_interval.t =
    let bitwidth = match sz with
      | `r8 -> 8
      | `r16 -> 16
      | `r32 -> 32
      | `r64 -> 64
      | `r128 -> 128
      | `r256 -> 256
    in
    Wrapping_interval.of_int bitwidth

  let rec get_var_names (e : Bil.exp) : SS.t =
    match e with
    | Bil.Var v -> SS.singleton @@ Var.name v
    | Bil.Load (_, idx, _, _) -> get_var_names idx
    | Bil.Store (_, idx, v, _, _) ->
       SS.union (get_var_names idx) (get_var_names v)
    | Bil.BinOp (_, x, y) ->
       SS.union (get_var_names x) (get_var_names y)
    | Bil.UnOp (_, x) -> get_var_names x
    | Bil.Int _ -> SS.empty
    | Bil.Cast (cast, n, e) -> get_var_names e
    | Bil.Ite (cond, then', else') ->
       SS.union (get_var_names then') (get_var_names else')
    | Bil.Unknown (str, _) -> SS.empty
    | Bil.Let (v, exp, body) ->
       SS.union (SS.singleton (Var.name v)) (get_var_names exp)
       |> SS.union (get_var_names body)
    | Bil.Extract (hi, lo, e) -> get_var_names e
    | Bil.Concat (x, y) ->
       SS.union (get_var_names x) (get_var_names y)

  let equal {cells=cells1; env=env1; bases=bases1}
        {cells=cells2; env=env2; bases=bases2} =
    Env.equal env1 env2 &&
      Map.equal (Set.equal) cells1 cells2 &&
      Map.equal (Set.equal) bases1 bases2

  let setptr ~(name:string) ~regions ~offs ~width {cells; env; bases} =
    let env = Env.set name offs env in
    let bases = BaseSetMap.set bases
                  ~key:name
                  ~data:regions
    in
    { cells; env; bases }

  let unptr ~name {cells; env; bases} : t =
    let new_env = Env.set name N.bot env in
    let new_bases = BaseSetMap.remove bases name in
    {cells; env=new_env; bases=new_bases}

    let is_pointer ~name {cells; env; bases} : bool =
    BaseSetMap.mem bases name

  let holds_ptr (name : string) (env : t) : bool =
    is_pointer ~name env

  let is_scalar ~name mem : bool =
    not @@ is_pointer ~name mem

  let get_offset ~name {cells; env; bases} : N.t option =
    if is_pointer name {cells; env; bases}
    then Some (Env.lookup name env)
    else None

  let lookup name {cells; env; bases} : N.t =
    Env.lookup name env

  let set name data (mem : t) : t =
    { mem with env = Env.set name data mem.env }

  let set_cell_to_top cell_name (mem : t) : t =
    { mem with env = Env.set cell_name N.top mem.env }

  (* Returns tuple of (exact cell match option, overlapping cells) *)
  let get_cells_for_ptr ptr mem : C.t option * C.Set.t =
    failwith "todo"

  let rec compute_type (e : Bil.exp) (mem : t) : Common.cell_t =
    match e with
    | Load (_mem, idx, _endian, size) -> Scalar (* todo *)
    | Store (_mem, idx, v, _endian, _sz) -> Scalar (* todo *)
    | Var v ->
       let name = Var.name v in
       if holds_ptr name mem
       then Ptr
       else Scalar
    | Int w -> Scalar (* maybe, later this may be a global *)
    | BinOp (op, x, y) ->
       let xt = compute_type x mem in
       let yt = compute_type y mem in
       begin
         match op with
         | Bil.PLUS -> (match xt, yt with
                        | Ptr, _ -> Ptr
                        | _, Ptr -> Ptr
                        | _ -> Scalar)
         | Bil.MINUS -> (match xt, yt with
                         | Ptr, Ptr -> Scalar
                         | Ptr, Scalar -> Ptr
                         | _ -> failwith "Can't subtract these types in compute_type mem")
         | _ -> Scalar
            (* begin *)
            (*   let ops = binop_to_string op in *)
            (*   let es = Sexp.to_string @@ Bil.sexp_of_exp e in *)
            (*   let warn_str = sprintf "Can't type exp %s (%s) in compute_type mem" es ops in *)
            (*   failwith warn_str *)
            (* end *)
       end
    | UnOp (op, x) -> compute_type x mem
    | Cast (cast_op, size, e) -> compute_type e mem
    | Extract (hi, lo, e) -> compute_type e mem
    | Concat (left, right) ->
       let left_t = compute_type left mem in
       let right_t = compute_type right mem in
       (match left_t, right_t with
       | Ptr, _ -> Ptr
       | _, Ptr -> Ptr
       | _ -> Scalar)
    | Let _ -> Scalar (* todo *)
    | Ite _ -> Scalar (* todo *)
    | _ ->
       let es = Sexp.to_string @@ Bil.sexp_of_exp e in
       let warn_str = sprintf "Can't type exp %s in compute_type mem" es in
       failwith warn_str

  let update_on_assn ~lhs ~rhs mem : t =
    let name = Var.name lhs in
    let rhs_is_store = match rhs with
      | Bil.Store _ -> true
      | _ -> false
    in
    if rhs_is_store
    then mem
    else
      let lhs_t = if holds_ptr name mem then Ptr else Scalar in
      let rhs_t = compute_type rhs mem in
      match lhs_t, rhs_t with
      | Ptr, Scalar ->
         unptr ~name mem
      | Scalar, Ptr
        | Ptr, Ptr ->
         let vars = get_var_names rhs in
         let bases_to_load_from = BaseSetMap.bases_of_vars vars mem.bases in
         { mem with bases = BaseSetMap.set mem.bases ~key:name ~data:bases_to_load_from }
      | Scalar, Scalar -> mem

  let cells_of_offs_and_regions ~(offs : Wrapping_interval.t) ~regions ~(width : Wrapping_interval.t) (mem : t) : C.Set.t =
    let ptrs = Ptr.of_regions ~regions ~offs ~width in
    let ptr_strings = List.map ptrs ~f:Ptr.to_string in
    let () = List.iter ptr_strings ~f:(fun s -> printf "Pointer to load from is: %s\n" s) in
    List.fold ptrs ~init:C.Set.empty
      ~f:(fun acc ptr ->
        C.Set.union acc @@
          Map.fold mem.cells ~init:C.Set.empty
            ~f:(fun ~key ~data acc ->
              if Ptr.equal ptr key
              then C.Set.union acc data
              else acc))
    (* let () = Map.iter_keys mem.cells ~f:(fun p -> *)
    (*              let first_ptr = List.hd_exn ptrs in *)
    (*              let ptr_s = Ptr.to_string p in *)
    (*              let is_eq = 0 = Ptr.compare first_ptr p in *)
    (*              printf "Ptr in cell map: %s is equal to ptr? %B\n" ptr_s is_eq) *)
    (* in *)
    (* List.fold ptrs ~init:(C.Set.empty) *)
    (*   ~f:(fun foundcells ptr -> *)
    (*     match C.Map.find mem.cells ptr with *)
    (*     | Some cells -> *)
    (*        (let ptr_s = Ptr.to_string ptr in *)
    (*         let () = printf "found cells for ptr %s\n" ptr_s in *)
    (*         C.Set.union cells foundcells) *)
    (*     | None -> ( *)
    (*       let ptr_s = Ptr.to_string ptr in *)
    (*       let () = printf "didn't find any cells for ptr %s\n" ptr_s in *)
    (*       foundcells)) *)

  let load_from_offs_and_regions ~(offs : Wrapping_interval.t) ~regions ~(width : Wrapping_interval.t) (mem : t) : N.t =
    let ptrs = Ptr.of_regions ~regions ~offs ~width in
    let cells = cells_of_offs_and_regions ~offs ~regions ~width mem in
    match C.Set.length cells with
    | 0 ->
       let ptr_strings = List.fold ptrs ~init:"" ~f:(fun acc x ->
                             acc ^ " " ^ Ptr.to_string x)
       in
       failwith @@ sprintf "Didn't find cells for ptrs: %s" ptr_strings
    | 1 ->
       C.Set.fold cells ~init:N.bot ~f:(fun valset c ->
           let celname = C.name c in
           N.join valset @@ lookup celname mem)
    | _ ->
       let ptr_strings = List.fold ptrs ~init:"" ~f:(fun acc x ->
                             acc ^ " " ^ Ptr.to_string x)
       in
       failwith @@ sprintf "No support for reading overlapping ptrs: %s" ptr_strings

  (** Here, name is the name of the var in the mem env that
      holds the offset of the pointer *)
  (* let load ~name ~width (mem : t) : N.t = *)
  (*   match get_offset ~name mem with *)
  (*   | Some offs -> *)
  (*      let regions = match BaseSetMap.find mem.bases name with *)
  (*        | Some regions -> regions *)
  (*        | None -> let err_msg = sprintf "Attempt to read from non-pointer: %s" name in *)
  (*                  failwith err_msg *)
  (*      in *)
  (*      load_from_offs_and_regions ~offs ~regions ~width mem *)
  (*   | None -> *)
  (*      let err_msg = sprintf "Attempt to read from non-pointer: %s" name in *)
  (*      failwith err_msg *)

  let load_of_bil_exp (e : Bil.exp) (idx_res : N.t) (m : t) : N.t =
    match e with
    | Bil.Load (_mem, idx, _endian, size) ->
       let load_from_vars = get_var_names idx in
       let regions = BaseSetMap.bases_of_vars load_from_vars m.bases in
       let width = bap_size_to_absdom size in
       let offs = get_intvl idx_res in
       load_from_offs_and_regions m ~offs ~regions ~width
    | _ -> failwith "Not a load in load_of_bil_exp"
  
  let store ~(offs : Wrapping_interval.t) ~region ~(width : Wrapping_interval.t) ~data ~valtype mem : t =
    let ptr = Ptr.make ~region ~offs ~width in
    let () = printf "storing to ptr %s\n" (Ptr.to_string ptr) in
    let overlap = C.Map.get_overlapping ptr mem.cells in
    let () = Set.iter overlap ~f:(fun c ->
                 printf "overlapping cell: %s\n" (C.to_string c)) in
    if C.Set.length overlap > 1
    then
      let overlap = C.Set.fold overlap ~init:""
                      ~f:(fun acc c -> acc ^ " " ^ (C.name c))
      in
      failwith @@ sprintf "No support for storing with overlapping ptrs %s" overlap
    else
      let cel = if C.Set.length overlap = 1
                then List.hd_exn @@ C.Set.to_list overlap
                else C.t_of_ptr ~valtype ptr
      in
      let celname = C.name cel in
      if not (C.equals_ptr cel ptr)
      then
        failwith @@
          sprintf "Can't store ptr %s to cell %s" (Ptr.to_string ptr) celname
      else
        { mem with env = Env.set celname data mem.env;
                   cells = C.Map.add_cell ptr cel mem.cells }

  let store_of_bil_exp (e : Bil.exp) ~(offs : N.t) ~data ~valtype m =
    let () = printf "store_of_bil_exp called\n" in
    match e with
    | Bil.Store (_mem, idx, v, _endian, size) -> 
      begin
        let vars = get_var_names idx in
        let () = Set.iter vars ~f:(fun v ->
                     printf "var: %s in store offs exp\n" v) in
        let width = bap_size_to_absdom size in
        let bases_to_load_from = BaseSetMap.bases_of_vars vars m.bases in
        let () = Set.iter bases_to_load_from ~f:(fun b ->
                     let reg_s = Region.to_string b in
                     printf "Storing to base %s\n" reg_s) in
        let offs = get_intvl offs in
        Set.fold bases_to_load_from ~init:m ~f:(fun env base ->
            let () = printf "Loading from base %s\n" (Region.to_string base) in
            store ~offs ~region:base ~width ~data ~valtype env)
      end
    | _ -> failwith "store got non-store expression in store_of_bil_exp"

  (* for init the mem on the outermost API call.
     store N.top *)
  let store_init_ret_ptr (mem : t) : t =
    let rsp = "RSP" in
    let rsp_offs = Env.lookup rsp mem.env in
    let rsp_offs_wi = get_intvl rsp_offs in
    let stack_region = match BaseSetMap.find mem.bases rsp with
      | Some bases -> (if Set.length bases <> 1
                      then failwith "RSP should only have stack as its base before calling store_init_ret_ptr"
                      else Set.to_list bases |> List.hd_exn)
      | None -> failwith "RSP should be setup before calling store_init_ret_ptr"
    in
    let rsp_width = Wrapping_interval.of_int 64 in
    store ~offs:rsp_offs_wi ~region:stack_region ~width:rsp_width ~data:N.top ~valtype:Scalar mem

  let set_rsp (offs : int) (mem : t) : t =
    let offs = N.of_int ~width:64 offs in
    setptr mem
      ~name:"RSP"
      ~regions:(BaseSet.singleton Region.Stack)
      ~offs
      ~width:`r64
  |> store_init_ret_ptr

  let set_rbp (offs : int) (mem : t) : t =
    let offs = N.of_int ~width:64 offs in
    setptr mem
      ~name:"RBP"
      ~regions:(BaseSet.singleton Region.Stack)
      ~offs
      ~width:`r64

  (* TODO: type consistency in cell merging *)
  let merge mem1 mem2 : t =
    let {cells=cells1; env=env1; bases=bases1} = mem1 in
    let {cells=cells2; env=env2; bases=bases2} = mem2 in
    let merged_cells = C.Map.merge cells1 cells2 in
    let merged_env = Env.merge env1 env2 in
    let merged_bases = BaseSetMap.merge bases1 bases2 in
    {cells=merged_cells; env=merged_env; bases=merged_bases}

  let widen_threshold = 256

  let widen_with_step steps n prev next : t =
    let widen_cell_map {cells=prev; _} {cells=next; _} : C.Map.t =
      C.Map.merge prev next
    in
    let widen_bases_map {bases=prev; _} {bases=next; _} : basemap =
      BaseSetMap.merge prev next
    in
    let widen_env steps n {env=prev;_} {env=next;_} : Env.t =
      Env.widen_with_step steps n prev next
    in
    let widen steps n mem1 mem2 =
      {cells = widen_cell_map prev next;
       env = widen_env steps n prev next;
       bases = widen_bases_map prev next}
    in
    widen steps n prev next

  let pp {cells; env; bases}  =
    let print_ptr_to_cells ~key ~data =
      let cell_set_str =  Set.to_list data |> List.to_string ~f:C.to_string in
      Format.printf "Ptr %s --> %s\n%!" (Ptr.to_string key) cell_set_str
    in
    let print_bases_map ~key ~data =
      let region_set_str = Set.to_list data |> List.to_string ~f:Region.to_string in
      Format.printf "Var %s --> %s\n%!" key region_set_str
    in
    Map.iteri cells ~f:print_ptr_to_cells;
    Env.pp env;
    Map.iteri bases ~f:print_bases_map
end
