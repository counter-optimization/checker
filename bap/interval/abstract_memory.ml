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
               valtype: CellType.t;
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
      | CellType.Ptr -> "ptr"
      | CellType.Scalar -> "scalar"
      | CellType.Unknown -> "unknowntype"

    let make ~region ~offs ~width ~valtype : t =
      { region; offs; width; valtype }

    let ptr_of_t { region; offs; width; valtype } : Pointer.t =
      Pointer.make ~region ~offs ~width

    let t_of_ptr ?(valtype = CellType.Scalar) p : t =
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
      let valtype_str = string_of_valtype m.valtype in
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
  end
end

module BaseSet = Region.Set

module TypeEnv = struct
  module M = Map.Make_binable_using_comparator(String)
  include M

  let merge te1 te2 =
    let merge_helper ~key ~data prev =
      match find prev key with
      | None -> set prev ~key ~data
      | Some last ->
         let res = CellType.join last data |> Result.ok_exn in
         set prev ~key ~data:res
    in
    M.fold te1 ~init:te2 ~f:merge_helper
end

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
  type t = { cells: cellmap;
             env: env;
             types: CellType.t TypeEnv.t;
             bases: basemap;
             img: Image.t option }
  type regions = Region.Set.t
  type valtypes = Common.cell_t

  let empty : t = { cells = C.Map.empty;
                    env = Env.empty;
                    types = TypeEnv.empty;
                    bases = BaseSetMap.empty;
                    img = None }

  let make_pointer = Ptr.make

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "Couldn't extract interval information out of product domain"

  let get_typd : N.t -> Type_domain.t =
    match N.get Type_domain.key with
    | Some f -> f
    | None -> failwith "Couldn't extract type information out of product domain"

  (* let set_typd (typ : Type_domain) = *)
  (*   N.set Type_domain.key typ *)

  let set_img (mem : t) (img : Image.t) : t =
    { mem with img = Some img }

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

  let equal {cells=cells1; env=env1; bases=bases1; _}
        {cells=cells2; env=env2; bases=bases2; _} =
    Env.equal env1 env2 &&
      Map.equal (Set.equal) cells1 cells2 &&
        Map.equal (Set.equal) bases1 bases2

  let set_type ~name (typ : CellType.t) m =
    let updated_types = TypeEnv.set m.types ~key:name ~data:typ in
    { m with types = updated_types }

  let setptr ~(name:string) ~regions ~offs ~width m =
    let env = Env.set name offs m.env in
    let bases = BaseSetMap.set m.bases
                  ~key:name
                  ~data:regions
    in
    let types = TypeEnv.set m.types ~key:name ~data:CellType.Ptr in
    { m with env = env; bases = bases; types = types }

  let get_type ~name m : CellType.t =
    match TypeEnv.find m.types name with
    | Some typ -> typ
    | None -> CellType.Unknown
 
  let unptr ~name mem : t =
    let new_env = Env.set name N.bot mem.env in
    let new_bases = BaseSetMap.remove mem.bases name in
    let new_types = TypeEnv.set ~key:name ~data:CellType.Scalar mem.types in
    { mem with env=new_env; bases=new_bases; types=new_types }

  let is_pointer ~name {cells; env; bases; _} : bool =
    BaseSetMap.mem bases name

  let holds_ptr (name : string) (env : t) : bool =
    is_pointer ~name env

  let setunk ~(name:string) m =
    let cleared_of_ptr = if is_pointer ~name m
                         then unptr ~name m
                         else m
    in
    set_type ~name CellType.Unknown cleared_of_ptr
    
  let is_scalar ~name mem : bool =
    not @@ is_pointer ~name mem

  let get_offset ~name mem : N.t option =
    if is_pointer name mem
    then Some (Env.lookup name mem.env)
    else None

  let lookup name m : N.t =
    Env.lookup name m.env

  let set name data (mem : t) : t =
    { mem with env = Env.set name data mem.env }

  let set_cell_to_top cell_name (mem : t) : t =
    { mem with env = Env.set cell_name N.top mem.env }

  let rec compute_type (e : Bil.exp) (mem : t) : CellType.t =
    match e with
    | Load (_mem, idx, _endian, size) -> CellType.Unknown
    | Store (_mem, idx, v, _endian, _sz) -> CellType.Unknown
    | Var v ->
       let name = Var.name v in
       if holds_ptr name mem
       then CellType.Ptr
       else CellType.Scalar
    | Int w -> Scalar (* maybe, later this may be a global *)
    | BinOp (op, x, y) ->
       let xt = compute_type x mem in
       let yt = compute_type y mem in
       begin
         match op with
         | Bil.PLUS -> (match xt, yt with
                        | CellType.Ptr, _ -> CellType.Ptr
                        | _, CellType.Ptr -> CellType.Ptr
                        | _ -> CellType.Scalar)
         | Bil.MINUS -> (match xt, yt with
                         | CellType.Ptr, CellType.Ptr -> CellType.Scalar
                         | CellType.Ptr, CellType.Scalar -> CellType.Ptr
                         | _ -> failwith "Can't subtract these types in compute_type mem")
         | _ -> CellType.Scalar
       end
    | UnOp (op, x) -> compute_type x mem
    | Cast (cast_op, size, e) -> compute_type e mem
    | Extract (hi, lo, e) -> compute_type e mem
    | Concat (left, right) ->
       let left_t = compute_type left mem in
       let right_t = compute_type right mem in
       (match left_t, right_t with
       | CellType.Ptr, _ -> CellType.Ptr
       | _, CellType.Ptr -> CellType.Ptr
       | _ -> CellType.Scalar)
    | Let _ -> CellType.Scalar (* todo *)
    | Ite _ -> CellType.Scalar (* todo *)
    | Unknown _ -> CellType.Scalar
  
  let update_on_assn ~lhs ~rhs mem : t =
    let name = Var.name lhs in
    let rhs_is_store = match rhs with
      | Bil.Store _ -> true
      | _ -> false
    in
    if rhs_is_store
    then mem
    else
      let lhs_t = get_type ~name mem in
      let rhs_t = compute_type rhs mem in
      let move_ptrs mem : t =
        let vars = get_var_names rhs in
        let bases_to_load_from = BaseSetMap.bases_of_vars vars mem.bases in
        { mem with bases = BaseSetMap.set mem.bases ~key:name ~data:bases_to_load_from }
      in
      match lhs_t, rhs_t with
      | CellType.Ptr, CellType.Scalar ->
         unptr ~name mem
      | CellType.Scalar, CellType.Ptr
      | CellType.Unknown, CellType.Ptr ->
         let st' = set_type ~name CellType.Ptr mem in
         move_ptrs st'
      | CellType.Ptr, CellType.Ptr ->
         move_ptrs mem
      | CellType.Scalar, CellType.Scalar
        | CellType.Unknown, CellType.Unknown ->
         mem
      | CellType.Ptr, CellType.Unknown ->
         let st' = unptr ~name mem in
         setunk ~name st'
      | CellType.Scalar, CellType.Unknown ->
         setunk ~name mem
      | CellType.Unknown, CellType.Scalar ->
         set_type ~name CellType.Scalar mem

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

  let load_global (offs : Wrapping_interval.t) (sz : size) (m : t) : N.t =
    match m.img with
    | None -> failwith "memory's image should be set before load"
    | Some img ->
       let segs = Image.segments img in
       match Wrapping_interval.to_int offs with
       | None ->
          begin
            let offs_s = Wrapping_interval.to_string offs in
            failwith @@ sprintf "couldn't convert offs %s to address for image" offs_s
          end
       | Some addr ->
          let addr_w = Word.of_int ~width:64 addr in
          let target_seg = Table.find_addr segs addr_w in
          match target_seg with
          | None ->
             begin
               let addr_s = Word.to_string addr_w in
               failwith @@ sprintf "couldn't find addr %s in image" addr_s
             end
          | Some (mem, seg) ->
             match Memory.get ~addr:addr_w ~scale:sz mem with
             | Error e ->
                begin
                  let segname = Image.Segment.name seg in
                  let addr_s = Word.to_string addr_w in
                  let err_s = Error.to_string_hum e in
                  failwith @@ sprintf "Error reading address %s from seg %s: %s" addr_s segname err_s
                end
             | Ok data ->
                let res = N.of_word data in
                let res_s = N.to_string res in
                let () = printf "in load_global, loaded data was %s\n" res_s in
                res
  
  let load_of_bil_exp (e : Bil.exp) (idx_res : N.t) (m : t) : N.t =
    match e with
    | Bil.Load (_mem, idx, _endian, size) ->
       let load_from_vars = get_var_names idx in
       let regions = BaseSetMap.bases_of_vars load_from_vars m.bases in
       let offs_type = compute_type idx m in
       let width = bap_size_to_absdom size in
       let offs = get_intvl idx_res in
       let is_scalar = match offs_type with | Scalar -> true | _ -> false in
       if is_scalar && Set.is_empty regions
       then
         load_global offs size m
       else
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
    |> set_type ~name:"RSP" CellType.Ptr

  let set_rbp (offs : int) (mem : t) : t =
    let offs = N.of_int ~width:64 offs in
    setptr mem
      ~name:"RBP"
      ~regions:(BaseSet.singleton Region.Stack)
      ~offs
      ~width:`r64
  (* |> set_type ~name:"RBP" CellType.Ptr *)

  let merge_images img1 img2 : Image.t option =
    if Option.is_some img1
    then img1
    else
      if Option.is_some img2
      then img2
      else None

  (* TODO: type consistency in cell merging *)
  let merge mem1 mem2 : t =
    let {cells=cells1; env=env1; types=types1; bases=bases1; img=img1} = mem1 in
    let {cells=cells2; env=env2; types=types2; bases=bases2; img=img2} = mem2 in
    let merged_img = merge_images img1 img2 in
    let merged_cells = C.Map.merge cells1 cells2 in
    let merged_env = Env.merge env1 env2 in
    let merged_types = TypeEnv.merge types1 types2 in
    let merged_bases = BaseSetMap.merge bases1 bases2 in
    { cells=merged_cells;
      env=merged_env;
      types=merged_types;
      bases=merged_bases;
      img=merged_img }

  let widen_threshold = 256

  let widen_with_step steps n prev next : t =
    let widen_cell_map {cells=prev; _} {cells=next; _} : C.Map.t =
      C.Map.merge prev next
    in
    let widen_bases_map {bases=prev; _} {bases=next; _} : basemap =
      BaseSetMap.merge prev next
    in
    let widen_env steps n {env=prev; _} {env=next; _} : Env.t =
      Env.widen_with_step steps n prev next
    in
    let widen_type_env steps n {types=prev; _} {types=next; _} =
      TypeEnv.merge prev next
    in
    let widen steps n mem1 mem2 =
      {cells = widen_cell_map prev next;
       env = widen_env steps n prev next;
       types = widen_type_env steps n prev next;
       bases = widen_bases_map prev next;
       img = merge_images prev.img next.img }
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
