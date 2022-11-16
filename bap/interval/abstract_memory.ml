open Core
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
    type t = { region: Region.t; offs: N.t; width: N.t }
             [@@deriving sexp_of, compare]

    let make ~region ~offs ~width : t = { region; offs; width }

    let region p = p.region
    let offs p = p.offs
    let width p = p.width

    let all_widths_of_ptr {region; offs; width} =
      let possible_offs = [8; 16; 32; 64; 128; 256; 512] in
      List.map possible_offs ~f:(fun width ->
          let width = N.of_int ~width:64 width in
          {region; offs; width})

    let of_regions ~regions ~offs ~width =
      List.map (Set.to_list regions) ~f:(fun region ->
          make ~region ~offs ~width)

    let to_string {region; offs; width} =
      let r = Region.to_string region in
      let o = N.to_string offs in
      let w = N.to_string width in
      sprintf "(%s, %s, %s)" r o w
  end

  include T
  include Comparator.Make(T)
end

(** Cells *)
module Cell(N : NumericDomain) = struct
  module T = struct
    module Pointer = Pointer(N)
    (* the new environment operates over ValDom.t and pointers (PtrT of ptr) *)
    type content = Ptr | Scalar [@@deriving sexp, compare, bin_io]
    
    (* it's important that a cell knows whether it holds a pointer or a scalar. *)
    (* for our model of memory, pointers are (region * offset) pairs, so taking *)
    (* e.g., the low 32 bits of a pointer doesn't type check *)
    type t = { region: Region.t;
               offs: N.t;
               valtype: content;
               width: N.t }
               [@@deriving sexp_of, compare]

    let string_of_valtype = function
      | Ptr -> "ptr"
      | Scalar -> "scalar"

    let make ~region ~offs ~width ~valtype : t =
      { region; offs; width; valtype }

    let ptr_of_t { region; offs; width; valtype } : Pointer.t =
      Pointer.make ~region ~offs ~width

    let t_of_ptr ?(valtype = Scalar) p : t =
      let region = Pointer.region p in
      let width = Pointer.width p in
      let offs = Pointer.offs p in
      make ~region ~offs ~width ~valtype

    let equals_ptr cel ptr : bool =
      let reg = Pointer.region ptr in
      let offs = Pointer.offs ptr in
      let width = Pointer.width ptr in
      Region.equal cel.region reg &&
        N.could_be_true (N.booleq cel.offs offs) &&
          N.could_be_true (N.booleq cel.width width)

    let name (m : t) : string =
      let reg_str = Region.to_string m.region in
      let off_str = N.to_string m.offs in
      let width_str = N.to_string m.width in
      sprintf "%s-%s-%s" reg_str off_str width_str

    let overlaps_with_ptr cel ptr : bool =
      if not (Region.equal (Pointer.region ptr) cel.region)
      then false
      else
        let ptr_base = Pointer.offs ptr in
        let ptr_end = N.add (Pointer.offs ptr) (Pointer.width ptr) in
        let cel_base = cel.offs in
        let cel_end = N.add cel.offs cel.width in
        N.could_be_true (N.boolle ptr_base cel_base) &&
          N.could_be_true (N.boolle cel_base ptr_end) ||
          N.could_be_true (N.boolle ptr_base cel_end) &&
            N.could_be_true (N.boolle cel_end ptr_end)

    (** values are actually in big endian because we're in
        ocaml land, but we want to simulate little endian
        especially if there are overlapping reads/writes *)
    
  end

  module Cmp = struct
    include T
    include Comparator.Make(T)
  end

  include Cmp

  (* module Overlap = struct *)
  (*   type t = (N.t * N.t) list [@@deriving compare, sexp_of] *)

  (*   let get_values_feor_indices (cell : T.t) (indices : N.t list) *)
  (*       : N.t list = *)

  (*   let rec loop_over_indices (start_idx : N.t) *)
  (*             (end_idx : N.t) *)
  (*             (cmp : N.t -> N.t -> bool) *)
  (*             (next_idx : N.t -> N.t) *)
  (*             ?(acc = [] : N.t list) : N.t list = *)
  (*     if cmp start_idx end_idx *)
  (*     then f (next_idx start_idx) end_idx cmp next_idx (start_idx :: acc) *)
  (*     else acc *)

  (*   (\** PRE: fromcell and tocell definitely overlap *\) *)
  (*   let get_overlap_indices fromcell tocell : t = *)
  (*     let {region=fromregion; *)
  (*          width=fromwidth; *)
  (*          offs=fromoffs; *)
  (*          valtype=fromvaltype} = fromcell in *)
  (*     let {region=toregion; *)
  (*          width=towidth; *)
  (*          offs=tooffs; *)
  (*          valtype=tovaltype} = tocell in *)
  (*     let to_base = tooffs in *)
  (*     let to_end = N.add to_base towidth in *)
  (*     let from_base = fromoffs in *)
  (*     let from_end = N.add from_base fromwidth in *)
  (*     if N.could_be_true (N.boolle from_base to_end) && *)
  (*          N.could_be_true (N.boolle to_base from_base) *)
  (*     then *)
  (*       let overlap_indices = loop_over_indices from_base *)
  (*                               to_end *)
  (*                               (fun f t -> N.could_be_true (N.boolle f t)) *)
  (*                               (fun idx -> N.add (N.of_int 1) idx) *)
  (*       in *)
  (*       let  *)
  (*       (\* the left part of fromcell overlaps the rightside of tocell *)
  (*          OR they completely overlap *\) *)
  (*     else *)
  (*       (\* the right part of fromcell overlaps the leftside of tocell *\) *)
           

  (*   let calculate fromcell tocell : t = *)
      
  (* end *)
  
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
    let merge m1 m2 =
      let combine ~key v1 v2 = Set.merge v1 v2 in
      Map.merge_skewed m1 m2 ~combine
    let find = Map.find
    let find_exn = Map.find_exn

    let add_cell ptr cell m =
      let new_cell_set = if Map.mem m ptr
                         then Set.add (find_exn m ptr) cell
                         else Set.singleton cell
      in
      Map.set m ~key:ptr ~data:new_cell_set

    let get_overlapping ptr m =
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

module Make(N : NumericDomain) = struct
  module Env = NumericEnv(N)
  module C = Cell(N)
  module Ptr = Pointer(N)
  module CellSet = C.Set
  module BaseSet = Region.Set

  type basemap = BaseSet.t BaseSetMap.t
  type cellset = CellSet.t
  type cellmap = C.Map.t
  type env = Env.t
  type t = { cells: cellmap; env: env; bases: basemap }

  let empty : t = { cells = C.Map.empty;
                    env = Env.empty;
                    bases = BaseSetMap.empty }

  let make_pointer = Ptr.make

  let set_ptr ~name ~region ~offs ~width {cells; env; bases} =
    let env = Env.set name offs env in
    let bases = BaseSetMap.set bases
                  ~key:name
                  ~data:(BaseSet.singleton region)
    in
    { cells; env; bases }

  let unptr ~name {cells; env; bases} : t =
    let new_env = Env.set name N.bot env in
    let new_bases = BaseSetMap.remove bases name in
    {cells; env=new_env; bases=new_bases}

  let set_rsp (offs : int) (mem : t) : t =
    let offs = N.of_int ~width:64 offs in
    set_ptr mem
      ~name:"RSP"
      ~region:Region.Stack
      ~offs
      ~width:`r64

  let set_rbp (offs : int) (mem : t) : t =
    let offs = N.of_int ~width:64 offs in
    set_ptr mem
      ~name:"RBP"
      ~region:Region.Stack
      ~offs
      ~width:`r64

  let is_pointer ~name {cells; env; bases} : bool =
    BaseSetMap.mem bases name

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

  (* let get_value_at_index (c : t) (idx : N.t) = *)
  (*     fun mem finder ->  *)
  (*     let one = N.of_int 1 in *)
  (*     let eight = N.of_int 8 in *)
  (*     let zero_idx_byte_width = N.sub (N.div c.width eight) one in *)
  (*     let shift_left_n = N.sub zero_idx_byte_width idx in *)
  (*     let shift_right_n = zero_idx_byte_width in *)
  (*     let cell_name = name c in *)
  (*     let shifted_left = N.lshift (finder mem cell_name) shift_left_n in *)
  (*     N.rshift shifted_left shift_right_n *)

  (** For a given pointer, ptr, if ptr points to an existing memory
      cell, then return the cell and memory. otherwise, if ptr does
      not pointer to an existing memory cell, then create it,
      initialize the cell's value to N.top, find all overlapping cells,
      merge the overlapping values, update the cell's value based on the
      overlapping values, and return the cell and updated memory *)
  let realize ptr mem : C.t * t =
    let maybe_existing_cell = match C.Map.find mem.cells ptr with
      | Some cellset -> Set.find cellset ~f:(fun c -> C.equals_ptr c ptr)
      | None -> None
    in
    let final_cell = match maybe_existing_cell with
      | Some cel -> cel
      | None -> C.t_of_ptr ptr
    in
    let final_cell_name = C.name final_cell in
    let final_cell_already_has_value = Env.mem mem.env final_cell_name in
    if final_cell_already_has_value
    then final_cell, mem
    else
      let new_mem = set_cell_to_top final_cell_name mem in
      final_cell, new_mem

  let load_from_offs_and_regions ~offs ~regions ~width (mem : t) : N.t =
    let ptrs = Ptr.of_regions ~regions ~offs ~width in
    let cells = List.fold ptrs ~init:(C.Set.empty)
                  ~f:(fun foundcells p ->
                    match C.Map.find mem.cells p with
                    | Some cells -> C.Set.union cells foundcells
                    | None -> foundcells)
    in
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
  let load ~name ~width (mem : t) : N.t =
    match get_offset ~name mem with
    | Some offs ->
       let regions = match BaseSetMap.find mem.bases name with
         | Some regions -> regions
         | None -> let err_msg = sprintf "Attempt to read from non-pointer: %s" name in
                   failwith err_msg
       in
       load_from_offs_and_regions ~offs ~regions ~width mem
    | None ->
       let err_msg = sprintf "Attempt to read from non-pointer: %s" name in
       failwith err_msg
  
  let store ~offs ~region ~width ~data ~valtype mem : t =
    let ptr = Ptr.make ~region ~offs ~width in
    let overlap = C.Map.get_overlapping ptr mem.cells in
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

  (* TODO: type consistency in cell merging *)
  let merge mem1 mem2 : t =
    let {cells=cells1; env=env1; bases=bases1} = mem1 in
    let {cells=cells2; env=env2; bases=bases2} = mem2 in
    let merged_cells = C.Map.merge cells1 cells2 in
    let merged_env = Env.merge env1 env2 in
    let merged_bases = BaseSetMap.merge bases1 bases2 in
    {cells=merged_cells; env=merged_env; bases=merged_bases}
end
