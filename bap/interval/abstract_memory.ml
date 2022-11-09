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
    let fold = Set.fold
    let map = Set.map (module Cmp)
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

    let fix_overlap (added_cell : T.t) (overlap_cells : Set.t) (m : t) : t =
      Set.fold overlap_cells ~init:m
        ~f:(fun m cel ->
          let ptr = ptr_of_t cel in
          let old_cellset = find_exn m ptr in
          let new_cellset = Set.add old_cellset added_cell in
          set ~key:ptr ~data:new_cellset m)
         
    let add_cell ptr valtype m : t =
      let ptr_base = Pointer.region ptr in
      let ptr_offs = Pointer.offs ptr in
      (* TODO: this should check for other similar pointers of different *)
      (* widths *)
      let all_ptrs = Pointer.all_widths_of_ptr ptr in
      if Map.mem m ptr
      then
        begin
          let base_str = Region.to_string ptr_base in
          let offs_str = N.to_string ptr_offs in
          failwith @@
            sprintf "Cell for ptr (%s, %s) already exists" base_str offs_str
        end
      else
        let ptr_width = Pointer.width ptr in
        let new_cell = T.make
                         ~region:ptr_base
                         ~offs:ptr_offs
                         ~width:ptr_width
                         ~valtype
        in
        let overlapping_cells = get_overlapping ptr m in
        let withnew = Set.add overlapping_cells new_cell in
        let new_m = Map.set m ~key:ptr ~data:withnew in
        fix_overlap new_cell overlapping_cells new_m
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

  let is_scalar ~name {cells; env; bases} : bool =
    not @@ BaseSetMap.mem bases name

  let get_offset ~name {cells; env; bases} : N.t option =
    if is_pointer name {cells; env; bases}
    then Some (Env.lookup name env)
    else None

  let lookup ~name {cells; env; bases} : N.t =
    Env.lookup name env

  let set ~name ~data (mem : t) : t =
    { mem with env = Env.set name data mem.env }

  (* let realize_cells_for_ptr (p : Ptr.t) (m : t) : t = *)
  (*   let reg = Ptr.region p in *)
  (*   let offs = Ptr.offs p in *)
  (*   let width = Ptr.width p in *)
  (*   (\* Do the cell realization *\) *)
  (*   (\* If the cell already exists, then just return it *\) *)
  (*   (\* otherwise, create a cell and add it to the cells and env *)
  (*    before returning it*\) *)
  (*   let cell = match C.Map.find m.cells p with *)
  (*     | Some cel -> cel *)
  (*     | None -> C.make ~region:reg ~offs ~width ~valtype:C.Scalar *)

  let realize ptr mem : C.t * t =
      let found = match C.Map.find mem.cells ptr with
        | Some cellset -> Set.find cellset ~f:(fun c -> C.equals_ptr c ptr)
        | None -> None
      in
      let cel = match found with
        | Some c -> c
        | None -> C.t_of_ptr ptr
      in
      let cell_name = C.name cel in
      let cell_has_value = Env.mem mem.env cell_name in
      if cell_has_value
      then cel, mem
      else
        cel, { mem with env = Env.set cell_name N.top mem.env }

  (** Here, name is the name of the var in the mem env that
      holds the offset of the pointer *)
  let load ~name ~width (mem : t) : t =
    if not (is_pointer ~name mem)
    then
      failwith @@ sprintf "Cannot load from pointer with offs var: %s" name
    else
      begin
        (* find_exn same since is_pointer guarantees find finds something *)
        let bases = BaseSetMap.find_exn mem.bases name in
        let offs = match Env.find mem.env name with
          | Some o -> o
          | None ->
             failwith @@ sprintf "Couldn't find offset for pointer %s" name
        in
        let ptrs = Ptr.of_regions ~regions:bases ~width ~offs in
        let exact_cells = List.fold ptrs ~init:(C.Set.empty)
                            ~f:(fun acc p ->
                              match C.Map.find mem.cells p with
                              | Some cels ->
                              | None -> acc
        let overlapping_cells = List.fold ptrs ~init:(C.Set.empty)
                                  ~f:(fun acc p ->
                                    C.Map.get_overlapping p mem.cells)
        in
        mem
      end

  (* TODO: type consistency in cell merging *)
  let merge mem1 mem2 : t =
    let {cells=cells1; env=env1; bases=bases1} = mem1 in
    let {cells=cells2; env=env2; bases=bases2} = mem2 in
    let merged_cells = C.Map.merge cells1 cells2 in
    let merged_env = Env.merge env1 env2 in
    let merged_bases = BaseSetMap.merge bases1 bases2 in
    {cells=merged_cells; env=merged_env; bases=merged_bases}
end
