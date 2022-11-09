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
  type t = { region: Region.t; offs: N.t; width: Size.t }
             [@@deriving sexp_of, compare]

  let make ~region ~offs ~width = { region; offs; width }
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
               width: Size.t }
               [@@deriving sexp_of, compare]
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
  type env = Env.t
  type t = { cells: cellset; env: env; bases: basemap }

  let empty : t = { cells = CellSet.empty;
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

  (* TODO: type consistency in cell merging *)
  let merge mem1 mem2 : t =
    let {cells=cells1; env=env1; bases=bases1} = mem1 in
    let {cells=cells2; env=env2; bases=bases2} = mem2 in
    let merged_cells = CellSet.merge cells1 cells2 in
    let merged_env = Env.merge env1 env2 in
    let merged_bases = BaseSetMap.merge bases1 bases2 in
    {cells=merged_cells; env=merged_env; bases=merged_bases}
end
