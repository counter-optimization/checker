open Core
open Bap.Std
open Common

(* This is an implementation based on the paper 'Field-Sensitive Value *)
(* Analysis of Embedded C Programs with Union Types and Pointer *)
(* Arithmetics'. *)
(* currently assumes: *)
(* - little endian architecture *)

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

(** Cells *)
module Cell(N : NumericDomain) = struct
  module T = struct
    type t = { region: Region.t;
               offs: Wrapping_interval.t;
               valtype: CellType.t;
               width: Wrapping_interval.t } [@@deriving sexp_of]

    let compare x y =
      let reg_eq = Region.equal x.region y.region in
      let offs_eq = Wrapping_interval.equal x.offs y.offs in
      let width_eq = Wrapping_interval.equal x.width y.width in
      let valtype_eq = match x.valtype, y.valtype with
        | Scalar, Scalar -> true
        | Ptr, Ptr -> true
        | Undef, Undef -> true
        | Unknown, _ -> true
        | _ , Unknown -> true
        | _, _ -> false
      in
      if reg_eq && offs_eq && width_eq && valtype_eq
      then 0
      else -1

    let same_address c1 c2 : bool =
      Region.equal c1.region c2.region &&
        Wrapping_interval.equal c1.offs c2.offs

    let same_cell c1 c2 : bool =
      same_address c1 c2 &&
        Wrapping_interval.equal c1.width c2.width

    let bits_per_byte = Wrapping_interval.of_int 8
    
    let width_in_bits c = c.width
    
    let width_in_bytes c =
      let width = width_in_bits c in
      Wrapping_interval.div width bits_per_byte

    let width_in_bytes_int (cell : t) : int Or_error.t =
      Or_error.(
        Wrapping_interval.to_int cell.width >>= fun width ->
        Ok (width / 8)
      )

    let get_intvl : N.t -> Wrapping_interval.t =
      match N.get Wrapping_interval.key with
      | Some f -> f
      | None -> failwith "Couldn't extract interval information out of product domain, in module Cell"

    let make ~region ~offs ~width ~valtype : t =
      { region; offs; width; valtype }

    (* let ptr_of_t { region; offs; width; valtype } : Pointer.t = *)
    (*   Pointer.make ~region ~offs ~width *)

    (* let t_of_ptr ?(valtype = CellType.Scalar) p : t = *)
    (*   let region = Pointer.region p in *)
    (*   let width = Pointer.width_in_bits p in *)
    (*   let offs = Pointer.offs p in *)
    (*   make ~region ~offs ~width ~valtype *)

    (* let equals_ptr cel ptr : bool = *)
    (*   let reg = Pointer.region ptr in *)
    (*   let offs = Pointer.offs ptr in *)
    (*   let width = Pointer.width_in_bits ptr in *)
    (*   Region.equal cel.region reg && *)
    (*     Wrapping_interval.could_be_true (Wrapping_interval.booleq cel.offs offs) && *)
    (*       Wrapping_interval.could_be_true (Wrapping_interval.booleq cel.width width) *)

    let name (m : t) : string =
      let reg_str = Region.to_string m.region in
      let off_str = Wrapping_interval.to_string m.offs in
      let width_str = Wrapping_interval.to_string m.width in
      sprintf "%s-%s-%s" reg_str off_str width_str

    let to_string m : string =
      let valtype_str = Type_domain.to_string m.valtype in
      sprintf "%s-%s" (name m) valtype_str

    let pp (m : t) : unit =
      Format.printf "%s\n%!" @@ to_string m

    (* let overlaps_with_ptr cel ptr : bool = *)
    (*   if not (Region.equal (Pointer.region ptr) cel.region) *)
    (*   then false *)
    (*   else *)
    (*     let open Wrapping_interval in *)
    (*     let one = of_int 1 in *)
    (*     let ptr_base = Pointer.offs ptr in *)
    (*     let ptr_end = add (Pointer.offs ptr) (Pointer.width_in_bytes ptr) in *)
    (*     let ptr_end = sub ptr_end one in *)
    (*     let cel_base = cel.offs in *)
    (*     let cel_end = add cel.offs (width_in_bytes cel) in *)
    (*     let cel_end = sub cel_end one in *)
    (*     (could_be_true (boolle ptr_base cel_base) && *)
    (*        could_be_true (boolle cel_base ptr_end)) || *)
    (*       (could_be_true (boolle ptr_base cel_end) && *)
    (*          could_be_true (boolle cel_end ptr_end)) *)

    let overlaps cel other : bool =
      if not (Region.equal other.region cel.region)
      then false
      else
        let open Wrapping_interval in
        let one = of_int 1 in
        
        let other_base = other.offs in
        let other_end = add other_base (width_in_bytes other) in
        let other_end = sub other_end one in
        
        let cel_base = cel.offs in
        let cel_end = add cel.offs (width_in_bytes cel) in
        let cel_end = sub cel_end one in
        
        (could_be_true (boolle other_base cel_base) &&
           could_be_true (boolle cel_base other_end)) ||
          (could_be_true (boolle other_base cel_end) &&
             could_be_true (boolle cel_end other_end))
  end

  module Cmp = struct
    include T
    include Comparator.Make(T)
  end

  include Cmp
  
  module Set = struct
    (* include Set.Make_binable_using_comparator(Cmp) *)
    type t = (Cmp.t, comparator_witness) Set.t

    let empty : t = Set.empty (module Cmp)
    
    let of_list = Set.of_list (module Cmp)
    
    let singleton = Set.singleton (module Cmp)
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
  
  module CellSet = C.Set
  
  module SS = Set.Make_binable_using_comparator(String)

  module WI = Wrapping_interval

  module T = struct
    type basemap = BaseSet.t BaseSetMap.t
  
    type cellset = CellSet.t
    
    type env = Env.t
    
    type t = { cells: cellset;
               env: env;
               bases: basemap;
               img: Image.t option;
               globals_read : cellset }
    
    type regions = Region.Set.t
    
    type valtypes = Common.cell_t

    type 'a err = ('a, Error.t) Result.t
  end

  module Overlap = struct
    type offs = WI.t list
      
    type t = { cell : C.t ; offsets : offs ; data : N.t }

    let of_existing_cell (cell : C.t) (mem : T.t) : t =
      let int_width = match C.width_in_bytes_int cell with
        | Ok i -> i
        | Error e ->
           failwith @@
             sprintf "in Overlap.of_cell, Couldn't turn cell width WI into Int: %s" @@
               Error.to_string_hum e in

      (* e.g., if a pointer points to offs, X, and points to a value of width u32, then
         generate a list containing [X; X+1; X+2; X+3] *)
      let relative_offsets = List.init int_width ~f:(fun x -> x) in
      let absolute_offsets = List.map relative_offsets ~f:(fun reloffs ->
                                        let wi_offs = Wrapping_interval.of_int reloffs in
                                        WI.add cell.offs wi_offs) in

      let cellname = C.name cell in
      let data = Env.lookup cellname mem.env in

      { cell ; offsets = absolute_offsets ; data }

    let of_cell (cell : C.t) (data : N.t) (mem : T.t) : t =
      let int_width = match C.width_in_bytes_int cell with
        | Ok i -> i
        | Error e ->
           failwith @@
             sprintf "in Overlap.of_cell, Couldn't turn cell width WI into Int: %s" @@
               Error.to_string_hum e in
      (* e.g., if a pointer points to offs, X, and points to a value of width u32, then
         generate a list containing [X; X+1; X+2; X+3] *)
      let relative_offsets = List.init int_width ~f:(fun x -> x) in
      let absolute_offsets = List.map relative_offsets ~f:(fun reloffs ->
                                        let wi_offs = Wrapping_interval.of_int reloffs in
                                        WI.add cell.offs wi_offs) in
      { cell ; offsets = absolute_offsets ; data }

    let extract_byte (data : N.t) (byte_offs : int) : N.t =
      let low_bit_idx = 8 * byte_offs in
      let high_bit_idx = low_bit_idx + 7 in
      N.extract data high_bit_idx low_bit_idx

    let merge_bytes = N.meet

    let shift_byte (data : N.t) (count_as_bits : int) : N.t =
      let count_as_abstract_value = N.of_int count_as_bits in
      N.lshift data count_as_abstract_value

    let data_from_little_endian_bytes (le_bytes : N.t list) : N.t =
      let rec loop (bytes : N.t list) ?(idx : int = 0) (acc : N.t) : N.t =
        match bytes with
        | [] -> acc
        | byte :: bytes' ->
           let to_add = shift_byte byte idx in
           let new_acc = N.logor to_add acc in
           loop bytes' new_acc ~idx:(idx + 1)
      in
      let start = N.of_int 0 in
      loop le_bytes start ~idx:0

    let get_merge_lists ~(this : t) ~(other : t) : (N.t * N.t) list =
      let lt x y = WI.could_be_true (WI.boollt x y) in
      let gt x y = WI.could_be_true (WI.boollt y x) in
      let rec loop ?(tidx : int = 0) ?(oidx : int = 0)
                   ?(rev_acc : (N.t * N.t) list = [])
                   (toffs : WI.t list) (ooffs : WI.t list) : (N.t * N.t) list =
        match toffs, ooffs with
        | [], [] -> List.rev rev_acc
        | [], _ -> List.rev rev_acc
        | t :: ts, [] ->
           let this_byte = extract_byte this.data tidx in
           loop ts ooffs ~tidx:(tidx + 1) ~oidx ~rev_acc:(List.cons (this_byte, this_byte) rev_acc)
        | t :: ts, o :: os ->
           if lt t o
           then
             let this_byte = extract_byte this.data tidx in
             loop ts ooffs ~tidx:(tidx + 1) ~oidx ~rev_acc:(List.cons (this_byte, this_byte) rev_acc)
           else
             if gt t o
             then
               loop toffs os ~tidx ~oidx:(oidx + 1) ~rev_acc
             else
               let this_byte = extract_byte this.data tidx in
               let other_byte = extract_byte other.data oidx in
               loop ts os ~tidx:(tidx + 1) ~oidx:(oidx + 1) ~rev_acc:(List.cons (this_byte, other_byte) rev_acc)
             
      in
      loop this.offsets other.offsets ~tidx:0 ~oidx:0 ~rev_acc:[]

    (* returns ~this merged with ~other. where the final overlap value is based on
       ~this. i.e., if merging memory cell C1 (as ~this) that contains u64 with overlapping memory
       cell C2 (as ~other) that contains u64, then this will return a value of overlap.t type
       representing the C1's contents with C2 overlain as a value that contains both contents (but
       from the viewpoint of C1).
     *)
    let merge ~(this : t) ~(other : t) : t =
      let bytes_to_merge = get_merge_lists ~this ~other in
      let merged_le_bytes = List.map bytes_to_merge ~f:(fun (tb, ob) -> merge_bytes tb ob) in
      let merged_data = data_from_little_endian_bytes merged_le_bytes in
      { this with data = merged_data }
  end

  include T

  let pp {cells; env; bases; globals_read}  =
    (* let print_ptr_to_cells ~key ~data = *)
    (*   let cell_set_str =  Set.to_list data |> List.to_string ~f:C.to_string in *)
    (*   Format.printf "\t%s --> %s\n%!" (Ptr.to_string key) cell_set_str *)
    (* in *)
    let print_bases_map ~key ~data =
      let region_set_str = Set.to_list data |> List.to_string ~f:Region.to_string in
      Format.printf "\t%s --> %s\n%!" key region_set_str
    in
    printf "* Ptr->Cells map is:\n%!";
    Set.iter cells ~f:C.pp;
    Env.pp env;
    printf "* Var->Bases map is:\n%!";
    Map.iteri bases ~f:print_bases_map;
    printf "* Loaded globals are:\n%!";
    Set.iter globals_read ~f:C.pp

  let empty : t = { cells = C.Set.empty;
                    env = Env.empty;
                    bases = BaseSetMap.empty;
                    globals_read = C.Set.empty;
                    img = None }

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "Couldn't extract interval information out of product domain"

  let get_typd : N.t -> Type_domain.t =
    match N.get Type_domain.key with
    | Some f -> f
    | None -> failwith "Couldn't extract type information out of product domain"

  let get_bases : N.t -> Bases_domain.t =
    match N.get Bases_domain.key with
    | Some f -> f
    | None -> failwith "Couldn't extract bases information out of product domain"

  let get_taint : N.t -> Checker_taint.Analysis.t =
    match N.get Checker_taint.Analysis.key with
    | Some f -> f
    | None -> failwith "Couldn't extract taint information out of product domain"

  let set_taint (prod : N.t) : N.t =
    N.set Checker_taint.Analysis.key prod Checker_taint.Analysis.Taint

  let set_untaint (prod : N.t) : N.t =
    N.set Checker_taint.Analysis.key prod Checker_taint.Analysis.Notaint

  let set_typd (prod : N.t) (typ : Type_domain.t) : N.t =
    N.set Type_domain.key prod typ

  let set_based (prod : N.t) (typ : Bases_domain.t) : N.t =
    N.set Bases_domain.key prod typ

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
  
  let equal {cells=cells1; env=env1; bases=bases1; _}
        {cells=cells2; env=env2; bases=bases2; _} =
    Env.equal env1 env2 &&
      Set.equal cells1 cells2 &&
        Map.equal (Set.equal) bases1 bases2

  let setptr ~(name:string) ~regions ~offs ~width m =
    let env = Env.set name offs m.env in
    let bases = BaseSetMap.set m.bases
                  ~key:name
                  ~data:regions
    in
    { m with env = env; bases = bases }

  let unptr ~name mem : t =
    let new_env = Env.set name N.bot mem.env in
    let new_bases = BaseSetMap.remove mem.bases name in
    { mem with env=new_env; bases=new_bases }

  let is_pointer ~name {cells; env; bases; _} : bool =
    BaseSetMap.mem bases name

  let holds_ptr (name : string) (env : t) : bool =
    is_pointer ~name env

  let is_scalar ~name mem : bool =
    not @@ is_pointer ~name mem

  let get_offset ~name mem : N.t option =
    if is_pointer ~name mem
    then Some (Env.lookup name mem.env)
    else None

  let lookup name m : N.t =
    Env.lookup name m.env

  let set name data (mem : t) : t =
    { mem with env = Env.set name data mem.env }

  let unset name (mem : t) : t =
    { mem with env = Env.unset name mem.env }

  let set_cell_to_top cell_name (mem : t) : t =
    { mem with env = Env.set cell_name N.top mem.env }

  let init_arg ~(name : string) (mem : t) : t =
    let heap_or_stack = Bases_domain.join Bases_domain.heap Bases_domain.stack in
    let init_arg_val = set_based N.top heap_or_stack in
    set name init_arg_val mem

  let get_overlapping_cells (cell : C.t) (mem : t) : C.Set.t =
    Set.filter mem.cells ~f:(C.overlaps cell)

  let load_global (offs : Wrapping_interval.t) (sz : size) (m : t) : N.t err =
    match m.img with
    | None -> Or_error.error_string "load_global: memory's image should be set before load"
    | Some img ->
       let segs = Image.segments img in
       match Wrapping_interval.to_int offs with
       | Error e ->
          let offs_s = Wrapping_interval.to_string offs in
          Or_error.error_string @@
            sprintf
              "load_global: couldn't convert offs %s to address for image: %s" offs_s (Error.to_string_hum e)

       | Ok addr ->
          let addr_w = Word.of_int ~width:64 addr in
          let target_seg = Table.find_addr segs addr_w in
          match target_seg with
          | None ->
             begin
               
               let addr_s = Word.to_string addr_w in
               Or_error.error_string @@ sprintf "load_global: couldn't find addr %s in image" addr_s
             end
          | Some (mem, seg) ->
             match Memory.get ~addr:addr_w ~scale:sz mem with
             | Error e ->
                begin
                  let segname = Image.Segment.name seg in
                  let addr_s = Word.to_string addr_w in
                  let err_s = Error.to_string_hum e in
                  Or_error.error_string @@ sprintf "load_global: Error reading address %s from seg %s: %s" addr_s segname err_s
                end
             | Ok data ->
                let res = N.of_word data in
                (* let res_s = N.to_string res in *)
                (* let () = printf "in load_global, loaded data was %s\n%!" res_s in *)
                Ok res

  let set_cell_to_top (c : C.t) ?(secret : bool = false) (mem : t) : t =
    let top = if secret then set_taint N.top else set_untaint N.top in
    { mem with env = Env.set (C.name c) top mem.env }

  let remove_cell (c : C.t) (mem : t) : t =
    let () = printf "in remove_cell, removing cell %s\n%!" (C.name c) in
    let cells' = Set.remove mem.cells c in
    let env' = Env.unset (C.name c) mem.env in
    { mem with cells = cells'; env = env' }

  let store ~(offs : Wrapping_interval.t) ~region
            ~(width : Wrapping_interval.t) ~data ~valtype mem : t err =
    let open Or_error.Monad_infix in
    
    let cel = C.make ~region ~offs ~width ~valtype in
    let celname = C.name cel in
    
    let () = printf "storing to cell %s\n%!" (C.to_string cel) in

    let overlap = get_overlapping_cells cel mem in

    let () = printf "in store, overlapping cells are:\n%!" in
    let () = Set.iter overlap ~f:(fun c ->
                        printf "overlapping cell: %s\n%!" (C.to_string c)) in
    let () = printf "in store, deleting overlapping cells\n%!" in
    
    let mem' = Set.fold overlap ~init:mem ~f:(fun mem' c -> remove_cell c mem') in

    let old_env = mem'.env in
    let old_cells = mem'.cells in
    let res = Ok { mem' with env = Env.set celname data old_env;
                             cells = Set.add old_cells cel } in
    if Set.length overlap > 1
    then
      let () = printf "in store: did propagate unstore, here is the out state:\n%!" in
      let () = pp mem' in
      res
    else
      res

  let global_already_read ~(cell : C.t) ~(mem : t) : bool =
    match Set.find mem.cells ~f:(C.same_cell cell) with
    | Some _ -> true
    | None -> false

  let cell_exists ~(cell : C.t) ~(mem : t) : bool =
    match Set.find mem.cells ~f:(C.same_cell cell) with
    | Some _ -> true
    | None -> false
  
  let load ~(offs : Wrapping_interval.t) ~(width : Wrapping_interval.t)
           ~(size : size) ~(region : Region.t) ~(mem : t) : (N.t * t) err =
    let open Or_error.Monad_infix in
    
    let cell = C.make ~offs ~width ~region ~valtype:CellType.Unknown in
    let () = printf "in load, cell is %s\n%!" (C.to_string cell) in

    let is_global = match region with | Global -> true | _ -> false in
    let global_already_loaded_from_img = global_already_read ~cell ~mem in
  
    if is_global && not global_already_loaded_from_img
    then
      let () = printf "in load, loading global from img\n%!" in
      
      load_global offs size mem >>= fun data ->
      let valtype = CellType.Unknown in
      (* and set it in the current env, so next read doesn't go to the image *)
      store ~offs ~region ~width ~data ~valtype mem >>= fun mem' ->
      Ok (data, { mem' with globals_read = Set.add mem'.globals_read cell })
    else
      let has_existing_value = cell_exists ~cell ~mem in
      if has_existing_value
      then
        let () = printf "in load, loading existing value\n%!" in
        let data = lookup (C.name cell) mem in
        Ok (data, mem)
      else
        let () = printf "in load, loading new value\n%!" in
        let overlap = get_overlapping_cells cell mem in
        let () = printf "in load, overlapping cells are: \n%!" in
        let () = Set.iter overlap ~f:(fun c -> printf "overlap cell: %s\n%!" (C.name c)) in 
        
        let cell_as_overlap = Overlap.of_cell cell N.top mem in
        
        if Set.length overlap >= 1
        then
          let () = printf "in load, doing overlapping load\n%!" in
          let final = Set.fold overlap ~init:cell_as_overlap ~f:(fun current_data other_cell ->
                                 let other_overlapper = Overlap.of_existing_cell other_cell mem in
                                 Overlap.merge ~this:current_data ~other:other_overlapper) in
          Ok (final.data, mem)
        else
          Ok (N.top, mem)
  
(* on first load of global, load from the image into the abstract
     memory environment, then do the load a usual. this is unsound force
     general programs in the case of a previous overlapping store,
     but the cryptographic libs shouldn't do this, so ignoring
     overlap/nonalignment for now
   *)
  let load_of_bil_exp (e : Bil.exp) (idx_res : N.t)
        (size : Size.t) (m : t) : (N.t * t) err =
    match e with
    | Bil.Load (_mem, idx, _endian, size) ->
       (* let () = printf "in load_of_bil_exp, getting load from vars\n%!" in *)
       (* let load_from_vars = Var_name_collector.run idx in *)
       
       (* let () = printf "in load_of_bil_exp, load from vars: \n%!" in *)
       (* let () = SS.iter load_from_vars ~f:(fun v -> printf "%s\n%!" v) in *)
       
       (* let () = printf "in load_of_bil_exp, getting regions\n%!" in *)
       (* let regions = BaseSetMap.bases_of_vars load_from_vars m.bases in *)
       let regions : Region.Set.t = get_bases idx_res in
       let () = printf "in load_of_bil_exp, regions to load from:\n%!" in
       let () = Set.iter regions ~f:Region.pp in

       (* let () = printf "in load_of_bil_exp, getting offs intvl\n%!" in *)
       let offs = get_intvl idx_res in
       
       (* let () = printf "in load_of_bil_exp, computing type\n%!" in *)
       (* let offs_type = compute_type idx m in *)
       let offs_type = get_typd idx_res in
       
       (* let () = printf "in load_of_bil_exp, computing is scalar\n%!" in *)
       let offs_is_scalar = CellType.is_scalar offs_type in

       (* let () = printf "in load_of_bil_exp, computing width\n%!" in *)
       let width = bap_size_to_absdom size in

       let offs_size = match Wrapping_interval.size offs with
         | Some offs_size -> offs_size
         | None ->
            failwith @@
              sprintf "in load_of_bil_exp, couldn't convert offs %s to Z.t"
                      (Wrapping_interval.to_string offs) in
            
       let max_ptd_to_elts = Z.of_int 64 in

       (if Z.gt offs_size max_ptd_to_elts
        then
          let () = printf "in load_of_bil_exp, pointer %s points to too many elements. maybe an unconstrained pointer?\n%!" (Exp.to_string idx) in
          let () = printf "in load_of_bil_exp, returning top instead\n%!" in
          Ok (N.top, m)
        else
          (* let () = printf "in load_of_bil_exp, computing is_global\n%!" in *)
          let is_global = offs_is_scalar && Set.is_empty regions in

          (* let () = printf "in load_of_bil_exp, computing region\n%!" in *)
          let regions = (if is_global
                         then Set.add regions Region.Global
                         else regions) |> Set.to_list
          in

          let open Or_error.Monad_infix in
          
          Wrapping_interval.to_list offs >>= fun all_offsets ->

          (* let () = printf "in load_of_bil_exp, all offsets are:\n%!" in *)
          (* let () = List.iter all_offsets ~f:(fun o -> *)
          (*                      printf "offset: %s\n%!" (Wrapping_interval.to_string o)) in *)

          (* let () = printf "in load_of_bil_exp, all regions are:\n%!" in *)
          (* let () = List.iter regions ~f:(fun r -> *)
          (*                      printf "region: %s\n%!" (Region.to_string r)) in *)
          
          let regions_and_offsets = List.cartesian_product regions all_offsets in

          (* let () = printf "in load_of_bil_exp, region,offset pairs are:\n%!" in *)
          (* let () = List.iter regions_and_offsets ~f:(fun (r, o) -> *)
          (*                      printf "reg,off: (%s, %s)\n%!" *)
          (*                             (Region.to_string r) *)
          (*                             (Wrapping_interval.to_string o)) in *)
          
          List.fold regions_and_offsets
                    ~init:(Ok (N.bot, m))
                    ~f:(fun state (region, offs) ->
                      state >>= fun (data_acc, mem) ->
                      load ~offs ~region ~width ~size ~mem >>= fun (loaded_val, mem') ->
                      Ok (N.join data_acc loaded_val, mem')))
    | _ -> Or_error.error_string "load_of_bil_exp: Not a load in load_of_bil_exp"

  (* don't let pointers point to too many locations. right now,
     this will error if the pointer points to more than 8 members
     of ~width~
   *)
  let ensure_offs_range_is_ok ~(offs : Wrapping_interval.t)
        ~(width : Wrapping_interval.t)
      : Z.t err =
    let size = match Wrapping_interval.size offs with
    | Some sz -> Ok sz
    | None ->
       Or_error.error_string @@
         sprintf "in ensure_offs_range_is_ok, can't get size of offs (WI.t): %s"
                 (Wrapping_interval.to_string offs) in
    Or_error.bind size ~f:(fun size ->
    let max_pointers = Z.of_int 64 in
    if Z.gt size max_pointers
    then
      Or_error.error_string
        "in ensure_offs_range_is_ok, pointer points to too many data members, probably an unconstrained pointer"
    else
      Ok size)

  let get_all_offs ~(offs : Wrapping_interval.t)
        ~(width : Wrapping_interval.t)
      : Wrapping_interval.t list err =
    let open Or_error.Monad_infix in
    
    (match Wrapping_interval.to_z width with
      | Some w -> Ok w
      | None ->
         Or_error.error_string "Couldn't convert width to int in get_all_offs")
  
    >>= fun z_width ->

    let mem_idx_to_offs (mnum : Z.t) : Wrapping_interval.t err =
      let offs_offs = Z.mul mnum z_width in
      let base = Wrapping_interval.to_z_lo offs in
      match base with
      | Some l ->
         let new_base = Z.add l offs_offs in
         Ok (Wrapping_interval.of_z new_base)
      | None ->
         Or_error.error_string
           "in get_all_offs, couldn't get lo part of offs interval"
    in
    
    ensure_offs_range_is_ok ~offs ~width >>= fun num_members ->

    let num_members_int = Z.to_int num_members in

    List.init num_members_int ~f:(fun mem_idx ->
        Z.of_int mem_idx |> mem_idx_to_offs)

    |> List.fold ~init:(Ok []) ~f:(fun acc offs ->
           offs >>= fun offs ->
           acc >>= fun offs_list ->
           Ok (List.cons offs offs_list))

  let set_stack_canary (mem : t) : t =
    let fs_base = 0x0000_4000 in

    let fs_ptr = set_typd (N.of_int fs_base) CellType.Ptr in
    let fs_ptr = set_based (fs_ptr) Bases_domain.heap in
    
    let stack_canary_width = Wrapping_interval.of_int 8 in
    
    let stack_canary_value = N.of_int 0x123456 in

    let with_fs_base_set = setptr
                             ~name:"FS_BASE"
                             ~regions:Bases_domain.heap
                             ~offs:fs_ptr
                             ~width:stack_canary_width
                             empty in

    let env_with_canary_set = store
                                ~offs:(Wrapping_interval.of_int (fs_base + 0x28))
                                ~region:Region.Heap
                                ~width:stack_canary_width
                                ~data:stack_canary_value
                                ~valtype:CellType.Scalar
                                with_fs_base_set in

    match env_with_canary_set with
    | Ok env' -> env'
    | Error e ->
       failwith "in set_stack_canary, couldn't set value of canary"
    
  (* on first store to a global, just treat it as every other store. *)
  let store_of_bil_exp (e : Bil.exp) ~(offs : N.t) ~data ~size m
      : t err =
    match e with
    | Bil.Store (_mem, idx, v, _endian, size) ->
       (* let () = printf "in store_of_bil_exp, getting var names\n%!" in *)
       let vars = Var_name_collector.run idx in
       
       (* let () = printf "in store_of_bil_exp, getting width\n%!" in *)
       let width = bap_size_to_absdom size in

       (* let () = printf "in store_of_bil_exp, getting bases_to_load_from\n%!" in *)
       let bases_to_load_from = BaseSetMap.bases_of_vars vars m.bases in
       
       (* let () = printf "in store_of_bil_exp, getting bases_to_load_from final\n%!" in *)
       let bases_to_load_from = (if Set.is_empty bases_to_load_from
                                then Region.Set.from_region Region.Global
                                 else bases_to_load_from)
                                |> Region.Set.to_list in

       let valtype = get_typd data in
       
       (* let () = printf "in store_of_bil_exp, getting intvl offs\n%!" in *)
       let offs = get_intvl offs in

       (match ensure_offs_range_is_ok ~offs ~width with
       | Error err ->
          let () = printf "in store_of_bil_exp, denote of exp %s, offs: %s : %s\n%!"
                          (Exp.to_string e)
                          (Wrapping_interval.to_string offs)
                          (Error.to_string_hum err) in
          Ok m
       | Ok _ ->
          let all_offs = get_all_offs ~offs ~width in

          Or_error.bind all_offs ~f:(fun all_offs ->

          (* let () = printf "in store_of_bil_exp, all offsets are:\n%!" in *)
          (* let () = List.iter all_offs ~f:(fun offswi -> *)
          (*              printf "%s\n%!"  *)
          (*                (Wrapping_interval.to_string offswi)) *)
          (* in *)

          (* let () = printf "in store_of_bil_exp, doing stores\n%!" in *)
          (* let () = printf "in store_of_bil_exp, store to offs %s\n%!" *)
          (*            (Wrapping_interval.to_string offs) *)
          (* in *)
          (* let () = printf "in store_of_bil_exp, store to bases:\n%!" in *)
          (* let () = List.iter bases_to_load_from ~f:Region.pp in *)

          (* let () = printf "in store_of_bil_exp, data is %s\n%!" *)
          (*            (N.to_string data) *)
          (* in *)

          let base_offs_pairs = List.cartesian_product bases_to_load_from all_offs in

          List.fold base_offs_pairs ~init:(Ok m) ~f:(fun env (base, offs) ->
              Or_error.bind env ~f:(fun env ->
                  store ~offs ~region:base ~width ~data ~valtype env))))
    | _ -> Or_error.error_string
             "store_of_bil_exp: store got non-store expression"

  (* for init the mem on the outermost API call.
     store N.top *)
  let store_init_ret_ptr (mem : t) : t err =
    let open Or_error.Monad_infix in 
    let rsp = "RSP" in
    let rsp_offs = Env.lookup rsp mem.env in
    let rsp_offs_wi = get_intvl rsp_offs in

    (match BaseSetMap.find mem.bases rsp with
    | Some bases -> (if Set.length bases <> 1
                     then
                       Or_error.error_string
                         "store_init_ret_ptr: RSP should only have stack as its base before calling store_init_ret_ptr"
                     else
                       Ok (Set.to_list bases |> List.hd_exn))
    | None -> Or_error.error_string
                "store_init_ret_ptr: RSP should be setup before calling store_init_ret_ptr")
    >>= fun stack_region ->
    let rsp_width = Wrapping_interval.of_int 64 in
    let init_val = set_untaint N.top in
    let init_ret_ptr = N.set Type_domain.key init_val CellType.Ptr in
    store mem
      ~offs:rsp_offs_wi
      ~region:stack_region
      ~width:rsp_width
      ~data:init_ret_ptr
      ~valtype:CellType.Ptr

  let havoc_on_call (mem : t) : t =
    let havoc_one_cell (mem : t) (cname : string) : t =
      { mem with env = Env.set cname N.top mem.env }
    in
    let havoc_rax (mem : t) : t =
      let rax = "RAX" in
      { mem with env = Env.set rax N.top mem.env }
    in
    (* let cell_map = mem.cells in *)
    let cells = mem.cells in
    let cell_names = Set.to_list cells |> List.map ~f:C.name in
    List.fold cell_names ~init:mem ~f:havoc_one_cell
    |> havoc_rax

  let set_rsp (offs : int) (mem : t) : t err =
    let offs = N.of_int ~width:64 offs in
    let offs_as_ptr = set_typd offs CellType.Ptr in
    let offs_with_base = set_based offs_as_ptr Bases_domain.stack in
    setptr mem
      ~name:"RSP"
      ~regions:(BaseSet.singleton Region.Stack)
      ~offs:offs_with_base
      ~width:`r64
    |> store_init_ret_ptr

  let set_rbp (offs : int) (mem : t) : t err =
    let offs = N.of_int ~width:64 offs in
    let offs_as_ptr = set_typd offs CellType.Ptr in
    let offs_with_base = set_based offs_as_ptr Bases_domain.stack in
    Ok (setptr mem
          ~name:"RBP"
          ~regions:(BaseSet.singleton Region.Stack)
          ~offs:offs_with_base
          ~width:`r64)

  let merge_images img1 img2 : Image.t option =
    if Option.is_some img1
    then img1
    else
      if Option.is_some img2
      then img2
      else None

  (* TODO: type consistency in cell merging *)
  let merge mem1 mem2 : t =
    let {cells=cells1; env=env1; bases=bases1; img=img1; globals_read=gr1} = mem1 in
    let {cells=cells2; env=env2; bases=bases2; img=img2; globals_read=gr2} = mem2 in
    let merged_globals_read = Set.union gr1 gr2 in
    let merged_img = merge_images img1 img2 in
    let merged_cells = Set.union cells1 cells2 in
    let merged_env = Env.merge env1 env2 in
    let merged_bases = BaseSetMap.merge bases1 bases2 in
    { cells=merged_cells;
      env=merged_env;
      bases=merged_bases;
      img=merged_img;
      globals_read=merged_globals_read }

  let widen_threshold = 256

  let widen_with_step steps n prev next : t =
    let widen_cell_map {cells=prev; _} {cells=next; _} : C.Set.t =
      Set.union prev next
    in
    let widen_bases_map {bases=prev; _} {bases=next; _} : basemap =
      BaseSetMap.merge prev next
    in
    let widen_env steps n {env=prev; _} {env=next; _} : Env.t =
      Env.widen_with_step steps n prev next
    in
    let widen_globals_read {globals_read=prev; _} {globals_read=next; _} : C.Set.t =
      Set.union prev next
    in
    let widen steps n mem1 mem2 =
      {cells = widen_cell_map prev next;
       env = widen_env steps n prev next;
       bases = widen_bases_map prev next;
       img = merge_images prev.img next.img;
       globals_read = widen_globals_read prev next }
    in
    widen steps n prev next
end
