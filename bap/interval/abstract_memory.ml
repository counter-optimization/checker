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
      Format.printf "%s%!" @@ to_string x
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
      Format.printf "%s%!" @@ to_string ptr
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
      let valtype_str = Type_domain.to_string m.valtype in
      sprintf "%s-%s" (name m) valtype_str

    let pp (m : t) : unit =
      Format.printf "%s%!" @@ to_string m

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
             bases: basemap;
             img: Image.t option }
  type regions = Region.Set.t
  type valtypes = Common.cell_t

  type 'a err = ('a, Error.t) Result.t

  let empty : t = { cells = C.Map.empty;
                    env = Env.empty;
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

  let get_bases : N.t -> Bases_domain.t =
    match N.get Bases_domain.key with
    | Some f -> f
    | None -> failwith "Couldn't extract bases information out of product domain"

  let set_taint (prod : N.t) : N.t =
    N.set Checker_taint.Analysis.key prod Checker_taint.Analysis.Taint

  let set_untaint (prod : N.t) : N.t =
    N.set Checker_taint.Analysis.key prod Checker_taint.Analysis.Notaint

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
  
  let equal {cells=cells1; env=env1; bases=bases1; _}
        {cells=cells2; env=env2; bases=bases2; _} =
    Env.equal env1 env2 &&
      Map.equal (Set.equal) cells1 cells2 &&
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

  let cells_of_offs_and_regions ~(offs : Wrapping_interval.t) ~regions ~(width : Wrapping_interval.t) (mem : t) : C.Set.t =
    let ptrs = Ptr.of_regions ~regions ~offs ~width in
    List.fold ptrs ~init:C.Set.empty
      ~f:(fun acc ptr ->
        C.Set.union acc @@
          Map.fold mem.cells ~init:C.Set.empty
            ~f:(fun ~key ~data acc ->
              if Ptr.equal ptr key
              then C.Set.union acc data
              else acc))

  let load_from_offs_and_regions
        ~(offs : Wrapping_interval.t)
        ~regions
        ~(width : Wrapping_interval.t)
        (mem : t)
      : N.t err =
    let ptrs = Ptr.of_regions ~regions ~offs ~width in
    let cells = cells_of_offs_and_regions ~offs ~regions ~width mem in
    match C.Set.length cells with
    | 0 ->
       let ptr_strings = List.fold ptrs ~init:"" ~f:(fun acc x ->
                             acc ^ " " ^ Ptr.to_string x)
       in
       let () = printf "Didn't find cells for ptrs: %s\n%!" ptr_strings in
       let () = printf "Setting to untainted top...\n%!" in
       (* Ok N.top *)
       Ok (set_untaint N.top)
    | 1 ->
       Ok (C.Set.fold cells ~init:N.bot ~f:(fun valset c ->
               let celname = C.name c in
               N.join valset @@ lookup celname mem))
    | _ ->
       let ptr_strings = List.fold ptrs ~init:"" ~f:(fun acc x ->
                             acc ^ " " ^ Ptr.to_string x)
       in
       Or_error.error_string @@
         sprintf "in load_from_offs_and_regions: No support for reading overlapping ptrs: %s" ptr_strings

  let load_global (offs : Wrapping_interval.t) (sz : size) (m : t) : N.t err =
    match m.img with
    | None -> Or_error.error_string "load_global: memory's image should be set before load"
    | Some img ->
       let segs = Image.segments img in
       match Wrapping_interval.to_int offs with
       | None ->
          begin
            let offs_s = Wrapping_interval.to_string offs in
            Or_error.error_string @@ sprintf "load_global: couldn't convert offs %s to address for image" offs_s
          end
       | Some addr ->
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
                let res_s = N.to_string res in
                let () = printf "in load_global, loaded data was %s\n%!" res_s in
                Ok res

  let store ~(offs : Wrapping_interval.t) ~region
        ~(width : Wrapping_interval.t) ~data ~valtype mem
      : t err =
    let ptr = Ptr.make ~region ~offs ~width in
    let () = printf "storing to ptr %s\n%!" (Ptr.to_string ptr) in
    let overlap = C.Map.get_overlapping ptr mem.cells in
    let () = Set.iter overlap ~f:(fun c ->
                 printf "overlapping cell: %s\n%!" (C.to_string c)) in
    if C.Set.length overlap > 1
    then
      let overlap = C.Set.fold overlap ~init:""
                      ~f:(fun acc c -> acc ^ " " ^ (C.name c))
      in
      Or_error.error_string @@
        sprintf "No support for storing with overlapping ptrs %s" overlap
    else
      let cel = if C.Set.length overlap = 1
                then List.hd_exn @@ C.Set.to_list overlap
                else C.t_of_ptr ~valtype ptr
      in
      let celname = C.name cel in
      if not (C.equals_ptr cel ptr)
      then
        Or_error.error_string @@
          sprintf "Can't store ptr %s to cell %s" (Ptr.to_string ptr) celname
      else
        Ok { mem with env = Env.set celname data mem.env;
                      cells = C.Map.add_cell ptr cel mem.cells }

  let global_exists ~(offs : Wrapping_interval.t)
        ~(width : Wrapping_interval.t) (mem : t) : bool =
    let regions = Region.Set.from_region Region.Global in 
    let cells = cells_of_offs_and_regions ~offs ~regions ~width mem in
    0 < C.Set.length cells
  
(* on first load of global, load from the image into the abstract
     memory environment, then do the load a usual. this is unsound force
     general programs in the case of a previous overlapping store,
     but the cryptographic libs shouldn't do this, so ignoring
     overlap/nonalignment for now
   *)
  let load_of_bil_exp (e : Bil.exp) (idx_res : N.t) (m : t) : N.t err =
    match e with
    | Bil.Load (_mem, idx, _endian, size) ->
       let () = Format.printf "in load_of_bil_exp, start\n%!" in
       let () = Format.printf "in load_of_bil_exp, exp is %s\n%!"
                  (Sexp.to_string (Bil.sexp_of_exp e))
       in
       let load_from_vars = Var_name_collector.run idx in
       let regions = BaseSetMap.bases_of_vars load_from_vars m.bases in

       let () = Format.printf "in load_of_bil_exp, getting offs\n%!" in
       let offs = get_intvl idx_res in
       let offs_type = compute_type idx m in
       let offs_is_scalar = CellType.is_scalar offs_type in
       let () = Format.printf "in load_of_bil_exp, done getting offs\n%!" in

       let () = Format.printf "in load_of_bil_exp, getting width\n%!" in
       let width = bap_size_to_absdom size in
       let () = Format.printf "in load_of_bil_exp, done getting width\n%!" in

       let () = Format.printf "in load_of_bil_exp, checking if global\n%!" in
       let is_global = offs_is_scalar && Set.is_empty regions in
       let () = Format.printf "in load_of_bil_exp, is_global: %B\n%!" is_global in
       let () = Format.printf "in load_of_bil_exp, done checking if global\n%!" in

       let () = Format.printf "in load_of_bil_exp, geting regions\n%!" in
       let regions = if is_global
                     then Set.add regions Region.Global
                     else regions
       in
       let () = Format.printf "in load_of_bil_exp, done getting regions\n%!" in

       let () = Format.printf "in load_of_bil_exp, checking if global xists\n%!" in
       let glob_xists = (global_exists ~offs ~width m) in
       let () = Format.printf "in load_of_bil_exp, done checking if global xists\n%!" in
       
       let open Or_error.Monad_infix in
       (if is_global && not glob_xists
        then
          load_global offs size m >>= fun data ->
          let region = Region.Global in
          let valtype = CellType.Unknown in
          store ~offs ~region ~width ~data ~valtype m >>= fun m' ->
          Ok (m', Region.Set.from_region region)
        else
          Ok (m, regions))
       >>= fun (m', regions') ->
       let () = Format.printf "in load_of_bil_exp, calling load_from_offs_and_regions\n%!" in
       let res = load_from_offs_and_regions m' ~offs ~regions:regions' ~width in
       let () = Format.printf "in load_of_bil_exp, done load_from_offs_and_regions\n%!" in
       res
    | _ -> Or_error.error_string "load_of_bil_exp: Not a load in load_of_bil_exp"
  
  (* on first store to a global, just treat it as every other store. *)
  let store_of_bil_exp (e : Bil.exp) ~(offs : N.t) ~data ~valtype m : t err =
    match e with
    | Bil.Store (_mem, idx, v, _endian, size) -> 
      begin
        let vars = Var_name_collector.run idx in
        let width = bap_size_to_absdom size in
        let bases_to_load_from = BaseSetMap.bases_of_vars vars m.bases in
        let bases_to_load_from = if Set.is_empty bases_to_load_from
                                 then Region.Set.from_region Region.Global
                                 else bases_to_load_from
        in
        let offs = get_intvl offs in
        Set.fold bases_to_load_from ~init:(Ok m) ~f:(fun env base ->
            Or_error.bind env ~f:(fun env ->
                store ~offs ~region:base ~width ~data ~valtype env))
      end
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

  (* have to handle:
     1) don't havoc stack slots that hold frame ptr, return ptr
   *)
  let havoc_on_call (mem : t) : t =
    let havoc_one_cell (mem : t) (cname : string) : t =
      { mem with env = Env.set cname N.top mem.env }
    in
    let cell_name_of_ptr (p : Ptr.t) : string =
      C.t_of_ptr p |> C.name
    in
    let havoc_rax (mem : t) : t =
      let rax = "RAX" in
      { mem with env = Env.set rax N.top mem.env }
    in
    let cell_map = mem.cells in
    let cells = Map.keys cell_map in
    let cell_names = List.map cells ~f:cell_name_of_ptr in
    List.fold cell_names ~init:mem ~f:havoc_one_cell
    |> havoc_rax

  let set_rsp (offs : int) (mem : t) : t err =
    let offs = N.of_int ~width:64 offs in
    let offs_as_ptr = N.set Type_domain.key offs CellType.Ptr in
    let offs_with_base = N.set Bases_domain.key offs_as_ptr Bases_domain.stack in
    setptr mem
      ~name:"RSP"
      ~regions:(BaseSet.singleton Region.Stack)
      ~offs:offs_with_base
      ~width:`r64
    |> store_init_ret_ptr

  let set_rbp (offs : int) (mem : t) : t err =
    let offs = N.of_int ~width:64 offs in
    let offs_as_ptr = N.set Type_domain.key offs CellType.Ptr in
    let offs_with_base = N.set Bases_domain.key offs_as_ptr Bases_domain.stack in
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
    let {cells=cells1; env=env1; bases=bases1; img=img1} = mem1 in
    let {cells=cells2; env=env2; bases=bases2; img=img2} = mem2 in
    let merged_img = merge_images img1 img2 in
    let merged_cells = C.Map.merge cells1 cells2 in
    let merged_env = Env.merge env1 env2 in
    let merged_bases = BaseSetMap.merge bases1 bases2 in
    { cells=merged_cells;
      env=merged_env;
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
    let widen steps n mem1 mem2 =
      {cells = widen_cell_map prev next;
       env = widen_env steps n prev next;
       bases = widen_bases_map prev next;
       img = merge_images prev.img next.img }
    in
    widen steps n prev next

  let pp {cells; env; bases}  =
    let print_ptr_to_cells ~key ~data =
      let cell_set_str =  Set.to_list data |> List.to_string ~f:C.to_string in
      Format.printf "\t%s --> %s\n%!" (Ptr.to_string key) cell_set_str
    in
    let print_bases_map ~key ~data =
      let region_set_str = Set.to_list data |> List.to_string ~f:Region.to_string in
      Format.printf "\t%s --> %s\n%!" key region_set_str
    in
    printf "* Ptr->Cells map is:\n%!";
    Map.iteri cells ~f:print_ptr_to_cells;
    Env.pp env;
    printf "* Var->Bases map is:\n%!";
    Map.iteri bases ~f:print_bases_map
end
