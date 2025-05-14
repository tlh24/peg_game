open Core
(*open Pairing_heap*)

(* --- Core Types --- *)

(* Cell states are now chars directly *)
let cell_empty = '.'
let cell_one = '1'
let cell_cross = 'X'
let cell_circle = 'C'
let cell_zeroed_one = 'Z' (* '1' that was cancelled *)

type move_type = MoveX | MoveC (* Prefixed to avoid conflict with X, C chars *)
type move_info = { move_type : move_type; r : int; c : int }
type path = move_info list

(* Grid type using Bigarray of chars *)
type grid_t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

type action_details = {
  heuristic_val : int; (* Negative of (cleared '1's + 1 for the piece) *)
  move : move_info;
}

type tree_node = {
  id : int;
  grid_state : grid_t;
  path_from_root : path;
  depth : int;
  (*parent : tree_node option;*) (* never used! *)
  mutable children : (move_info * tree_node) list;
  mutable potential_actions : action_details list option;
  is_solved_state : bool;
  (*mutable next_alt_idx_to_explore : int;*)
}

type pq_entry = {
  alt_heuristic : int; (* Primary sort key *)
  parent_depth : int;  (* Secondary sort key *)
  parent_node_id : int;(* Tie-breaker *)
  parent_node : tree_node;
  alt_idx : int;
}

(* --- Global Mutable State --- *)
let global_state = object
  val mutable min_moves_overall = Int.max_value
  val mutable best_path_overall = None
  val mutable exploration_step_counter = 0
  val mutable node_id_counter = 0
  val mutable initial_grid_for_display : grid_t option = None

  method get_min_moves = min_moves_overall
  method set_min_moves v = min_moves_overall <- v
  method get_best_path = best_path_overall
  method set_best_path v = best_path_overall <- v
  method incr_exploration_steps () = exploration_step_counter <- exploration_step_counter + 1; exploration_step_counter
  method get_exploration_steps = exploration_step_counter
  method get_next_node_id () = node_id_counter <- node_id_counter + 1; node_id_counter
  method set_initial_grid_for_display g = initial_grid_for_display <- Some g
  method get_initial_grid_for_display = initial_grid_for_display
  method reset () =
    min_moves_overall <- Int.max_value;
    best_path_overall <- None;
    exploration_step_counter <- 0;
    node_id_counter <- 0;
    initial_grid_for_display <- None;
end

(* --- Grid Operations --- *)
let get_grid_dims grid = (Bigarray.Array2.dim1 grid, Bigarray.Array2.dim2 grid)

let copy_grid grid:grid_t =
  let new_grid = Bigarray.Array2.create Bigarray.char Bigarray.c_layout
                   (Bigarray.Array2.dim1 grid) (Bigarray.Array2.dim2 grid) in
  Bigarray.Array2.blit grid new_grid;
  new_grid

let parse_grid_from_file filename : grid_t option =
  try
    let lines = In_channel.read_lines filename in
    match lines with
    | [] -> None
    | first_line :: rest_lines ->
        let cols = String.length first_line in
        if cols = 0 then None else
        let rows = 1 + List.length rest_lines in
        let all_lines = first_line :: rest_lines in
        if not (List.for_all ~f:(fun line -> String.length line = cols) all_lines) then
          (Printf.eprintf "Error: Puzzle file does not contain a rectangular grid.\n"; None)
        else
          let arr = Bigarray.Array2.create Bigarray.char Bigarray.c_layout rows cols in
          List.iteri all_lines ~f:(fun r line ->
            String.iteri line ~f:(fun c char_val ->
              let cell_char = match char_val with (* Directly use chars *)
                | '.' | '0' -> cell_empty
                | '1' -> cell_one
                | 'X' -> cell_cross (* Should not be in input *)
                | 'C' -> cell_circle (* Should not be in input *)
                | 'Z' -> cell_zeroed_one (* Should not be in input *)
                | _ as c -> if Char.is_whitespace c then cell_empty (* Handle potential trailing spaces if lines are not perfectly stripped*)
                            else failwith ("Invalid character in puzzle: " ^ String.make 1 c)
              in
              arr.{r,c} <- cell_char (* Bigarray syntax *)
            )
          );
          Some arr
  with Sys_error _ -> Printf.eprintf "Error: Puzzle file '%s' not found or unreadable.\n" filename; None

let print_grid_ocaml ?(highlight_pos: (int * int) option = None) ?(message: string option = None) (grid: grid_t) : unit =
  Option.iter message ~f:(Printf.printf "%s\n");
  let rows, cols = get_grid_dims grid in
  if rows = 0 then () else
  let line_len = (2 * cols -1) in (* Adjust for spaces *)
  Printf.printf "%s\n" (String.make line_len '-');
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let cell_char = grid.{r,c} in (* Bigarray syntax *)
      let is_highlight = match highlight_pos with
        | Some (hr, hc) -> hr = r && hc = c
        | None -> false in
      if is_highlight then Printf.printf "[%c]" cell_char
      else Printf.printf " %c " cell_char;
    done;
    Printf.printf "\n";
  done;
  Printf.printf "%s\n" (String.make line_len '-');
  Out_channel.flush stdout

let is_one x = Char.compare x cell_one = 0
let isnt_one x = Char.compare x cell_one <> 0
let is_empty x = Char.compare x cell_empty = 0
let is_cross x = Char.compare x cell_cross = 0
let is_circle x = Char.compare x cell_circle = 0
let is_zeroed x = Char.compare x cell_zeroed_one = 0

let count_active_ones (grid:grid_t) : int =
  let rows, cols = get_grid_dims grid in
  let count = ref 0 in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      if is_one grid.{r,c} then count := !count + 1
    done
  done;
  !count

let is_grid_solved grid = count_active_ones grid = 0

(* --- Move Application --- *)
let apply_cross_to_grid (grid_in: grid_t) (r_move: int) (c_move: int) : (grid_t * int) =
  let grid = copy_grid grid_in in
  let rows, cols = get_grid_dims grid in
  let cleared_count = ref 0 in

  if r_move < 0 || r_move >= rows || c_move < 0 || c_move >= cols || isnt_one grid.{r_move,c_move} then
    (grid, 0)
  else begin
    grid.{r_move,c_move} <- cell_cross;
    (* Horizontal right *)
    (try for c = c_move + 1 to cols - 1 do
        match grid.{r_move,c} with
        | cc when is_one cc -> grid.{r_move,c} <- cell_zeroed_one; incr cleared_count
        | cc when is_empty cc || is_cross cc || is_circle cc || is_zeroed cc -> raise Exit
        | _ -> raise Exit (* Should not happen if grid only contains valid chars *)
    done with Exit -> ());
    (* Horizontal left *)
    (try for c = c_move - 1 downto 0 do
        match grid.{r_move,c} with
        | cc when is_one cc -> grid.{r_move,c} <- cell_zeroed_one; incr cleared_count
        | cc when is_empty cc || is_cross cc || is_circle cc || is_zeroed cc -> raise Exit
        | _ -> raise Exit
    done with Exit -> ());
    (* Vertical down *)
    (try for r = r_move + 1 to rows - 1 do
        match grid.{r,c_move} with
        | cc when is_one cc -> grid.{r,c_move} <- cell_zeroed_one; incr cleared_count
        | cc when is_empty cc || is_cross cc || is_circle cc || is_zeroed cc -> raise Exit
        | _ -> raise Exit
    done with Exit -> ());
    (* Vertical up *)
    (try for r = r_move - 1 downto 0 do
        match grid.{r,c_move} with
        | cc when is_one cc -> grid.{r,c_move} <- cell_zeroed_one; incr cleared_count
        | cc when is_empty cc || is_cross cc || is_circle cc || is_zeroed cc -> raise Exit
        | _ -> raise Exit
    done with Exit -> ());
    (grid, !cleared_count)
  end

let apply_circle_to_grid (grid_in: grid_t) (r_move: int) (c_move: int) : (grid_t * int) =
  let grid = copy_grid grid_in in
  let rows, cols = get_grid_dims grid in
  let cleared_count = ref 0 in

  if r_move < 0 || r_move >= rows || c_move < 0 || c_move >= cols || isnt_one grid.{r_move,c_move} then
    (grid, 0)
  else begin
    grid.{r_move,c_move} <- cell_circle;
    for dr = -1 to 1 do
      for dc = -1 to 1 do
        if not (dr = 0 && dc = 0) then
          let nr, nc = r_move + dr, c_move + dc in
          if nr >= 0 && nr < rows && nc >= 0 && nc < cols && is_one grid.{nr,nc} then
            (grid.{nr,nc} <- cell_zeroed_one; incr cleared_count)
      done
    done;
    (grid, !cleared_count)
  end

let apply_move (grid_in: grid_t) (move: move_info) : (grid_t * int) =
  match move.move_type with
  | MoveX -> apply_cross_to_grid grid_in move.r move.c
  | MoveC -> apply_circle_to_grid grid_in move.r move.c

(* --- Action Generation --- *)
let get_sorted_actions (grid: grid_t) : action_details list =
  let rows, cols = get_grid_dims grid in
  let actions = ref [] in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      if is_one grid.{r,c} then begin
        List.iter [MoveX; MoveC] ~f:(fun move_type ->
          let current_move = { move_type; r; c } in
          let _, zs_cleared = apply_move grid current_move in (* Eval on copy *)
          let heuristic_val = -(zs_cleared + 1) in
          actions := { heuristic_val; move = current_move } :: !actions
        )
      end
    done
  done;
  List.sort ~compare:(fun a1 a2 ->
      Int.compare a1.heuristic_val a2.heuristic_val
    ) !actions


(* --- TreeNode Creation --- *)
let make_tree_node (*?(parent: tree_node option = None)*) (grid: grid_t) (path: path) (depth: int) : tree_node =
  {
    id = global_state#get_next_node_id ();
    grid_state = grid;
    path_from_root = path;
    depth = depth;
    children = [];
    potential_actions = None;
    is_solved_state = is_grid_solved grid;
  }

(* --- Animation Helper --- *)
let print_animated_display (current_node_grid: grid_t) (current_node_path: path)
                           ?(focus_coord: (int*int) option) (phase_msg: string)
                            : unit =
  let step = global_state#incr_exploration_steps () in
  let best_len = global_state#get_min_moves in
  let best_path_opt = global_state#get_best_path in

  Printf.printf "\027[2J\027[H";
  Out_channel.flush stdout; (* Clear screen *)

  let path_len = List.length current_node_path in
  let header = Printf.sprintf "%s (Step: %d, PathLen: %d)" phase_msg step path_len in
  let highlight_pos = match focus_coord with
    | Some coord -> Some coord
    | None ->
        (match List.last current_node_path with (* Use List.last for safety *)
         | Some last_move -> Some (last_move.r, last_move.c)
         | None -> None)
  in
  let ones_now = count_active_ones current_node_grid in
  let sub_header1 = Printf.sprintf "   Active '1's: %d " ones_now in
  let best_len_str = if best_len = Int.max_value then "None" else Int.to_string best_len in
  let sub_header2 = Printf.sprintf "   Best Solution Length: %s" best_len_str in

  print_grid_ocaml ~highlight_pos ~message:( Some (Printf.sprintf "%s\n%s\n%s" header sub_header1 sub_header2)) current_node_grid;

  begin match best_path_opt, global_state#get_initial_grid_for_display with
  | Some bp, Some ig ->
      let best_grid_reconstructed = ref (copy_grid ig) in
      List.iter bp ~f:(fun move ->
        let (next_g, _) = apply_move !best_grid_reconstructed move in
        best_grid_reconstructed := next_g
      );
      print_grid_ocaml ~message:(Some (Printf.sprintf "\n--- Current Best Solution (%d moves) ---" (List.length bp))) !best_grid_reconstructed
  | _ -> ()
  end;
  ()

(* --- Priority Queue for Branching --- *)
module PQ = struct
  let compare_pq_entry e1 e2 = (* alt_heuristic (primary), parent_depth (secondary) *)
    let c1 = Int.compare e1.alt_heuristic e2.alt_heuristic in
    if c1 <> 0 then c1 else
    let c2 = Int.compare e1.parent_depth e2.parent_depth in
    if c2 <> 0 then c2 else
    Int.compare e1.parent_node_id e2.parent_node_id (* Tie-breaker *)

  type t = pq_entry Pairing_heap.t
  let create () : t = Pairing_heap.create ~cmp:compare_pq_entry ()
  let push (heap: t) (entry: pq_entry) : unit = Pairing_heap.add heap entry
  let pop (heap: t) : pq_entry option = Pairing_heap.pop heap
  let is_empty (heap: t) : bool = Pairing_heap.is_empty heap
end

(* --- Solver Logic --- *)
let ensure_actions_loaded (node: tree_node) : unit =
  if Option.is_none node.potential_actions then
    node.potential_actions <- Some (get_sorted_actions node.grid_state)

let add_node_alternatives_to_pq
    (node: tree_node) (frontier_pq: PQ.t) : unit =
  (* call this whenever creating a new node! *)
  ensure_actions_loaded node;
  let actions = Option.value_exn node.potential_actions in
  try
    if node.depth + 1 < global_state#get_min_moves then (
      List.iteri ~f:(fun i action ->
        let pq_entry_data = {
          alt_heuristic = action.heuristic_val;
          parent_depth = node.depth;
          parent_node_id = node.id;
          parent_node = node;
          alt_idx = i; (* indexes potential_actions in the parent_node *)
        } in
        PQ.push frontier_pq pq_entry_data
        ) actions
    ) else (
      raise Exit (* Pruning: no more alternatives from this node will be better *)
    );
  with Exit -> ()

let rec rollout_from_node (start_node: tree_node)
                          (frontier_pq: PQ.t)
                          (phase_prefix: string)
                          : (tree_node * bool) =

  print_animated_display start_node.grid_state
                        start_node.path_from_root
                        phase_prefix;

  if start_node.is_solved_state then begin
    if start_node.depth < global_state#get_min_moves then begin
      (* new best! *)
      global_state#set_min_moves start_node.depth;
      global_state#set_best_path (Some start_node.path_from_root);
      Printf.printf "\n*** New Best Solution: %d moves! (At start of rollout %s) ***\n" global_state#get_min_moves phase_prefix;
      Out_channel.flush stdout;
      print_animated_display start_node.grid_state (Option.value_exn global_state#get_best_path) "NEW BEST!";
      ignore (Core_unix.nanosleep 0.5);
      (start_node, true)
    end else (start_node, false)
  end else if start_node.depth >= global_state#get_min_moves then
    (start_node, false) (* Pruned by depth at start of rollout *)
  else begin
    ensure_actions_loaded start_node;
    match Option.value_exn start_node.potential_actions with
    | [] -> (start_node, false) (* Dead end *)
    | greedy_action :: _ ->
        let move = greedy_action.move in
        let next_grid, _ = apply_move start_node.grid_state move in
        let next_depth = start_node.depth + 1 in
        let next_path = start_node.path_from_root @ [move] in

        let child = make_tree_node next_grid next_path next_depth in
        start_node.children <- (move, child) :: start_node.children;
        add_node_alternatives_to_pq child frontier_pq;
        rollout_from_node child frontier_pq phase_prefix (* Tail call if OCaml optimizes this context *)
  end

let solve_puzzle_main (initial_grid_param: grid_t) : path option =
  global_state#reset ();
  global_state#set_initial_grid_for_display initial_grid_param;

  let root_node = make_tree_node initial_grid_param [] 0 in
  let exploration_frontier_pq = PQ.create () in
  add_node_alternatives_to_pq root_node exploration_frontier_pq;

  Printf.printf "\n--- Initial Greedy Rollout, fill the PQ ---\n";
  Out_channel.flush stdout;
  let _, _ = rollout_from_node root_node exploration_frontier_pq "Initial Greedy Rollout" in

  Printf.printf "\n--- Exploring Alternative Branches (Priority Queue) ---\n";
  Out_channel.flush stdout;
  (*let processed_pq_choices = Caml.Hashtbl.create 1000 in*)

  while not (PQ.is_empty exploration_frontier_pq) do
    (* this loop removes just one item from the pq,
      but it adds many more in the course of roll-outs! *)
    ( match PQ.pop exploration_frontier_pq with
    | None -> ()
    | Some pq_entry ->
        let { parent_node = branch_parent; alt_idx; _ } = pq_entry in

        (*branch_parent.next_alt_idx_to_explore <- alt_idx + 1;*) (* don't need this, managed by the pq.  game is *ordered* so won't revisit *)

        if Option.is_none branch_parent.potential_actions then (
          Printf.printf "Error: branch_parent.potential_actions empty!\n"
        );
        (*ensure_actions_loaded branch_parent;*) (* this should always be true *)
        let potential_actions = Option.value_exn branch_parent.potential_actions in

        if alt_idx < List.length potential_actions && branch_parent.depth + 1 < global_state#get_min_moves then
          let action_to_take = List.nth_exn potential_actions alt_idx in
          let move_info = action_to_take.move in

          let phase_msg = Printf.sprintf "Branch from N%d D%d, AltIdx %d (H:%d)"
                            branch_parent.id branch_parent.depth alt_idx action_to_take.heuristic_val in
          print_animated_display branch_parent.grid_state branch_parent.path_from_root
                                phase_msg ~focus_coord:(move_info.r, move_info.c);

          let next_grid, _ = apply_move branch_parent.grid_state move_info in
          let next_depth = branch_parent.depth + 1 in
          let next_path = branch_parent.path_from_root @ [move_info] in


          let branch_child_node = make_tree_node next_grid next_path next_depth in
          branch_parent.children <- (move_info, branch_child_node) :: branch_parent.children;
          add_node_alternatives_to_pq branch_child_node exploration_frontier_pq;
          ignore( rollout_from_node branch_child_node exploration_frontier_pq
                                    (Printf.sprintf "Rollout Post-Branch N%d-A%d" branch_parent.id alt_idx) ) ;

      );
  done;
  Printf.printf "\n--- Search Complete. Total exploration steps: %d ---\n" global_state#get_exploration_steps ;
  global_state#get_best_path


(* --- Main Execution --- *)
let () =
  let puzzle_file = "puzzl.txt" in
  (match Stdlib.Sys.file_exists puzzle_file with
    | true -> ()
    | _ ->
      Printf.printf "'%s' not found. Creating default.\n" puzzle_file;
      let default_content =
        ".11111.1111111\n.11.11111.1111\n.111111.1.11.1\n1.1111111.111.\n"
        ^ "11.11.1.1..111\n..111.11111111\n.111..1...111.\n.111111111.111\n.1.11111.11111\n" in
      Out_channel.write_all puzzle_file ~data:default_content;
  );

  match parse_grid_from_file puzzle_file with
  | None -> Printf.eprintf "Failed to parse puzzle.\n"
  | Some initial_grid ->
      let initial_ones = count_active_ones initial_grid in
      Printf.printf "\027[2J\027[H";
      print_grid_ocaml ~message:(Some "Initial Puzzle State:") initial_grid;
      Printf.printf "Total active '1's to clear: %d\n" initial_ones;
      Printf.printf "\n--- Info ---\nStrategy: Priority Queue Branching\n";
      Printf.printf "----------------------\n\nStarting Solver...\n";
      Out_channel.flush stdout;

      let final_solution_path_opt =
        try solve_puzzle_main initial_grid
        with
        (*| Sys_unix.Break -> Printf.printf "\n\nSearch interrupted.\n"; None*)
        | e -> Printf.eprintf "\n\nError: %s\n%s\n" (Exn.to_string e) (Printexc.get_backtrace ()); None
      in

      match final_solution_path_opt with
      | Some final_path ->
          Printf.printf "\n--- Optimal Solution Found ---\n";
          if List.is_empty final_path && initial_ones > 0 then Printf.printf "Error: No path found.\n"
          else if List.is_empty final_path && initial_ones = 0 then
             (print_grid_ocaml ~message:(Some "Final Solved State (0 moves):") initial_grid;
              Printf.printf "\nOptimal solution: 0 moves.\n")
          else
            let final_reconstructed_grid = ref (copy_grid initial_grid) in
            List.iter final_path ~f:(fun move ->
              let (next_g, _) = apply_move !final_reconstructed_grid move in
              final_reconstructed_grid := next_g);
            print_grid_ocaml ~message:(Some (Printf.sprintf "Final Solved State (Optimal: %d moves):" (List.length final_path))) !final_reconstructed_grid;
            Printf.printf "\nOptimal solution: %d moves.\nSequence:\n" (List.length final_path);
            List.iteri final_path ~f:(fun i move ->
              let type_char = match move.move_type with MoveX -> 'X' | MoveC -> 'C' in
              Printf.printf "  %d. %c at (%d, %d)\n" (i+1) type_char move.r move.c)
      | None -> Printf.printf "\nSolver finished: No solution or error.\n"
