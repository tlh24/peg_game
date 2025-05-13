import os
import time
import heapq
import copy

try:
    from termcolor import colored
except ImportError:
    print("The 'termcolor' package is not installed. Please install it: pip install termcolor")
    print("Displaying without colors.")
    def colored(text, color=None, on_color=None, attrs=None): return str(text)

# --- Global State ---
min_moves_overall = float('inf')
best_path_overall = None # This will store the list of moves for the best solution
initial_grid_for_display = None # Store the very first grid to reconstruct best solution display
visited_states = {} # {grid_string: min_depth_reached}
_exploration_step_counter = [0]
_node_id_counter = 0 # For unique TreeNode IDs if needed for PQ tie-breaking

def get_next_node_id():
    global _node_id_counter
    _node_id_counter += 1
    return _node_id_counter

# --- TreeNode Class ---
class TreeNode:
    def __init__(self, grid_state, path_from_root, depth, parent=None):
        self.id = get_next_node_id()
        self.grid_state = grid_state
        self.grid_string = grid_to_string(grid_state)
        self.path_from_root = path_from_root # List of move_info tuples
        self.depth = depth
        self.parent = parent
        self.children = [] # List of tuples: (move_info_taken, child_treenode_object)

        self.potential_actions = [] # Computed lazily: list of {"heuristic", "id", "move_info"}
        self.is_solved_state = is_solved(grid_state)

        self.next_alternative_action_idx_to_explore = 1

    def __lt__(self, other):
        return self.id < other.id

def parse_puzzle(filename="puzzl.txt"):
    grid = []
    try:
        with open(filename, 'r') as f:
            for line in f:
                stripped_line = line.strip()
                if stripped_line: grid.append(list(stripped_line))
        if not grid: return None
        first_row_len = len(grid[0])
        if not all(len(row) == first_row_len for row in grid): return None
    except FileNotFoundError: return None
    return grid

def grid_to_string(grid):
    return "".join("".join(row) for row in grid)

def _print_grid_core(grid, highlight_pos=None, message=None, max_cols=None):
    if message: print(message)
    rows, cols = len(grid), len(grid[0]) if grid else 0
    if rows == 0: return

    display_cols = cols
    if max_cols and cols > max_cols : # For potentially wide best solution path display
        display_cols = max_cols

    line_len = (2 * display_cols - 1) if display_cols > 0 else 0
    if line_len > 0: print("-" * line_len)

    for r_idx, row_data in enumerate(grid):
        row_display = []
        for c_idx, char_val in enumerate(row_data):
            if c_idx >= display_cols: break # Truncate if too wide for this display
            attrs_val = []
            if highlight_pos and r_idx == highlight_pos[0] and c_idx == highlight_pos[1]:
                attrs_val = ['bold', 'underline']

            if char_val == '1': row_display.append(colored('1', 'black', attrs=attrs_val))
            elif char_val == '.': row_display.append(colored('.', 'blue', attrs=attrs_val))
            elif char_val == 'X': row_display.append(colored('X', 'green', attrs=attrs_val))
            elif char_val == 'C': row_display.append(colored('C', 'red', attrs=attrs_val))
            elif char_val == 'Z': row_display.append(colored('1', 'cyan', attrs=attrs_val))
            else: row_display.append(colored(str(char_val), attrs=attrs_val))
        print(" ".join(row_display))

    if line_len > 0: print("-" * line_len)


def print_grid_animated(grid_to_display, path_taken, step_num, total_ones_initial,
                        current_best_len, best_solution_path_for_display, # New argument
                        search_phase_msg="", focus_action_coord=None):
    global initial_grid_for_display # Access the global initial grid
    os.system('cls' if os.name == 'nt' else 'clear')
    path_len = len(path_taken)
    header_message = f"{search_phase_msg} (Step: {step_num}, PathLen: {path_len})\n"
    highlight_move_placement = focus_action_coord

    if not highlight_move_placement and path_taken:
        _, lm_r, lm_c = path_taken[-1]
        highlight_move_placement = (lm_r, lm_c)

    current_ones = count_ones(grid_to_display)
    header_message += f"   Active '1's: {current_ones} (Initial: {total_ones_initial})\n"
    best_known_str = str(current_best_len) if current_best_len != float('inf') else 'None yet'
    header_message += f"   Best solution length: {best_known_str}"

    # Print the current exploration grid
    _print_grid_core(grid_to_display, highlight_pos=highlight_move_placement, message=header_message)

    # Print the current best solution grid below, if it exists
    if best_solution_path_for_display and initial_grid_for_display:
        best_grid_reconstructed = copy.deepcopy(initial_grid_for_display)
        for move_type, r_move, c_move in best_solution_path_for_display:
           if move_type == 'X': best_grid_reconstructed, _ = apply_cross(best_grid_reconstructed, r_move, c_move)
           elif move_type == 'C': best_grid_reconstructed, _ = apply_circle(best_grid_reconstructed, r_move, c_move)

        best_solution_message = f"\n--- Current Best Solution ({len(best_solution_path_for_display)} moves) ---"
        # Determine max_cols for the best solution path display to avoid excessive wrapping
        # Can be based on initial_grid_for_display's width or a fixed reasonable number
        max_cols_for_best_display = len(initial_grid_for_display[0]) if initial_grid_for_display else 20
        _print_grid_core(best_grid_reconstructed, message=best_solution_message, max_cols=max_cols_for_best_display)

    # time.sleep(0.01) # Your faster speed

def count_ones(grid): return sum(row.count('1') for row in grid)
def is_solved(grid): return count_ones(grid) == 0

_apply_move_id_counter = 0
def get_next_apply_move_id():
    global _apply_move_id_counter
    _apply_move_id_counter +=1
    return _apply_move_id_counter

def apply_move_to_grid(grid_state, move_info):
    move_type, r, c = move_info
    if move_type == 'X':
        return apply_cross(grid_state, r, c)
    # Elif not needed if only two options and 'X' is first
    else: # 'C'
        return apply_circle(grid_state, r, c)

def apply_cross(grid_state, r, c):
    new_grid = copy.deepcopy(grid_state)
    rows, cols = len(new_grid), len(new_grid[0])
    if not (0 <= r < rows and 0 <= c < cols and new_grid[r][c] == '1'): return new_grid, 0
    new_grid[r][c] = 'X'; cleared_count = 0
    for ci in range(c + 1, cols):
        if new_grid[r][ci] == '1': new_grid[r][ci] = 'Z'; cleared_count += 1
        elif new_grid[r][ci] in ('.', 'X', 'C', 'Z'): break; # Semicolon was here
        else: break
    for ci in range(c - 1, -1, -1):
        if new_grid[r][ci] == '1': new_grid[r][ci] = 'Z'; cleared_count += 1
        elif new_grid[r][ci] in ('.', 'X', 'C', 'Z'): break; # Semicolon was here
        else: break
    for ri in range(r + 1, rows):
        if new_grid[ri][c] == '1': new_grid[ri][c] = 'Z'; cleared_count += 1
        elif new_grid[ri][c] in ('.', 'X', 'C', 'Z'): break; # Semicolon was here
        else: break
    for ri in range(r - 1, -1, -1):
        if new_grid[ri][c] == '1': new_grid[ri][c] = 'Z'; cleared_count += 1
        elif new_grid[ri][c] in ('.', 'X', 'C', 'Z'): break; # Semicolon was here
        else: break
    return new_grid, cleared_count

def apply_circle(grid_state, r, c):
    new_grid = copy.deepcopy(grid_state)
    rows, cols = len(new_grid), len(new_grid[0])
    if not (0 <= r < rows and 0 <= c < cols and new_grid[r][c] == '1'): return new_grid, 0
    new_grid[r][c] = 'C'; cleared_count = 0
    for dr_offset in [-1, 0, 1]:
        for dc_offset in [-1, 0, 1]:
            if dr_offset == 0 and dc_offset == 0: continue
            nr, nc = r + dr_offset, c + dc_offset
            if 0 <= nr < rows and 0 <= nc < cols and new_grid[nr][nc] == '1':
                new_grid[nr][nc] = 'Z'; cleared_count += 1
    return new_grid, cleared_count

def get_sorted_potential_actions(grid_state):
    rows, cols = len(grid_state), len(grid_state[0])
    possible_actions = []
    for r_idx in range(rows):
        for c_idx in range(cols):
            if grid_state[r_idx][c_idx] == '1':
                for move_type_char in ['X', 'C']:
                    _, zs_cleared = apply_move_to_grid(grid_state, (move_type_char, r_idx, c_idx))
                    heuristic_val = -(zs_cleared + 1)
                    move_details = (move_type_char, r_idx, c_idx)
                    possible_actions.append({
                        "heuristic": heuristic_val,
                        "id": get_next_apply_move_id(),
                        "move_info": move_details,
                    })
    possible_actions.sort(key=lambda x: (x["heuristic"], x["id"]))
    return possible_actions

def add_node_alternatives_to_frontier(tree_node, frontier_pq):
    global min_moves_overall
    if not tree_node.potential_actions:
        tree_node.potential_actions = get_sorted_potential_actions(tree_node.grid_state)

    for alt_idx in range(tree_node.next_alternative_action_idx_to_explore, len(tree_node.potential_actions)):
        if tree_node.depth + 1 < min_moves_overall:
            action = tree_node.potential_actions[alt_idx]
            heapq.heappush(frontier_pq,
                           (tree_node.depth, action["heuristic"], tree_node.id, tree_node, alt_idx)
                          )
        else:
            break

def rollout_path_from_node(start_node, initial_ones_count, frontier_pq, phase_msg_prefix="Rollout"):
    global min_moves_overall, best_path_overall, visited_states, _exploration_step_counter

    current_treenode = start_node
    new_best_found_in_rollout = False

    while not current_treenode.is_solved_state and current_treenode.depth < min_moves_overall:
        _exploration_step_counter[0] += 1
        print_grid_animated(current_treenode.grid_state, current_treenode.path_from_root,
                            _exploration_step_counter[0], initial_ones_count, min_moves_overall,
                            best_path_overall, # Pass current best path
                            f"{phase_msg_prefix} from D{start_node.depth}, N{start_node.id}")

        if not current_treenode.potential_actions:
            current_treenode.potential_actions = get_sorted_potential_actions(current_treenode.grid_state)

        if not current_treenode.potential_actions:
            break

        greedy_action_details = current_treenode.potential_actions[0]
        move = greedy_action_details["move_info"]

        next_grid_state, _ = apply_move_to_grid(current_treenode.grid_state, move)
        next_depth = current_treenode.depth + 1
        next_path = current_treenode.path_from_root + [move]
        next_grid_string = grid_to_string(next_grid_state)

        if next_grid_string in visited_states and visited_states[next_grid_string] <= next_depth:
            break

        child_node = TreeNode(next_grid_state, next_path, next_depth, current_treenode)
        current_treenode.children.append((move, child_node))
        visited_states[next_grid_string] = next_depth
        add_node_alternatives_to_frontier(child_node, frontier_pq)
        current_treenode = child_node

    _exploration_step_counter[0] += 1
    print_grid_animated(current_treenode.grid_state, current_treenode.path_from_root,
                        _exploration_step_counter[0], initial_ones_count, min_moves_overall,
                        best_path_overall, # Pass current best path
                        f"{phase_msg_prefix} End D{current_treenode.depth}")

    if current_treenode.is_solved_state and current_treenode.depth < min_moves_overall:
        min_moves_overall = current_treenode.depth
        best_path_overall = list(current_treenode.path_from_root)
        new_best_found_in_rollout = True
        print(f"\n*** New Best Solution: {min_moves_overall} moves! (During {phase_msg_prefix}) ***")
        # Display the new best solution that was just found
        print_grid_animated(current_treenode.grid_state, best_path_overall,
                            _exploration_step_counter[0], initial_ones_count, min_moves_overall,
                            best_path_overall, # Pass it to itself for display
                            f"NEW BEST FOUND!")
        time.sleep(0.5) # Increased pause to see new best

    return current_treenode, new_best_found_in_rollout

def solve_puzzle_explicit_tree_exploration(initial_grid_param, initial_ones_count_param):
    global min_moves_overall, best_path_overall, visited_states, _exploration_step_counter, _node_id_counter, initial_grid_for_display

    min_moves_overall = float('inf')
    best_path_overall = None
    visited_states = {}
    _exploration_step_counter = [0]
    _node_id_counter = 0
    initial_grid_for_display = copy.deepcopy(initial_grid_param) # Store for displaying best solution

    if initial_ones_count_param == 0:
        print("Puzzle is already solved.")
        return []

    root_node = TreeNode(initial_grid_param, [], 0)
    visited_states[root_node.grid_string] = 0
    exploration_frontier_pq = []

    print("\n--- Phase 1: Initial Greedy Rollout ---")
    add_node_alternatives_to_frontier(root_node, exploration_frontier_pq)
    _, initial_best_found = rollout_path_from_node(root_node, initial_ones_count_param,
                                                   exploration_frontier_pq, "Initial Greedy Rollout")
    if best_path_overall:
        print(f"Initial best solution: {min_moves_overall} moves.")
    else:
        print("Initial greedy rollout did not find a solution (or it was pruned).")
        if not exploration_frontier_pq :
             print("No moves possible from initial state or very shallow depth limit hit.")
             return None

    print("\n--- Phase 2: Exploring Alternative Branches ---")
    processed_branch_choices = set()

    while exploration_frontier_pq:
        try:
            _, _, _, parent_node_for_branching, alt_action_idx = heapq.heappop(exploration_frontier_pq)
        except IndexError: break

        branch_choice_key = (parent_node_for_branching.id, alt_action_idx)
        if branch_choice_key in processed_branch_choices:
            continue
        processed_branch_choices.add(branch_choice_key)

        parent_node_for_branching.next_alternative_action_idx_to_explore = alt_action_idx + 1

        if not parent_node_for_branching.potential_actions:
            parent_node_for_branching.potential_actions = get_sorted_potential_actions(parent_node_for_branching.grid_state)

        if alt_action_idx >= len(parent_node_for_branching.potential_actions):
            continue

        if parent_node_for_branching.depth + 1 >= min_moves_overall:
            continue

        action_to_take_details = parent_node_for_branching.potential_actions[alt_action_idx]
        move = action_to_take_details["move_info"]

        _exploration_step_counter[0] += 1
        phase_msg = (f"Branching from N{parent_node_for_branching.id} D{parent_node_for_branching.depth}, "
                     f"AltIdx {alt_action_idx} (H:{action_to_take_details['heuristic']})")
        print_grid_animated(parent_node_for_branching.grid_state, parent_node_for_branching.path_from_root,
                            _exploration_step_counter[0], initial_ones_count_param, min_moves_overall,
                            best_path_overall, # Pass current best path
                            phase_msg, focus_action_coord=(move[1], move[2]))
        time.sleep(0.01)

        next_grid_state, _ = apply_move_to_grid(parent_node_for_branching.grid_state, move)
        next_depth = parent_node_for_branching.depth + 1
        next_path = parent_node_for_branching.path_from_root + [move]
        next_grid_string = grid_to_string(next_grid_state)

        if next_grid_string in visited_states and visited_states[next_grid_string] <= next_depth:
            continue

        branch_child_node = TreeNode(next_grid_state, next_path, next_depth, parent_node_for_branching)
        parent_node_for_branching.children.append((move, branch_child_node))
        visited_states[next_grid_string] = next_depth
        add_node_alternatives_to_frontier(branch_child_node, exploration_frontier_pq)

        rollout_path_from_node(branch_child_node, initial_ones_count_param,
                               exploration_frontier_pq, f"Rollout after Branch N{parent_node_for_branching.id}-A{alt_action_idx}")

        if parent_node_for_branching.next_alternative_action_idx_to_explore < len(parent_node_for_branching.potential_actions):
             add_node_alternatives_to_frontier(parent_node_for_branching, exploration_frontier_pq)

    print(f"\n--- Search Complete. Total exploration steps: {_exploration_step_counter[0]} ---")
    return best_path_overall

if __name__ == "__main__":
    puzzle_file = "puzzl.txt"
    if not os.path.exists(puzzle_file):
        print(f"'{puzzle_file}' not found. Creating default.")
        default_puzzle_content = (
            ".11111.1111111\n.11.11111.1111\n.111111.1.11.1\n1.1111111.111.\n"
            "11.11.1.1..111\n..111.11111111\n.111..1...111.\n.111111111.111\n.1.11111.11111\n"
        )
        with open(puzzle_file, "w") as f: f.write(default_puzzle_content)

    initial_grid_data_raw = parse_puzzle(puzzle_file)

    if initial_grid_data_raw:
        initial_grid_data_main = copy.deepcopy(initial_grid_data_raw) # Use this for the solver
        for r in range(len(initial_grid_data_main)):
            for c in range(len(initial_grid_data_main[r])):
                if initial_grid_data_main[r][c] == '0': initial_grid_data_main[r][c] = '.'

        os.system('cls' if os.name == 'nt' else 'clear')
        _print_grid_core(initial_grid_data_main, message="Initial Puzzle State:") # Use the main copy
        initial_ones_count_main = count_ones(initial_grid_data_main) # Use the main copy
        print(f"Total active '1's to clear: {initial_ones_count_main}")
        print("\n--- Color Legend & Info ---")
        print(f"{colored('1', 'black')}:Active {colored('1', 'cyan')}:Cancelled {colored('.', 'blue')}:Empty "
              f"{colored('X', 'green')}:Cross {colored('C', 'red')}:Circle")
        print("Highlight shows action placement. Search explores alternatives from tree nodes.")
        print("----------------------\n")
        print("Starting Explicit Tree Exploration Solver...")
        print("Press Ctrl+C to interrupt.")
        # time.sleep(3) # Removed for faster start with your speedup

        final_solution_path = None
        try:
            if initial_ones_count_main == 0:
                 final_solution_path = []
            else:
                # Pass the main copy of initial_grid to the solver
                final_solution_path = solve_puzzle_explicit_tree_exploration(initial_grid_data_main, initial_ones_count_main)
        except KeyboardInterrupt:
            print("\n\nSearch interrupted by user.")
        except Exception as e:
            print(f"\n\nAn error occurred: {e}")
            import traceback
            traceback.print_exc()

        if final_solution_path is not None:
            print(f"\n--- Optimal Solution Found ---")
            if not final_solution_path and initial_ones_count_main > 0:
                 print("Error: No path found but puzzle was not initially solved.")
            elif not final_solution_path and initial_ones_count_main == 0:
                 # Use main copy for final display too
                 _print_grid_core(initial_grid_data_main, message="Final Solved State (0 moves - initially solved):")
                 print("\nOptimal solution length: 0 moves (already solved).")
            else:
                # Use main copy for reconstructing final grid
                final_grid_reconstructed = copy.deepcopy(initial_grid_data_main)
                for move_type, r_move, c_move in final_solution_path:
                   if move_type == 'X': final_grid_reconstructed, _ = apply_cross(final_grid_reconstructed, r_move, c_move)
                   elif move_type == 'C': final_grid_reconstructed, _ = apply_circle(final_grid_reconstructed, r_move, c_move)

                _print_grid_core(final_grid_reconstructed,
                                 message=f"Final Solved State (Optimal: {len(final_solution_path)} moves):")

                print(f"\nOptimal solution length: {len(final_solution_path)} moves.")
                print("Sequence of moves (Type, Row, Column):")
                for i, move in enumerate(final_solution_path):
                    print(f"  {i+1}. {move[0]} at ({move[1]}, {move[2]})")
        else:
            print("\nSolver finished: No solution found or search was interrupted/error.")
