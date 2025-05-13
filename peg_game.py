import os
import time
import heapq
import copy
import random # For randomized branching
import argparse # For command-line arguments

try:
    from termcolor import colored
except ImportError:
    print("The 'termcolor' package is not installed. Please install it: pip install termcolor")
    print("Displaying without colors.")
    def colored(text, color=None, on_color=None, attrs=None): return str(text)

# --- Global State ---
min_moves_overall = float('inf')
best_path_overall = None
initial_grid_for_display = None # For reconstructing best solution display
visited_states = {} # {grid_string: min_depth_reached}
_exploration_step_counter = [0]
_node_id_counter = 0

def get_next_node_id():
    global _node_id_counter
    _node_id_counter += 1
    return _node_id_counter

# --- TreeNode Class ---
class TreeNode:
    def __init__(self, grid, path, depth, parent=None):
        self.id = get_next_node_id()
        self.grid = grid
        self.grid_str = grid_to_string(grid)
        self.path = path # List of move_info tuples
        self.depth = depth
        self.parent = parent
        self.children = [] # List of tuples: (move_info_taken, child_treenode_object)
        self.potential_actions = [] # Lazily computed: list of {"h_val", "id", "move"}
        self.is_solved = is_solved(grid)
        self.next_alt_idx = 1 # Next non-greedy action to try (0 is greedy)

    def __lt__(self, other):
        return self.id < other.id

# --- Puzzle Parsing and Grid Operations ---
def parse_puzzle(filename="puzzl.txt"):
    # ... (same as your version)
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

def count_ones(grid): return sum(row.count('1') for row in grid)
def is_solved(grid): return count_ones(grid) == 0

# --- Move Application ---
_apply_move_id_counter = 0
def get_next_apply_move_id():
    global _apply_move_id_counter
    _apply_move_id_counter +=1
    return _apply_move_id_counter

def apply_move_to_grid(grid, move): # move is (type, r, c)
    move_type, r, c = move
    if move_type == 'X':
        return apply_cross(grid, r, c)
    else: # 'C'
        return apply_circle(grid, r, c)

def apply_cross(grid, r, c):
    # ... (same as your version, ensure no trailing semicolons in break)
    new_grid = copy.deepcopy(grid)
    rows, cols = len(new_grid), len(new_grid[0])
    if not (0 <= r < rows and 0 <= c < cols and new_grid[r][c] == '1'): return new_grid, 0
    new_grid[r][c] = 'X'; cleared_count = 0
    for ci in range(c + 1, cols):
        if new_grid[r][ci] == '1': new_grid[r][ci] = 'Z'; cleared_count += 1
        elif new_grid[r][ci] in ('.', 'X', 'C', 'Z'): break
        else: break
    for ci in range(c - 1, -1, -1):
        if new_grid[r][ci] == '1': new_grid[r][ci] = 'Z'; cleared_count += 1
        elif new_grid[r][ci] in ('.', 'X', 'C', 'Z'): break
        else: break
    for ri in range(r + 1, rows):
        if new_grid[ri][c] == '1': new_grid[ri][c] = 'Z'; cleared_count += 1
        elif new_grid[ri][c] in ('.', 'X', 'C', 'Z'): break
        else: break
    for ri in range(r - 1, -1, -1):
        if new_grid[ri][c] == '1': new_grid[ri][c] = 'Z'; cleared_count += 1
        elif new_grid[ri][c] in ('.', 'X', 'C', 'Z'): break
        else: break
    return new_grid, cleared_count


def apply_circle(grid, r, c):
    # ... (same as your version)
    new_grid = copy.deepcopy(grid)
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


def get_sorted_actions(grid):
    # Returns list of dicts: {"h_val", "id", "move"}
    rows, cols = len(grid), len(grid[0])
    actions = []
    for r_idx in range(rows):
        for c_idx in range(cols):
            if grid[r_idx][c_idx] == '1':
                for move_type in ['X', 'C']:
                    _, zs_cleared = apply_move_to_grid(grid, (move_type, r_idx, c_idx))
                    h_val = -(zs_cleared + 1) # Heuristic: negative of (cleared + 1 for piece)
                    move_details = (move_type, r_idx, c_idx)
                    actions.append({
                        "h_val": h_val, "id": get_next_apply_move_id(), "move": move_details,
                    })
    actions.sort(key=lambda x: (x["h_val"], x["id"]))
    return actions

# --- Animation ---
def _print_grid_core(grid, highlight_pos=None, message=None): # Simplified args
    if message: print(message)
    rows, cols = len(grid), len(grid[0]) if grid else 0
    if not rows: return
    line_len = (2 * cols - 1)
    print("-" * line_len)
    for r_idx, row_data in enumerate(grid):
        row_display = []
        for c_idx, char_val in enumerate(row_data):
            attrs = ['bold', 'underline'] if highlight_pos and r_idx == highlight_pos[0] and c_idx == highlight_pos[1] else []
            if char_val == '1': row_display.append(colored('1', 'black', attrs=attrs))
            elif char_val == '.': row_display.append(colored('.', 'blue', attrs=attrs))
            elif char_val == 'X': row_display.append(colored('X', 'green', attrs=attrs))
            elif char_val == 'C': row_display.append(colored('C', 'red', attrs=attrs))
            elif char_val == 'Z': row_display.append(colored('1', 'cyan', attrs=attrs))
            else: row_display.append(colored(str(char_val), attrs=attrs))
        print(" ".join(row_display))
    print("-" * line_len)

def print_animated_display(current_grid, current_path, step, total_initial_ones,
                           best_len_so_far, current_best_path, # Added current_best_path
                           phase_msg, focus_coord=None):
    global initial_grid_for_display
    os.system('cls' if os.name == 'nt' else 'clear')

    path_len = len(current_path)
    header = f"{phase_msg} (Step: {step}, PathLen: {path_len})\n"
    highlight_pos = focus_coord
    if not highlight_pos and current_path:
        _, r, c = current_path[-1]
        highlight_pos = (r, c)

    ones_now = count_ones(current_grid)
    header += f"   Active '1's: {ones_now} (Initial: {total_initial_ones})\n"
    best_len_str = str(best_len_so_far) if best_len_so_far != float('inf') else 'None'
    header += f"   Best Solution Length: {best_len_str}"

    _print_grid_core(current_grid, highlight_pos, header)

    if current_best_path and initial_grid_for_display:
        best_grid = copy.deepcopy(initial_grid_for_display)
        for move_type, r, c in current_best_path:
           best_grid, _ = apply_move_to_grid(best_grid, (move_type, r, c))
        _print_grid_core(best_grid, message=f"\n--- Current Best Solution ({len(current_best_path)} moves) ---")
    # time.sleep(0.01) # Your speed

# --- Core Search Logic ---
def add_node_alts_to_frontier_pq(node, frontier_pq):
    global min_moves_overall
    if not node.potential_actions:
        node.potential_actions = get_sorted_actions(node.grid)

    for alt_idx in range(node.next_alt_idx, len(node.potential_actions)):
        if node.depth + 1 < min_moves_overall:
            action = node.potential_actions[alt_idx]
            # PQ: (parent_depth, alt_heuristic, parent_node_id, parent_node_obj, alt_idx)
            heapq.heappush(frontier_pq, (node.depth, action["h_val"], node.id, node, alt_idx))
        else:
            break

def rollout_from_node(start_node, total_initial_ones, frontier_pq_or_none, randomized_mode, phase_prefix):
    global min_moves_overall, best_path_overall, visited_states, _exploration_step_counter

    current_node = start_node
    new_best_found = False

    while not current_node.is_solved and current_node.depth < min_moves_overall:
        _exploration_step_counter[0] += 1
        print_animated_display(current_node.grid, current_node.path, _exploration_step_counter[0],
                               total_initial_ones, min_moves_overall, best_path_overall,
                               f"{phase_prefix} from D{start_node.depth} N{start_node.id}")

        if not current_node.potential_actions:
            current_node.potential_actions = get_sorted_actions(current_node.grid)
        if not current_node.potential_actions: break # Dead end

        greedy_action = current_node.potential_actions[0]
        move = greedy_action["move"]

        next_grid, _ = apply_move_to_grid(current_node.grid, move)
        next_depth = current_node.depth + 1
        next_path = current_node.path + [move]
        next_grid_str = grid_to_string(next_grid)

        if next_grid_str in visited_states and visited_states[next_grid_str] <= next_depth:
            break # Cycle or worse path

        child = TreeNode(next_grid, next_path, next_depth, current_node)
        current_node.children.append((move, child))
        visited_states[next_grid_str] = next_depth

        if not randomized_mode and frontier_pq_or_none:
            add_node_alts_to_frontier_pq(child, frontier_pq_or_none)

        current_node = child

    _exploration_step_counter[0] += 1 # Final state of rollout
    print_animated_display(current_node.grid, current_node.path, _exploration_step_counter[0],
                           total_initial_ones, min_moves_overall, best_path_overall,
                           f"{phase_prefix} End D{current_node.depth}")

    if current_node.is_solved and current_node.depth < min_moves_overall:
        min_moves_overall = current_node.depth
        best_path_overall = list(current_node.path)
        new_best_found = True
        print(f"\n*** New Best Solution: {min_moves_overall} moves! (During {phase_prefix}) ***")
        print_animated_display(current_node.grid, best_path_overall, _exploration_step_counter[0],
                               total_initial_ones, min_moves_overall, best_path_overall, "NEW BEST!")
        time.sleep(0.5)
    return current_node, new_best_found

def solve_puzzle(initial_grid, initial_ones_count, randomized_branching):
    global min_moves_overall, best_path_overall, visited_states, _exploration_step_counter, _node_id_counter, initial_grid_for_display

    min_moves_overall = float('inf')
    best_path_overall = None
    visited_states = {}
    _exploration_step_counter = [0]
    _node_id_counter = 0
    initial_grid_for_display = copy.deepcopy(initial_grid)

    if initial_ones_count == 0: return []

    root = TreeNode(initial_grid, [], 0)
    visited_states[root.grid_str] = 0

    branch_frontier_pq = [] # Used only if not randomized_branching

    print("\n--- Phase 1: Initial Greedy Rollout ---")
    if not randomized_branching:
        add_node_alts_to_frontier_pq(root, branch_frontier_pq)
    rollout_from_node(root, initial_ones_count, branch_frontier_pq, randomized_branching, "Greedy Rollout")

    if best_path_overall: print(f"Initial best solution: {min_moves_overall} moves.")
    else: print("Initial greedy rollout failed or was pruned.")

    # --- Phase 2: Exploring Alternative Branches ---
    if randomized_branching:
        print("\n--- Phase 2: Exploring Alternative Branches (Randomized) ---")
        while True:
            candidate_parent_nodes = []
            # BFS to find all nodes in tree that can branch
            q = [root]
            scanned_ids = {root.id}
            while q:
                node_to_scan = q.pop(0)
                if not node_to_scan.is_solved and node_to_scan.depth + 1 < min_moves_overall:
                    if not node_to_scan.potential_actions:
                        node_to_scan.potential_actions = get_sorted_actions(node_to_scan.grid)
                    if node_to_scan.next_alt_idx < len(node_to_scan.potential_actions):
                        candidate_parent_nodes.append(node_to_scan)
                for _, child_node in node_to_scan.children:
                    if child_node.id not in scanned_ids:
                        scanned_ids.add(child_node.id)
                        q.append(child_node)

            if not candidate_parent_nodes:
                print("No more candidate branching points for randomized mode.")
                break

            branch_parent = random.choice(candidate_parent_nodes)
            alt_idx = branch_parent.next_alt_idx
            branch_parent.next_alt_idx += 1 # Consume this alternative index

            action_details = branch_parent.potential_actions[alt_idx]
            move = action_details["move"]

            _exploration_step_counter[0] += 1
            phase_msg = f"Random Branch: N{branch_parent.id} D{branch_parent.depth}, AltIdx {alt_idx} (H:{action_details['h_val']})"
            print_animated_display(branch_parent.grid, branch_parent.path, _exploration_step_counter[0],
                                   initial_ones_count, min_moves_overall, best_path_overall,
                                   phase_msg, focus_coord=(move[1], move[2]))
            time.sleep(0.01)

            next_grid, _ = apply_move_to_grid(branch_parent.grid, move)
            next_depth = branch_parent.depth + 1
            next_path = branch_parent.path + [move]
            next_grid_str = grid_to_string(next_grid)

            if next_grid_str in visited_states and visited_states[next_grid_str] <= next_depth:
                continue

            branch_child = TreeNode(next_grid, next_path, next_depth, branch_parent)
            branch_parent.children.append((move, branch_child))
            visited_states[next_grid_str] = next_depth

            rollout_from_node(branch_child, initial_ones_count, None, True,
                              f"Rollout Post-RandomBranch N{branch_parent.id}-A{alt_idx}")
    else: # Default: Priority Queue based exploration
        print("\n--- Phase 2: Exploring Alternative Branches (Priority Queue) ---")
        processed_pq_choices = set()
        while branch_frontier_pq:
            try:
                parent_depth, alt_h_val, parent_id, branch_parent, alt_idx = heapq.heappop(branch_frontier_pq)
            except IndexError: break

            pq_choice_key = (branch_parent.id, alt_idx)
            if pq_choice_key in processed_pq_choices: continue
            processed_pq_choices.add(pq_choice_key)

            branch_parent.next_alt_idx = alt_idx + 1

            if not branch_parent.potential_actions:
                branch_parent.potential_actions = get_sorted_actions(branch_parent.grid)
            if alt_idx >= len(branch_parent.potential_actions): continue
            if branch_parent.depth + 1 >= min_moves_overall: continue

            action_details = branch_parent.potential_actions[alt_idx]
            move = action_details["move"]

            _exploration_step_counter[0] += 1
            phase_msg = f"PQ Branch: N{branch_parent.id} D{branch_parent.depth}, AltIdx {alt_idx} (H:{action_details['h_val']})"
            print_animated_display(branch_parent.grid, branch_parent.path, _exploration_step_counter[0],
                                   initial_ones_count, min_moves_overall, best_path_overall,
                                   phase_msg, focus_coord=(move[1], move[2]))
            time.sleep(0.01)

            next_grid, _ = apply_move_to_grid(branch_parent.grid, move)
            next_depth = branch_parent.depth + 1
            next_path = branch_parent.path + [move]
            next_grid_str = grid_to_string(next_grid)

            if next_grid_str in visited_states and visited_states[next_grid_str] <= next_depth:
                continue

            branch_child = TreeNode(next_grid, next_path, next_depth, branch_parent)
            branch_parent.children.append((move, branch_child))
            visited_states[next_grid_str] = next_depth

            add_node_alts_to_frontier_pq(branch_child, branch_frontier_pq) # Add alts from new child
            rollout_from_node(branch_child, initial_ones_count, branch_frontier_pq, False,
                              f"Rollout Post-PQBranch N{branch_parent.id}-A{alt_idx}")

            # If branch_parent itself has more alts, re-add it.
            if branch_parent.next_alt_idx < len(branch_parent.potential_actions):
                add_node_alts_to_frontier_pq(branch_parent, branch_frontier_pq)


    print(f"\n--- Search Complete. Total exploration steps: {_exploration_step_counter[0]} ---")
    return best_path_overall

# --- Main Execution ---
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Solve the grid puzzle with X's and C's.")
    parser.add_argument("-r", "--randomized", action="store_true",
                        help="Use randomized branching strategy for exploring alternatives.")
    args = parser.parse_args()

    puzzle_file = "puzzl.txt"
    if not os.path.exists(puzzle_file):
        print(f"'{puzzle_file}' not found. Creating default.")
        default_content = (
            ".11111.1111111\n.11.11111.1111\n.111111.1.11.1\n1.1111111.111.\n"
            "11.11.1.1..111\n..111.11111111\n.111..1...111.\n.111111111.111\n.1.11111.11111\n"
        )
        with open(puzzle_file, "w") as f: f.write(default_content)

    initial_grid_main = parse_puzzle(puzzle_file)

    if initial_grid_main:
        for r in range(len(initial_grid_main)): # Ensure '.' for empty
            for c in range(len(initial_grid_main[r])):
                if initial_grid_main[r][c] == '0': initial_grid_main[r][c] = '.'

        os.system('cls' if os.name == 'nt' else 'clear')
        _print_grid_core(initial_grid_main, message="Initial Puzzle State:")
        initial_ones = count_ones(initial_grid_main)
        print(f"Total active '1's to clear: {initial_ones}")
        print("\n--- Color Legend & Info ---")
        print(f"{colored('1', 'black')}:Active {colored('1', 'cyan')}:Cancelled {colored('.', 'blue')}:Empty "
              f"{colored('X', 'green')}:Cross {colored('C', 'red')}:Circle")
        strategy = "Randomized Branching" if args.randomized else "Priority Queue Branching"
        print(f"Strategy: {strategy}")
        print("----------------------\n")
        print("Starting Solver...")
        print("Press Ctrl+C to interrupt.")
        # time.sleep(1) # Reduced for your faster speed

        final_path = None
        try:
            if initial_ones == 0: final_path = []
            else: final_path = solve_puzzle(initial_grid_main, initial_ones, args.randomized)
        except KeyboardInterrupt: print("\n\nSearch interrupted by user.")
        except Exception as e:
            print(f"\n\nAn error occurred: {e}")
            import traceback
            traceback.print_exc()

        if final_path is not None:
            print(f"\n--- Optimal Solution Found ---")
            if not final_path and initial_ones > 0: print("Error: No path found.")
            elif not final_path and initial_ones == 0:
                 _print_grid_core(initial_grid_main, message="Final Solved State (0 moves):")
                 print("\nOptimal solution length: 0 moves (already solved).")
            else:
                final_reconstructed_grid = copy.deepcopy(initial_grid_main)
                for move_t, r_m, c_m in final_path:
                   final_reconstructed_grid, _ = apply_move_to_grid(final_reconstructed_grid, (move_t, r_m, c_m))
                _print_grid_core(final_reconstructed_grid, message=f"Final Solved State (Optimal: {len(final_path)} moves):")
                print(f"\nOptimal solution length: {len(final_path)} moves.")
                print("Sequence of moves (Type, Row, Column):")
                for i, move_item in enumerate(final_path):
                    print(f"  {i+1}. {move_item[0]} at ({move_item[1]}, {move_item[2]})")
        else:
            print("\nSolver finished: No solution, interrupted, or error.")
