# peg_game

Vibe-coded solver for Brian Lustig's [Peg game](https://lustigb.github.io/ijpeggame/)

Does tree expansion based on an *inadmissible* heuristic: the number of circles filled.
Tree is expanded based on exploring nodes (on the full tree - not backtracking) that has the minimal increase in heuristic, with a bias toward shallow nodes.
When a new branch is added, tree is expanded to the end of the game or until more moves have been played than the current best series of moves.
