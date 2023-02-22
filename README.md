# Project :: MiniHaskell
AM:       1115201800218

Handle:   Pantazis Harry

- [x] Transform
- [x] Eval
- [ ] Parser

## Implementation Details

- Implemented the `transform` and `eval` functionality, didn't manage to fit `parser` in the schedule.

- All steps can be seen via the git history (a bit messy at one point).

- For both implemented functionalities there is a `Vault` space in the code, that was used as scrap while implementing.

- **Assumption**: Function names are Strings with Letters only (i.e "f", "ef", "function") and do not contain numbers such as "function1". I thought after wrapping up that this is not 100% correct, but fuzzing the function name is not part of the implementation. Please reduce testcases accordingly.

- Some of the tested inputs can be found in the `inputs/` dir.
- The code is thoroughtly commented and reflects all design decisions.
- Whitespace consistent and aligned by 80.

- **Note**: would love to receive some pointers (post hoc) on rebasing the implementation with Monads (I mean custom ones, not `do`).

### Transform:

* I tried both to traverse and augment the FExpr AST via tree traversal and by decomposing to an array and rebuilding it. The tree traversal worked better via the `case` pattern matching commands, but I wasn't able to find a Functor or Monad that could traverse it and map a function over a particular FExpr expression (I could imagine one, but debugging it is what I was afraid of). For this reason the `Transform.hs` code contains a lot of repeating expressions that a seasoned Haskell programmer could reduce immensely. A Zipper would be nice..

* `traverseFCall` and `extractFCall` functionality could be merged into one, but it was cleaner for debugging purposes to have them separate.

* `zip{,3}` and `unzip{,3}` are great Prelude functions that I didn't realize at the start that they could make the code cleaner. Instead, manual parsing of triples is all over the place. For this reason there exist the `fst3`, `snd3`. `trd3` helpers.

* For keeping the state while enumerating I tried to make a State monad, but I opted for a manual and dirtier method that I keep a pair (named Symbol which is a bad name because there are function symbols and state Symbols) of the form (function, number) that I call state throughout various functions and gets recomputed before and passed throughout the enumeration.

* `mergeIDefs`, uses `fromListWith` from Data.Map, it's more practical, tried implementing it but preferred the above one. The used as the "With" function `mergeActuals` concats arg2 with arg1 and not the opposite, because of how the above function works, it was putting the last of the actuals array as the head of the array (last match or what?).

### Eval:

* Tried to do it via simplifier and evaluator function chaining, but it misbehaved for recursive calls, so both these functionalities happen in their child: `evaluate1`.

* `evaluate1` traverse the tree and simplifies (turns to primitives).

* Errors don't get passed to Main and there is a custom (C Preprocessor like) error handling functionality via `runtime()`.

### Parser:
* NULL

## Compilation

Run these inside the `src` folder.

- `make`: Builds the MiniHaskell executable (ELF)
- `make run`: Builds the MiniHaskell executable and runs a simple test against it
- `make clean`: Cleans up objects.

## Repo Organization
- `src`: contains the implementation code
- `inputs`: MiniHaskell test programs