# Mastermind
💡 Mastermind solver

## Usage
Install dependencies with `cabal install`. Then, run with `cabal run` and it will give a guess and prompt you for the score. The guess uses the numbers `[1..6]` to represent the six colors. By symmetry, it does not matter which color corresponds to which number as long as you are consistent.

## Example game
The code to guess is `[3,2,1,5]`.
```bash
$ cabal run
Preprocessing executable 'mastermind' for mastermind-0.1.0.0...
Running mastermind...
[1,1,2,2]
Correct: 0
Incorrect: 2
[2,3,4,4]
Correct: 0
Incorrect: 2
[3,2,1,5]
```
