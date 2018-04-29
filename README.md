# Mastermind
💡 Mastermind solver

## Usage
```bash
$ git clone https://github.com/nicolaschan/mastermind.git && cd mastermind
$ runhaskell Setup configure --user
$ runhaskell Setup build
$ sudo runhaskell Setup install
$ mastermind
```

It will give a guess and prompt you for the score. The guess uses the numbers `[1..6]` to represent the six colors. By symmetry, it does not matter which color corresponds to which number as long as you are consistent.

## Example game
The code to guess is `[3,2,1,5]`.
```hs
$ mastermind
[1,1,2,2]
Correct: 0
Incorrect: 2
[2,3,4,4]
Correct: 0
Incorrect: 2
[3,2,1,5]
```
