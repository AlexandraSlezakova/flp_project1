# Functional Project FLP 2020/2021

## DKA-2-MKA
Author:  **Alexandra Slezáková (xsleza20)**

## Build
The project is compiled using the `ghc` compiler with the command `make` and  program `dka-2-mka` is created

## Run
When program is created, it can be run as follows:
```
$ ./dka-2-mka (-i|-t) [input-file]
```
`input-file` is the name of an input file (if not specified, the program reads standard input) containing the DFA.

Option `-i` prints loaded DFA in the following format to standard output:
```
list of all states
input alphabet
initial state
list of accept states
rule1
...
ruleN
```
`list of all states` is a sequence of states `state1,state2,...,stateM` with following conditions:
1. spaces between states or after comma are not allowed
2. list cannot be empty
3. duplicates of states are not allowed
4. state can be named with positive numbers (0,1,2,...)

`input alphabet` is a subset of the set of lowercase letters `[a-z]`. Input alphabet cannot be empty.

`initial state` must be an element of list of states.

For `list of accept states` applies the same as for `list of all states` and every state of list must be an element of list of states.

Rules are in format `source state`,`input symbol`,`destination state` separated with comma and no spaces. `source state` and `destination state` must be elements of `list of all states`, `input symbol` an element of `input alphabet`. Program also checks, if there are no duplicated rules.

With option `-t`, program minimizes loaded DFA and prints it to standard output.

## Description
Program checks if given DFA is in correct format according to conditions mentioned above. If some of the condition is not met, error message is printed to standard error output and program terminates. Otherwise, with option `-t` is created minimal deterministic finite automaton and printed to standard output. The source codes are located in the following files:
- `Parser.hs` - an input parser, creates the internal representation of DFA
- `DFAModule.hs` -  print and validation of input
- `MDFAModule.hs` - an implementation of the minimization of deterministic finite automaton. The implementation is realized in 3 steps according to algorithms from TIN scripts: elimination of the unreachable states, conversion of automaton to a fully defined DFA and subsequently reduced DFA
- `dka-2-mka.hs` - the main program
