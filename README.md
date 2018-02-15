# Turing Machine Model
An implementation of Turing Machine and Automaton for Language Theory

  [![turingMachine](https://img.shields.io/badge/turingMachine-v1.0.0.0-blue.svg?style=plastic)](https://hackage.haskell.org/package/turingMachine)
  [![Build Status](https://travis-ci.org/sanjorgek/turingMachine.svg?branch=master)](https://travis-ci.org/sanjorgek/turingMachine)
  [![Code Climate](https://codeclimate.com/github/sanjorgek/turingMachine/badges/gpa.svg)](https://codeclimate.com/github/sanjorgek/turingMachine)
  [![Issue Count](https://codeclimate.com/github/sanjorgek/turingMachine/badges/issue_count.svg)](https://codeclimate.com/github/sanjorgek/turingMachine)
  [![CircleCI](https://circleci.com/gh/sanjorgek/turingMachine.svg?style=svg)](https://circleci.com/gh/sanjorgek/turingMachine)

## Math Models
### Finite Automaton

Finite State machine, with no memory.
Exist:

1. Recognizer
2. Generator
3. Transducer

### Stack Automaton
Stack memory machine with states

### Turing Machine

## To Do

- [ ] Finite Automaton
  - [x] Delta
    - [x] Deterministic
    - [x] Non-deterministic
    - [x] Lift deltas
  - [x] Lambda
    - [x] Lambda1
    - [x] Lambda2
    - [x] Lift lambda
  - [ ] Recognizer
    - [x] Deterministic def
    - [x] Non-deterministic def
    - [x] Check Word
    - [ ] k-distinguishable states
    - [ ] Distinguishable states
    - [ ] Equivalent states
    - [x] Equivalent recognizer
    - [x] Non-deterministic to deterministic, and viceversa
    - [x] Recheable recognizer
    - [x] Distinguishable recognizer
    - [x] Minimize recognizer
    - [ ] Remove Ambiguity
    - [x] Language cadinality
  - [ ] Transductor
    - [x] Moore
    - [x] Mealy
    - [x] translate
    - [ ] Moore to Mealy, and viceversa   
  - [ ] Recognizer with epsilon transitions
    - [ ] def
    - [ ] Recognizer with epsilon transitions to Recognizer without epsilon transitions
- [ ] Stack Automaton
  - [x] Lift delta
  - [x] Deterministic stack automaton def
  - [ ] Non-deterministic stack automaton def
  - [ ] Non-deterministic to deterministic stack automaton
  - [ ] Recognizer with epsilon transitions
- [ ] Turing Machine
  - [ ] Class def
  - [ ] Tape def
  - [ ] Delta def
  - [ ] Accept word
