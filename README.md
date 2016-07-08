# Turing Machine Model
An implementation of Turing Machine and Automaton for language theory

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

## Settings
Init
~~~bash
$ cabal configure
$ cabal build
$ cabal check
$ hlint .
$ cabal sdist
~~~
Reload docs
~~~bash
$ cabal hscolour
$ cabal haddock --hyperlink-source
~~~

And upload to git
~~~
$ git tag -a vX.X COMMIT
$ git push origin --tags
~~~
