# Dirs

* `bap` - holds uarch opt specific checkers and the main runner (all
  current checker code)

* `checker.py` - legacy checkers runners, for use in eval, holds all
  but the small bitwidth checker
  
* `data` - holds some testing code and previous csv files with reports
  used to evaluate precision of the legacy checkers 
  
* `proposed-code` - holds a legacy checker that was a prototype for the
  small bitwidth checker compiling to one of boolector's input languages
  
* `synth` - contains old synthesis code _and the code that is used to
  verify transforms_
  
* `serval` - contains our modified serval version. _must be used
  instead of the main branch of serval_ if you want to use the things
  in the `synth` dir
  
* `test` - contains testing code and test cases for checkers

# Setup

## For synth/verification stuff

1. Install racket (I'm using v8.5)
2. Install rosette. Only version 4 (should be the latest) will work.
3. Install _the local serval repo_ using the directions in the dir's
   README file
   
## For checker stuff

Follow the instructions on the bap
[github](https://github.com/BinaryAnalysisPlatform/bap) for
installation instructions. 
   
The versions of ocaml, opam, and bap that I have been using for dev
and testing are: 4.12.0, 2.0.7, and 2.5.0 respectively. Any bap
version <2.5.0 may not work.
