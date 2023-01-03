This directory contains ELF object files used for testing the
checker's code paths that handle interprocedural functions and data. I
normally run this development on ELF files using clang, but the object
files in this directory need to be linked together to have BAP lift
these object files to a format similar to those found in libsodium and
hacl-star. Since linking ELF files using macOS-clang is hard on macOS,
these have been linked using containers and now live here, not to be
messed with by other Makefiles (i.e., accidentally cleaned).
