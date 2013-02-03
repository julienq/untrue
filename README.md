untrue
======

Compiler and VM for the untrue language, which is pretty much the same as the
[FALSE language](http://strlen.com/false-language), with a few differences:

  * Numbers are signed 32-bit integers
  * Pick is ` (there is no inline assembly)
  * There is no flush (should there be? ^ needs to work better)

The compiler is `untruec`, which reads from stdin and outputs to stdout. Source
files are named with the `.u` extensions; compiled files with the `.uc`
extension.

Compiled programs can be run with `untrue`, which takes a filename as argument.
Use `-` for stdin. The optional argument `dump` can be used to dump the
program instead of running it (a sort of disassembly.)
