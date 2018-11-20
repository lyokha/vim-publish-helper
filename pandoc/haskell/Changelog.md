### 0.1.2.4

- Removed *shebang* line as it may lead to compilation failures.

### 0.1.2.3

- Added support for older GHC *7.8*.

### 0.1.2.2

- Added Cabal flag *debug* (off by default) for printing how vim runs to
  *stderr*.

### 0.1.2.0

- Better treatment of handles of temporary files.
- Using *WriteMode* for vim process *std_in* prevents vim from getting
  unresponsive on *Ctrl-C* interrupts while still doing well its tasks.
- Using */dev/null* for vim process *std_out*.
- Exit program as soon as vim fails.

