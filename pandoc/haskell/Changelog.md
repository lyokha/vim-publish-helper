### 0.1.2.0

- Better treatment of handles of temporary files.
- Using *WriteMode* for vim process *std_in* prevents vim from getting
  unresponsive on *Ctrl-C* interrupts while still doing well its tasks.
- Using */dev/null* for vim process *std_out*.
- Exit program as soon as vim fails.

