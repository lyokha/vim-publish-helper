### 0.3.1.0

- Do not throw the error if */dev/tty* cannot be open. Note that Neovim loads
  the syntax engine without *tty* emulation just fine. This allows using Neovim
  as *VIMHL_BACKEND* in environments where */dev/tty* is unavailable.

### 0.3.0.0

- Support for packages *base* &lt; *4.8* and *pandoc-types* &lt; *1.20* was
  dropped.
- If output format is *gfm*, the code block gets translated to a *RawBlock* in
  *HTML* format with style adhered to code blocks in Github README pages.
- Use environment variable *VIMRC_PANDOC* to point to the custom vim
  configuration.
- Cabal flag *debug* was removed.

### 0.2.0.1

- Removed dependency on package *safe*.

### 0.2.0.0

- Removed dependency on packages *regex-compat* and *cond*.

### 0.1.4.0

- Use environment variable *VIMHL_BACKEND* to choose between vim flavors.

### 0.1.3.0

- Updated after Pandoc *2.8* and the *String-to-Text* migration.

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

