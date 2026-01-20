# Sanna Neovim Plugin

Language support for the Sanna specification language in Neovim.

## Features

- Syntax highlighting for `.sanna` files
- LSP integration (via nvim-lspconfig)
  - Real-time diagnostics
  - Code completion
  - Hover documentation
  - Go-to-definition
- Commands for verification workflow
- Keymaps for common actions

## Installation

### Using lazy.nvim

```lua
{
    "sanna/sanna.nvim",
    dependencies = {
        "neovim/nvim-lspconfig",
    },
    ft = "sanna",
    opts = {
        -- Configuration options
    },
}
```

### Using packer.nvim

```lua
use {
    "sanna/sanna.nvim",
    requires = { "neovim/nvim-lspconfig" },
    ft = "sanna",
    config = function()
        require("sanna").setup()
    end,
}
```

### Manual Installation

1. Copy the contents of `editors/neovim` to your Neovim config:
   - `ftdetect/` -> `~/.config/nvim/ftdetect/`
   - `syntax/` -> `~/.config/nvim/syntax/`
   - `ftplugin/` -> `~/.config/nvim/ftplugin/`
   - `lua/sanna/` -> `~/.config/nvim/lua/sanna/`

2. Add to your `init.lua`:

```lua
require("sanna").setup()
```

## Configuration

```lua
require("sanna").setup({
    -- Path to sanna executable (nil = use PATH)
    server_path = nil,

    -- LSP settings
    lsp = {
        -- Auto-start LSP when opening .sanna files
        autostart = true,
        -- Enable diagnostics
        diagnostics = true,
        -- Enable hover
        hover = true,
        -- Enable completion
        completion = true,
        -- Enable go-to-definition
        definition = true,
    },
})
```

## Commands

| Command | Description |
|---------|-------------|
| `:SannaVerify` | Verify current specification |
| `:SannaApprove` | Mark specification as approved |
| `:SannaReview` | Request human review |
| `:SannaTrust` | Calculate trust score |

## Keymaps

When in a Sanna buffer, the following keymaps are available:

| Keymap | Action |
|--------|--------|
| `K` | Show hover documentation |
| `gd` | Go to definition |
| `<C-Space>` | Trigger completion (insert mode) |
| `<leader>sv` | Verify specification |
| `<leader>sa` | Approve specification |
| `<leader>sr` | Request review |
| `<leader>st` | Show trust score |

## Without LSP

If you don't want LSP support, you can still use basic syntax highlighting
by just copying the `ftdetect/`, `syntax/`, and `ftplugin/` directories.

## Alternative: nvim-lspconfig Only

If you prefer to configure LSP manually:

```lua
require('lspconfig.configs').sanna = {
    default_config = {
        cmd = { 'sanna', 'lsp' },
        filetypes = { 'sanna' },
        root_dir = function(fname)
            return require('lspconfig.util').find_git_ancestor(fname)
        end,
    },
}
require('lspconfig').sanna.setup{}
```

## Requirements

- Neovim 0.8+
- `sanna` executable in PATH
- nvim-lspconfig (optional, for LSP features)

## License

MIT
