# Sanna VS Code Extension

Language support for the Sanna specification language.

## Features

- Syntax highlighting for `.sanna` files
- Real-time diagnostics (syntax and type errors)
- Code completion for keywords, types, and attributes
- Hover information with documentation
- Go-to-definition for identifiers
- Commands for verification workflow:
  - **Sanna: Verify Specification** - Run verification
  - **Sanna: Approve Specification** - Mark as approved
  - **Sanna: Request Review** - Request human review
  - **Sanna: Calculate Trust Score** - Show trust score

## Requirements

- The `sanna` command-line tool must be installed and available in your PATH
- Alternatively, configure the path in settings

## Extension Settings

- `sanna.serverPath`: Path to the Sanna executable (default: `sanna`)
- `sanna.trace.server`: Trace level for LSP communication (`off`, `messages`, `verbose`)

## Installation

### From Source

1. Clone the repository
2. Navigate to `editors/vscode`
3. Run `npm install`
4. Run `npm run compile`
5. Copy the extension folder to `~/.vscode/extensions/sanna-0.1.0`

### From VSIX

1. Download the `.vsix` file from releases
2. Run `code --install-extension sanna-0.1.0.vsix`

## Building

```bash
npm install
npm run compile
vsce package  # Creates .vsix file
```

## Development

1. Open this folder in VS Code
2. Run `npm install`
3. Press F5 to launch Extension Development Host
4. Open a `.sanna` file to test

## Syntax Highlighting

The extension provides syntax highlighting for:

- Keywords (`spec`, `type`, `requires`, `ensures`, etc.)
- Types (`Int`, `Bool`, `List`, `Option`, etc.)
- Attributes (`@author`, `@confidence`, `@trusted`, etc.)
- Operators and punctuation
- Comments (line `//` and block `/* */`)
- Strings and numbers

## License

MIT
