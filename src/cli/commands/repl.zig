//! sanna repl - Interactive mode for Sanna specifications
//!
//! Provides an interactive Read-Eval-Print Loop for experimenting with
//! Sanna specifications, expressions, and type checking.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Cli = @import("../root.zig").Cli;
const Args = @import("../Args.zig");
const Output = @import("../Output.zig");
const root = @import("../../root.zig");

const Lexer = root.Lexer;
const Parser = root.parser.Parser;
const Ast = root.Ast;
const TypeContext = root.TypeContext;
const TypeChecker = root.TypeChecker;
const PrettyPrinter = root.parser.pretty_print.PrettyPrinter;

/// REPL session state
const ReplSession = struct {
    allocator: Allocator,
    output: *Output,
    type_context: TypeContext,
    history: std.ArrayListUnmanaged([]const u8),
    line_number: usize,
    verbose: bool,

    pub fn init(allocator: Allocator, output: *Output, verbose: bool) ReplSession {
        return .{
            .allocator = allocator,
            .output = output,
            .type_context = TypeContext.init(allocator),
            .history = .{},
            .line_number = 1,
            .verbose = verbose,
        };
    }

    pub fn deinit(self: *ReplSession) void {
        for (self.history.items) |line| {
            self.allocator.free(line);
        }
        self.history.deinit(self.allocator);
        self.type_context.deinit();
    }

    pub fn addToHistory(self: *ReplSession, line: []const u8) !void {
        const owned = try self.allocator.dupe(u8, line);
        try self.history.append(self.allocator, owned);
    }
};

/// Run the REPL command
pub fn run(cli: *Cli, args: *Args) u8 {
    // Check for help flag
    if (args.hasFlag('h', "help")) {
        printHelp(cli);
        return 0;
    }

    // Check for verbose flag
    const verbose = args.hasFlag('v', "verbose");

    // Initialize REPL session
    var session = ReplSession.init(cli.allocator, &cli.output, verbose);
    defer session.deinit();

    // Print welcome banner
    printBanner(&cli.output);

    // Main REPL loop
    replLoop(&session) catch |err| {
        cli.output.err("REPL error: {}", .{err});
        return 1;
    };

    return 0;
}

fn printBanner(output: *Output) void {
    output.printColored(.cyan, "Sanna REPL", .{});
    output.print(" v0.1.0\n", .{});
    output.print("Type ", .{});
    output.printColored(.bold, ":help", .{});
    output.print(" for commands, ", .{});
    output.printColored(.bold, ":quit", .{});
    output.print(" to exit.\n\n", .{});
}

fn replLoop(session: *ReplSession) !void {
    const stdin_file = std.fs.File.stdin();
    var read_buf: [4096]u8 = undefined;
    var stdin = stdin_file.reader(&read_buf);

    while (true) {
        // Print prompt
        session.output.printColored(.green, "sanna", .{});
        session.output.printColored(.dim, "({d})", .{session.line_number});
        session.output.print("> ", .{});

        // Read line using Zig 0.15 I/O interface
        const line = stdin.interface.takeDelimiterExclusive('\n') catch |err| {
            if (err == error.EndOfStream) {
                // EOF - exit gracefully
                session.output.print("\n", .{});
                break;
            }
            session.output.err("\nRead error: {}", .{err});
            continue;
        };

        const trimmed = std.mem.trim(u8, line, " \t\r\n");

        if (trimmed.len == 0) continue;

        // Handle REPL commands
        if (trimmed[0] == ':') {
            const should_continue = handleCommand(session, trimmed);
            if (!should_continue) break;
            continue;
        }

        // Process input
        processInput(session, trimmed);
        session.line_number += 1;

        // Add to history
        session.addToHistory(trimmed) catch {};
    }

    session.output.info("Goodbye!", .{});
}

/// Handle REPL commands (starting with :)
fn handleCommand(session: *ReplSession, input: []const u8) bool {
    const cmd = input[1..]; // Skip the ':'

    // Parse command and arguments
    var iter = std.mem.splitScalar(u8, cmd, ' ');
    const command = iter.first();
    const rest = iter.rest();

    if (std.mem.eql(u8, command, "quit") or std.mem.eql(u8, command, "q")) {
        return false; // Signal to exit
    } else if (std.mem.eql(u8, command, "help") or std.mem.eql(u8, command, "h") or std.mem.eql(u8, command, "?")) {
        printReplHelp(session.output);
    } else if (std.mem.eql(u8, command, "clear") or std.mem.eql(u8, command, "c")) {
        clearContext(session);
    } else if (std.mem.eql(u8, command, "type") or std.mem.eql(u8, command, "t")) {
        if (rest.len > 0) {
            showType(session, rest);
        } else {
            session.output.warn("Usage: :type <expression>", .{});
        }
    } else if (std.mem.eql(u8, command, "ast") or std.mem.eql(u8, command, "a")) {
        if (rest.len > 0) {
            showAst(session, rest);
        } else {
            session.output.warn("Usage: :ast <expression or declaration>", .{});
        }
    } else if (std.mem.eql(u8, command, "load") or std.mem.eql(u8, command, "l")) {
        if (rest.len > 0) {
            loadFile(session, rest);
        } else {
            session.output.warn("Usage: :load <file.sanna>", .{});
        }
    } else if (std.mem.eql(u8, command, "history") or std.mem.eql(u8, command, "hist")) {
        showHistory(session);
    } else if (std.mem.eql(u8, command, "context") or std.mem.eql(u8, command, "ctx")) {
        showContext(session);
    } else {
        session.output.warn("Unknown command: :{s}", .{command});
        session.output.print("Type :help for available commands.\n", .{});
    }

    return true; // Continue REPL
}

fn printReplHelp(output: *Output) void {
    output.print("\n", .{});
    output.printColored(.bold, "REPL Commands:\n", .{});
    output.print("  :help, :h, :?     Show this help\n", .{});
    output.print("  :quit, :q         Exit the REPL\n", .{});
    output.print("  :clear, :c        Clear type context\n", .{});
    output.print("  :type <expr>      Show the type of an expression\n", .{});
    output.print("  :ast <input>      Show the AST of input\n", .{});
    output.print("  :load <file>      Load a Sanna file\n", .{});
    output.print("  :history          Show input history\n", .{});
    output.print("  :context          Show current type context\n", .{});
    output.print("\n", .{});
    output.printColored(.bold, "Input Types:\n", .{});
    output.print("  Expressions:      1 + 2, x > 0 and y < 10\n", .{});
    output.print("  Declarations:     type Email = string\n", .{});
    output.print("  Specifications:   spec fn foo(x: i32) -> i32\n", .{});
    output.print("\n", .{});
    output.printColored(.bold, "Examples:\n", .{});
    output.print("  sanna(1)> type UserId = i64\n", .{});
    output.print("  sanna(2)> forall x: i32: x + 0 == x\n", .{});
    output.print("  sanna(3)> :type 42 + 3\n", .{});
    output.print("\n", .{});
}

fn clearContext(session: *ReplSession) void {
    session.type_context.deinit();
    session.type_context = TypeContext.init(session.allocator);
    session.output.success("Context cleared.", .{});
}

fn showHistory(session: *ReplSession) void {
    if (session.history.items.len == 0) {
        session.output.info("No history yet.", .{});
        return;
    }

    session.output.printColored(.bold, "History:\n", .{});
    for (session.history.items, 1..) |line, i| {
        session.output.printColored(.dim, "  {d}: ", .{i});
        session.output.print("{s}\n", .{line});
    }
}

fn showContext(session: *ReplSession) void {
    // TODO: Implement showing the current type context
    session.output.info("Type context display not yet implemented.", .{});
}

/// Process regular input (expressions or declarations)
fn processInput(session: *ReplSession, input: []const u8) void {
    // Use arena for this parse operation
    var arena = std.heap.ArenaAllocator.init(session.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // First try to parse as a module (which can contain declarations or expressions)
    var lexer = Lexer.init(alloc, input);
    const tokens = lexer.tokenize() catch |err| {
        session.output.err("Lexer error: {}", .{err});
        return;
    };

    if (lexer.hasErrors()) {
        for (lexer.errors.items) |err| {
            session.output.err("{d}:{d}: {s}", .{ err.location.line, err.location.column, err.message });
        }
        return;
    }

    // Try parsing as a complete module (for declarations)
    var parser = Parser.init(alloc, tokens.items);
    const module_result = parser.parseModule();

    if (module_result) |module| {
        if (!parser.hasErrors()) {
            // Successfully parsed as a module
            if (module.declarations.len > 0) {
                // Process declarations
                for (module.declarations) |decl| {
                    processDeclaration(session, decl);
                }
            } else {
                // No declarations - might be an expression wrapped oddly
                session.output.printColored(.dim, "(no declarations parsed)\n", .{});
            }
            return;
        }
    } else |_| {
        // Module parsing failed - show the errors
    }

    // Report parse errors
    if (parser.hasErrors()) {
        for (parser.diagnostics.items) |diag| {
            if (diag.severity == .err) {
                session.output.err("{d}:{d}: {s}", .{
                    diag.span.start.line,
                    diag.span.start.column,
                    diag.message,
                });
                if (diag.context) |ctx| {
                    session.output.printColored(.dim, "  (while parsing {s})\n", .{ctx});
                }
            }
        }
    }
}

fn processDeclaration(session: *ReplSession, decl: Ast.Declaration) void {
    switch (decl.kind) {
        .type_def => |td| {
            session.output.printColored(.green, "type ", .{});
            session.output.printColored(.bold, "{s}", .{td.name.name});
            switch (td.body) {
                .alias => |alias| {
                    session.output.print(" = ", .{});
                    printTypeExpr(session.output, alias);
                },
                .product => |fields| {
                    session.output.print(" = {{ ", .{});
                    for (fields, 0..) |field, i| {
                        if (i > 0) session.output.print(", ", .{});
                        session.output.print("{s}: ", .{field.name.name});
                        printTypeExpr(session.output, field.type_expr);
                    }
                    session.output.print(" }}", .{});
                },
                .sum => |variants| {
                    session.output.print(" = ", .{});
                    for (variants, 0..) |variant, i| {
                        if (i > 0) session.output.print(" ", .{});
                        session.output.print("| {s}", .{variant.name.name});
                    }
                },
            }
            session.output.print("\n", .{});
        },
        .spec_fn => |sf| {
            session.output.printColored(.green, "spec fn ", .{});
            session.output.printColored(.bold, "{s}", .{sf.name.name});
            session.output.print("(", .{});
            for (sf.params, 0..) |param, i| {
                if (i > 0) session.output.print(", ", .{});
                if (param.is_self) {
                    session.output.print("self", .{});
                } else {
                    session.output.print("{s}: ", .{param.name.name});
                    printTypeExpr(session.output, param.type_expr);
                }
            }
            session.output.print(") -> ", .{});
            printTypeExpr(session.output, sf.return_type);
            session.output.print("\n", .{});

            // Show requires/ensures counts
            if (sf.requires.len > 0) {
                session.output.printColored(.dim, "  requires: {d} clause(s)\n", .{sf.requires.len});
            }
            if (sf.ensures.len > 0) {
                session.output.printColored(.dim, "  ensures: {d} clause(s)\n", .{sf.ensures.len});
            }
        },
        .invariant => |inv| {
            session.output.printColored(.green, "invariant", .{});
            if (inv.name) |name| {
                session.output.print(" {s}", .{name.name});
            }
            session.output.print(": ", .{});
            printExpression(session.output, inv.condition);
            session.output.print("\n", .{});
        },
        .axiom => |ax| {
            session.output.printColored(.green, "axiom ", .{});
            session.output.printColored(.bold, "{s}", .{ax.name.name});
            session.output.print(": ", .{});
            printExpression(session.output, ax.condition);
            session.output.print("\n", .{});
        },
        .lemma => |lem| {
            session.output.printColored(.green, "lemma ", .{});
            session.output.printColored(.bold, "{s}", .{lem.name.name});
            session.output.print(": ", .{});
            printExpression(session.output, lem.condition);
            session.output.print("\n", .{});
        },
        .model_def => |md| {
            session.output.printColored(.green, "model ", .{});
            session.output.printColored(.bold, "{s}", .{md.name.name});
            session.output.print("\n", .{});
        },
        .spec_interface => |si| {
            session.output.printColored(.green, "interface ", .{});
            session.output.printColored(.bold, "{s}", .{si.name.name});
            session.output.print("\n", .{});
        },
    }
}

fn printTypeExpr(output: *Output, type_expr: Ast.TypeExpr) void {
    switch (type_expr.kind) {
        .named => |name| {
            for (name.parts, 0..) |part, i| {
                if (i > 0) output.print(".", .{});
                output.print("{s}", .{part.name});
            }
        },
        .generic => |gen| {
            for (gen.base.parts, 0..) |part, i| {
                if (i > 0) output.print(".", .{});
                output.print("{s}", .{part.name});
            }
            output.print("[", .{});
            for (gen.args, 0..) |arg, i| {
                if (i > 0) output.print(", ", .{});
                printTypeExpr(output, arg);
            }
            output.print("]", .{});
        },
        .function => |func| {
            output.print("fn(", .{});
            for (func.params, 0..) |param, i| {
                if (i > 0) output.print(", ", .{});
                printTypeExpr(output, param);
            }
            output.print(") -> ", .{});
            printTypeExpr(output, func.return_type.*);
        },
        .tuple => |types| {
            output.print("(", .{});
            for (types, 0..) |t, i| {
                if (i > 0) output.print(", ", .{});
                printTypeExpr(output, t);
            }
            output.print(")", .{});
        },
        .optional => |inner| {
            output.print("?", .{});
            printTypeExpr(output, inner.*);
        },
        .result => |res| {
            output.print("Result[", .{});
            printTypeExpr(output, res.ok_type.*);
            output.print(", ", .{});
            printTypeExpr(output, res.err_type.*);
            output.print("]", .{});
        },
        .self_type => output.print("Self", .{}),
        .hole => output.print("???", .{}),
    }
}

fn printExpression(output: *Output, expr: Ast.Expression) void {
    switch (expr.kind) {
        .int_literal => |val| output.print("{d}", .{val}),
        .float_literal => |val| output.print("{d}", .{val}),
        .string_literal => |val| output.print("\"{s}\"", .{val}),
        .bool_literal => |val| output.print("{s}", .{if (val) "true" else "false"}),
        .identifier => |id| output.print("{s}", .{id.name}),
        .qualified => |qn| {
            for (qn.parts, 0..) |part, i| {
                if (i > 0) output.print(".", .{});
                output.print("{s}", .{part.name});
            }
        },
        .binary => |bin| {
            output.print("(", .{});
            printExpression(output, bin.left.*);
            output.print(" {s} ", .{@tagName(bin.op)});
            printExpression(output, bin.right.*);
            output.print(")", .{});
        },
        .unary => |un| {
            output.print("{s}(", .{@tagName(un.op)});
            printExpression(output, un.operand.*);
            output.print(")", .{});
        },
        .and_expr => |ae| {
            output.print("(", .{});
            printExpression(output, ae.left.*);
            output.print(" and ", .{});
            printExpression(output, ae.right.*);
            output.print(")", .{});
        },
        .or_expr => |oe| {
            output.print("(", .{});
            printExpression(output, oe.left.*);
            output.print(" or ", .{});
            printExpression(output, oe.right.*);
            output.print(")", .{});
        },
        .not_expr => |ne| {
            output.print("not ", .{});
            printExpression(output, ne.operand.*);
        },
        .implies => |imp| {
            output.print("(", .{});
            printExpression(output, imp.antecedent.*);
            output.print(" => ", .{});
            printExpression(output, imp.consequent.*);
            output.print(")", .{});
        },
        .forall => |q| {
            output.print("forall ", .{});
            for (q.variables, 0..) |v, i| {
                if (i > 0) output.print(", ", .{});
                output.print("{s}", .{v.name.name});
            }
            output.print(": ", .{});
            printExpression(output, q.body.*);
        },
        .exists => |q| {
            output.print("exists ", .{});
            for (q.variables, 0..) |v, i| {
                if (i > 0) output.print(", ", .{});
                output.print("{s}", .{v.name.name});
            }
            output.print(": ", .{});
            printExpression(output, q.body.*);
        },
        .if_expr => |ie| {
            output.print("if ", .{});
            printExpression(output, ie.condition.*);
            output.print(" then ", .{});
            printExpression(output, ie.then_branch.*);
            if (ie.else_branch) |eb| {
                output.print(" else ", .{});
                printExpression(output, eb.*);
            }
        },
        .let_expr => |le| {
            output.print("let {s} = ", .{le.name.name});
            printExpression(output, le.value.*);
            output.print(" in ", .{});
            printExpression(output, le.body.*);
        },
        .old => |old| {
            output.print("old(", .{});
            printExpression(output, old.*);
            output.print(")", .{});
        },
        .result => output.print("result", .{}),
        .self_expr => output.print("self", .{}),
        .sequence_literal => |seq| {
            output.print("[", .{});
            for (seq, 0..) |elem, i| {
                if (i > 0) output.print(", ", .{});
                printExpression(output, elem);
            }
            output.print("]", .{});
        },
        .set_literal => |set| {
            output.print("{{", .{});
            for (set, 0..) |elem, i| {
                if (i > 0) output.print(", ", .{});
                printExpression(output, elem);
            }
            output.print("}}", .{});
        },
        .call => |call| {
            printExpression(output, call.callee.*);
            output.print("(", .{});
            for (call.args, 0..) |arg, i| {
                if (i > 0) output.print(", ", .{});
                printExpression(output, arg);
            }
            output.print(")", .{});
        },
        .field_access => |fa| {
            printExpression(output, fa.object.*);
            output.print(".{s}", .{fa.field.name});
        },
        .index_access => |ia| {
            printExpression(output, ia.object.*);
            output.print("[", .{});
            printExpression(output, ia.index.*);
            output.print("]", .{});
        },
        .method_call => |mc| {
            printExpression(output, mc.object.*);
            output.print(".{s}(", .{mc.method.name});
            for (mc.args, 0..) |arg, i| {
                if (i > 0) output.print(", ", .{});
                printExpression(output, arg);
            }
            output.print(")", .{});
        },
        .hole => output.print("???", .{}),
        else => output.print("<expr>", .{}),
    }
}

fn showType(session: *ReplSession, input: []const u8) void {
    // Parse and type-check the expression
    var arena = std.heap.ArenaAllocator.init(session.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lexer = Lexer.init(alloc, input);
    const tokens = lexer.tokenize() catch |err| {
        session.output.err("Lexer error: {}", .{err});
        return;
    };

    if (lexer.hasErrors()) {
        for (lexer.errors.items) |err| {
            session.output.err("{s}", .{err.message});
        }
        return;
    }

    // For now, just parse and show success
    var parser = Parser.init(alloc, tokens.items);
    _ = parser.parseModule() catch |err| {
        session.output.err("Parse error: {}", .{err});
        return;
    };

    if (parser.hasErrors()) {
        for (parser.diagnostics.items) |diag| {
            if (diag.severity == .err) {
                session.output.err("{s}", .{diag.message});
            }
        }
        return;
    }

    // TODO: Actually run type inference
    session.output.info("Type inference not yet implemented. Expression parsed successfully.", .{});
}

fn showAst(session: *ReplSession, input: []const u8) void {
    var arena = std.heap.ArenaAllocator.init(session.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lexer = Lexer.init(alloc, input);
    const tokens = lexer.tokenize() catch |err| {
        session.output.err("Lexer error: {}", .{err});
        return;
    };

    if (lexer.hasErrors()) {
        for (lexer.errors.items) |err| {
            session.output.err("{s}", .{err.message});
        }
        return;
    }

    var parser = Parser.init(alloc, tokens.items);
    const module = parser.parseModule() catch |err| {
        session.output.err("Parse error: {}", .{err});
        return;
    };

    if (parser.hasErrors()) {
        for (parser.diagnostics.items) |diag| {
            if (diag.severity == .err) {
                session.output.err("{s}", .{diag.message});
            }
        }
        return;
    }

    // Print AST
    session.output.print("\n", .{});
    var printer = PrettyPrinter.init(.{
        .show_locations = session.verbose,
        .max_depth = 10,
    });

    // Use ArrayListUnmanaged as writer for AST output
    var output_buffer = std.ArrayListUnmanaged(u8){};
    defer output_buffer.deinit(session.allocator);

    printer.printModule(output_buffer.writer(session.allocator), module) catch |err| {
        session.output.err("Print error: {}", .{err});
        return;
    };

    session.output.print("{s}", .{output_buffer.items});
}

fn loadFile(session: *ReplSession, path: []const u8) void {
    // Read file content
    const content = std.fs.cwd().readFileAlloc(session.allocator, path, 10 * 1024 * 1024) catch |err| {
        session.output.err("Failed to read file '{s}': {}", .{ path, err });
        return;
    };
    defer session.allocator.free(content);

    // Parse the file
    var arena = std.heap.ArenaAllocator.init(session.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lexer = Lexer.init(alloc, content);
    const tokens = lexer.tokenize() catch |err| {
        session.output.err("Lexer error: {}", .{err});
        return;
    };

    if (lexer.hasErrors()) {
        session.output.err("Lexer errors in file:", .{});
        for (lexer.errors.items) |err| {
            session.output.err("  {d}:{d}: {s}", .{ err.location.line, err.location.column, err.message });
        }
        return;
    }

    var parser = Parser.init(alloc, tokens.items);
    const module = parser.parseModule() catch |err| {
        session.output.err("Parse error: {}", .{err});
        return;
    };

    if (parser.hasErrors()) {
        session.output.err("Parse errors in file:", .{});
        for (parser.diagnostics.items) |diag| {
            if (diag.severity == .err) {
                session.output.err("  {d}:{d}: {s}", .{ diag.span.start.line, diag.span.start.column, diag.message });
            }
        }
        return;
    }

    // Report what was loaded
    session.output.success("Loaded '{s}':", .{path});
    if (module.imports.len > 0) {
        session.output.printColored(.dim, "  {d} import(s)\n", .{module.imports.len});
    }
    if (module.declarations.len > 0) {
        session.output.printColored(.dim, "  {d} declaration(s)\n", .{module.declarations.len});
    }

    // Process each declaration
    for (module.declarations) |decl| {
        processDeclaration(session, decl);
    }
}

fn printHelp(cli: *Cli) void {
    const help =
        \\sanna repl - Interactive mode for Sanna specifications
        \\
        \\USAGE:
        \\    sanna repl [OPTIONS]
        \\
        \\OPTIONS:
        \\    -h, --help       Print help information
        \\    -v, --verbose    Show verbose output (e.g., source locations in AST)
        \\
        \\DESCRIPTION:
        \\    Start an interactive REPL session for experimenting with Sanna
        \\    specifications, types, and expressions.
        \\
        \\    In the REPL, you can:
        \\    - Define types:        type Email = string
        \\    - Write specifications: spec fn validate(x: i32) -> bool
        \\    - Write axioms:        axiom comm: forall a, b: i32: a + b == b + a
        \\    - Evaluate expressions: 1 + 2 * 3
        \\
        \\    Type :help in the REPL for available commands.
        \\
        \\EXAMPLES:
        \\    sanna repl              # Start interactive session
        \\    sanna repl --verbose    # Start with verbose output
        \\
    ;
    cli.output.print("{s}", .{help});
}
