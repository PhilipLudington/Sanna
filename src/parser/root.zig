//! Parser module for the Sanna specification language.
//!
//! This module provides AST node types and parsing functionality for Sanna.

const std = @import("std");

pub const Ast = @import("Ast.zig");
pub const Parser = @import("Parser.zig");
pub const pretty_print = @import("pretty_print.zig");

// Re-export commonly used types
pub const Module = Ast.Module;
pub const Import = Ast.Import;
pub const Declaration = Ast.Declaration;
pub const DeclarationKind = Ast.DeclarationKind;
pub const TypeDefinition = Ast.TypeDefinition;
pub const ModelDefinition = Ast.ModelDefinition;
pub const FunctionSpec = Ast.FunctionSpec;
pub const InterfaceSpec = Ast.InterfaceSpec;
pub const TypeExpr = Ast.TypeExpr;
pub const Expression = Ast.Expression;
pub const Identifier = Ast.Identifier;
pub const QualifiedName = Ast.QualifiedName;
pub const TypeParameter = Ast.TypeParameter;
pub const Attribute = Ast.Attribute;
pub const Visibility = Ast.Visibility;

// Re-export parser types
pub const ParseError = Parser.ParseError;
pub const Diagnostic = Parser.Diagnostic;
pub const ParseContext = Parser.ParseContext;
pub const FormattedDiagnostic = Parser.FormattedDiagnostic;

// Re-export pretty printer
pub const PrettyPrinter = pretty_print.PrettyPrinter;
pub const PrintOptions = pretty_print.PrintOptions;
pub const printModule = pretty_print.printModule;
pub const debugPrintModule = pretty_print.debugPrintModule;

test {
    _ = Ast;
    _ = Parser;
    _ = pretty_print;
}
