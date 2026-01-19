//! Parser module for the Sanna specification language.
//!
//! This module provides AST node types and parsing functionality for Sanna.

const std = @import("std");

pub const Ast = @import("Ast.zig");

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

test {
    _ = Ast;
}
