//! Sanna Type System Module
//!
//! This module provides the type system for Sanna specifications, including:
//! - Type representation (primitives, named types, generics, functions, etc.)
//! - Type environment and scope management
//! - Type checking and inference
//! - Subtype checking for refinement types
//! - Generic type instantiation and unification
//!
//! ## Usage
//!
//! ```zig
//! const types = @import("types/root.zig");
//!
//! // Create a type context
//! var ctx = types.TypeContext.init(allocator);
//! defer ctx.deinit();
//!
//! // Create a type checker
//! var checker = types.TypeChecker.init(&ctx);
//!
//! // Check a module
//! const result = try checker.checkModule(module);
//! ```

const std = @import("std");

// Core type representation
pub const Type = @import("Type.zig");

// Type environment and scope management
pub const TypeContext = @import("TypeContext.zig").TypeContext;
pub const Scope = @import("TypeContext.zig").Scope;
pub const TypeError = @import("TypeContext.zig").TypeError;
pub const InterfaceDefinition = @import("TypeContext.zig").InterfaceDefinition;
pub const AssociatedType = @import("TypeContext.zig").AssociatedType;
pub const MethodSignature = @import("TypeContext.zig").MethodSignature;

// Type checker
pub const TypeChecker = @import("TypeChecker.zig").TypeChecker;
pub const TypeCheckResult = @import("TypeChecker.zig").TypeCheckResult;

// Subtyping and unification
pub const Subtyping = @import("Subtyping.zig").Subtyping;
pub const Unification = @import("Subtyping.zig").Unification;

// Re-export commonly used types from Type module
pub const IntType = Type.IntType;
pub const FloatType = Type.FloatType;
pub const NamedType = Type.NamedType;
pub const GenericType = Type.GenericType;
pub const FunctionType = Type.FunctionType;
pub const ResultType = Type.ResultType;
pub const TupleType = Type.TupleType;
pub const ModelType = Type.ModelType;
pub const TypeVar = Type.TypeVar;
pub const Bound = Type.Bound;
pub const TypeDefinition = Type.TypeDefinition;
pub const TypeParam = Type.TypeParam;
pub const Field = Type.Field;
pub const Variant = Type.Variant;
pub const Invariant = Type.Invariant;
pub const ModelDefinition = Type.ModelDefinition;
pub const ModelMember = Type.ModelMember;
pub const Signature = Type.Signature;
pub const Param = Type.Param;
pub const Axiom = Type.Axiom;
pub const builtins = Type.builtins;

// Tests
test {
    std.testing.refAllDecls(@This());
}
