" Vim syntax file
" Language: Sanna
" Maintainer: Sanna Project

if exists("b:current_syntax")
  finish
endif

" Keywords - Specification
syn keyword sannaKeyword spec type model interface impl module import
syn keyword sannaKeyword requires ensures modifies invariant axiom lemma
syn keyword sannaKeyword pure old result trusted

" Keywords - Control
syn keyword sannaControl if then else match let in

" Keywords - Quantifiers
syn keyword sannaQuantifier forall exists such_that

" Keywords - Logical
syn keyword sannaOperator and or not implies

" Keywords - Set
syn keyword sannaOperator union intersect subset empty

" Boolean literals
syn keyword sannaBoolean true false

" Primitive types
syn keyword sannaType Int Nat Bool String Float Unit Void

" Collection types
syn keyword sannaType List Set Map Option Result Array Seq

" Domain types
syn keyword sannaType Money DateTime Duration Email Url UUID

" User-defined types (capitalized identifiers)
syn match sannaTypeUser "\<[A-Z][a-zA-Z0-9_]*\>"

" Attributes
syn match sannaAttribute "@[a-zA-Z_][a-zA-Z0-9_]*"

" Numbers
syn match sannaNumber "\<\d\+\>"
syn match sannaNumber "\<\d\+\.\d\+\([eE][+-]\?\d\+\)\?\>"
syn match sannaNumber "\<0x[0-9a-fA-F]\+\>"

" Strings
syn region sannaString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=sannaEscape
syn match sannaEscape contained "\\."

" Comments
syn match sannaComment "//.*$" contains=sannaTodo
syn match sannaDocComment "///.*$" contains=sannaTodo
syn region sannaBlockComment start="/\*" end="\*/" contains=sannaTodo
syn region sannaDocBlockComment start="/\*\*" end="\*/" contains=sannaTodo
syn keyword sannaTodo contained TODO FIXME XXX NOTE

" Operators
syn match sannaOperator "=>"
syn match sannaOperator "<=>"
syn match sannaOperator "->"
syn match sannaOperator "::"
syn match sannaOperator "\.\."
syn match sannaOperator "=="
syn match sannaOperator "!="
syn match sannaOperator "<="
syn match sannaOperator ">="
syn match sannaOperator "&&"
syn match sannaOperator "||"

" Hole placeholder
syn match sannaHole "???"

" Highlighting
hi def link sannaKeyword Keyword
hi def link sannaControl Conditional
hi def link sannaQuantifier Keyword
hi def link sannaOperator Operator
hi def link sannaBoolean Boolean
hi def link sannaType Type
hi def link sannaTypeUser Type
hi def link sannaAttribute PreProc
hi def link sannaNumber Number
hi def link sannaString String
hi def link sannaEscape SpecialChar
hi def link sannaComment Comment
hi def link sannaDocComment SpecialComment
hi def link sannaBlockComment Comment
hi def link sannaDocBlockComment SpecialComment
hi def link sannaTodo Todo
hi def link sannaHole Todo

let b:current_syntax = "sanna"
