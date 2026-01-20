" Sanna filetype settings

" Indentation
setlocal expandtab
setlocal shiftwidth=4
setlocal tabstop=4
setlocal softtabstop=4

" Comments
setlocal commentstring=//\ %s

" Folding based on braces
setlocal foldmethod=syntax

" Set omni completion if LSP not available
setlocal omnifunc=syntaxcomplete#Complete
