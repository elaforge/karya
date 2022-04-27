" Syntax highlighting for .ky files.
syn match kySection "^\(note\|control\|pitch\) \(generator\|transformer\):$"
syn match kySection "^val:$"
syn match kySection "^alias:$"
syn match kySection "^instrument:$"
hi kySection cterm=underline

syn keyword kyKeyword import imports
hi kyKeyword cterm=underline

" Skip the definition, since it can contain 's, but is not a symbol.
syn match kyDefinition "^[^ ].\{-}= *[^ ]\+"
syn region kySymbol start="'"  skip="''"  end="'" oneline
hi kySymbol ctermfg=DarkBlue

" This goes last, so 'kyDefinition' doesn't override it.
syn match kyComment "--.*$"
hi kyComment cterm=bold
