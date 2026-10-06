" vim mapping and syntax to edit .ky files.
" This is sourced by bin/vi for each *.ky buffer, so everything should be
" buffer-local.

" Hook saving to notify seq to check and reread the ky file.
" Even if you're editing an indirectly imported files, it should
" notice the change.
"
" Alternately, seq could use fsevents to notice the edit itself, but
" this seems lighter weight, but is vim-specific and less reliable.
nmap <buffer> gz :call <SID>Send('LState.checkKy')<cr>
nmap <buffer> ZZ :call <SID>Send('LState.checkKy')<cr>:q<cr>

" Like :!, but don't do the "press ENTER to continue" prompt if it has no
" output.
function! s:ExecuteQuietly(cmd)
    " Without silent, the screen gets corrupted and needs redraw!
    silent let output = system(a:cmd)
    if substitute(output, ' ', '', 'g') != ''
        echo output
    endif
endfunction

function! s:Send(cmd)
    silent w
    call s:ExecuteQuietly('build/opt/send ''' . a:cmd . ''' < ' . expand('%'))
endfunction

" Syntax highlighting for .ky files.
syn clear
syn match kySection "^\(note\|control\|pitch\) \(generator\|transformer\):$"
syn match kySection "^\(val\|alias\|instrument\|tscore\):$"
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

let b:current_syntax = 'ky'
