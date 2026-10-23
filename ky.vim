" vim mapping and syntax to edit .ky files.
" This is sourced by bin/vi or .vimrc (if `set exrc`) for each *.ky buffer, so
" everything should be buffer-local.

" Hook saving to notify seq to reload the ky file.
" Even if you're editing an indirectly imported files, it should
" notice the change.
"
" Alternately, seq could use fsevents to notice the edit itself, but
" this seems lighter weight, but is vim-specific and less reliable.
"
" This could instead be `au BufWritePost <buffer> call s:Send()`
" but I think I like :w as a low level write.  But, I don't think other people
" use gz like I do, and maybe :w is not really useful, so maybe switch some
" day.
nmap <buffer> gz :call <SID>Send()<cr>
nmap <buffer> ZZ :call <SID>Send()<cr>:q<cr>

" Like :!, but don't do the "press ENTER to continue" prompt if it has no
" output.
function! s:ExecuteQuietly(cmd)
    " Without silent, the screen gets corrupted and needs redraw!
    silent let output = system(a:cmd)
    if substitute(output, ' ', '', 'g') != ''
        echo output
    endif
endfunction

function! s:Send()
    silent w
    call s:ExecuteQuietly('build/opt/send --no-server-ok LState.reloadKy')
endfunction

" Syntax highlighting for .ky files.

syn clear
syn match kySection "^\(note\|control\|pitch\) \(generator\|transformer\):$"
syn match kySection "^\(val\|alias\|instrument\|tscore\):$"
hi kySection cterm=underline

syn keyword kyKeyword import imports
hi kyKeyword cterm=underline

syn region kyString start="'"  skip="''"  end="'" oneline
hi kyString ctermfg=DarkBlue
" Skip the definition LHS, since it can contain 's, but is not a symbol.  The
" LHS is a single word.  This goes after kyString, so it wins when both start
" at the beginning of the line.
syn match kyDefinition "^\S\+\ze\s*="

" This goes last, so 'kyDefinition' doesn't override it.
syn match kyComment "--.*$"
hi kyComment cterm=bold

" Merge in tscore syntax, for the tscore: section.  They're mostly compatible,
" the only sketchy thing is tscore uses "-strings, while ky uses '-strings.
" TODO Why?  I'm sure I thought about it...
" Unlet to get past tscore-syntax.vim's guard.
unlet! b:current_syntax
source <script>:h/tscore-syntax.vim

" Alternately, apply tscore syntax only within the tscore: section:
" unlet! b:current_syntax
" syn include @Tscore <script>:h/tscore-syntax.vim
" syn region kyTscore matchgroup=kySection start="^tscore:$"
"     \ end="^\ze[a-z ]\+:$" contains=@Tscore

let b:current_syntax = 'ky'

" Personal editing settings, e.g. ~/.vim/ftplugin/ky.vim.  This doesn't set
" filetype, so vim won't load it automatically.
runtime! ftplugin/ky.vim
