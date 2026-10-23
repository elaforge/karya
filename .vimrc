" local vim config
" TODO not everyone uses init_tags, make this conditional?
au BufWritePost *.hs            silent !tools/init_tags %
au BufWritePost *.hsc           silent !tools/init_tags %

au BufRead,BufNewFile *.ky      source ./ky.vim
au BufRead,BufNewFile *.tscore,*.jscore
    \ source ./tscore-syntax.vim | runtime! ftplugin/tscore.vim
