" local vim config
" TODO not everyone uses init_tags, make this conditional?
au BufWritePost *.hs            silent !tools/init_tags %
au BufWritePost *.hsc           silent !tools/init_tags %
