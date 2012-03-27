" local vim config
au BufWritePost *.hs            silent !tools/init_tags %
au BufWritePost *.hsc           silent !tools/init_tags %
