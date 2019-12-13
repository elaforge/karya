" local vim config
au BufWritePost *.hs            silent !tools/init_tags %
au BufWritePost *.hsc           silent !tools/init_tags %

" utils for converting to under_scores

" nnoremap + /\.\@<!\<[a-z]\+[A-Z]<cr>
" except prelude things:
" forM_? mapM fromMaybe errorIO putStr\k\+

" nnoremap _ /[A-Z]<cr>i_<esc>l~

" py import replace
" nnoremap <silent> _ :py replace.to_snake(vim)<cr>n
