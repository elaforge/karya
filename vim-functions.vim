" Useful vim functions.

" Like :!, but don't do the "press ENTER to continue" prompt if it has no
" output.
function! ExecuteQuietly(cmd)
    " Without silent, the screen gets corrupted and needs redraw!
    silent let output = system(a:cmd)
    if substitute(output, ' ', '', 'g') != ''
        echo output
    endif
endfunction

function! Send(cmd)
    silent w
    call ExecuteQuietly('build/opt/send ''' . a:cmd . ''' < ' . expand('%'))
endfunction
