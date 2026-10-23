" Syntax highlighting for .tscore and .jscore files.

" Standard guard so loading twice is a no-op.  ky.vim unlets this to include
" these rules.
if exists('b:current_syntax')
    finish
endif

" I have to use the awkward regexes because :syn match keyword doesn't
" work because I don't want to modify iskeyword to put in %.
syn match tsDirective "%\(default-call\|order\)\>"
syn match tsDirective "%\(dur\|meter\|negative\|scale\|f\)\>"
syn match tsDirective "%ky\>"
" jscore only
syn match tsDirective "%\(source\|piece\|section\|laras\|irama\|instrument\)\>"
hi tsDirective cterm=underline

syn match tsTrackTitle ">[!@#$%^&*]*[a-z0-9.-]*"
hi tsTrackTitle ctermfg=DarkBlue

syn region tsString start='"' skip='"(' end='"' oneline
hi tsString ctermfg=DarkBlue

syn match tsComment "--.*$"
hi tsComment cterm=bold

" Turn off >80 column highlight.
match none

let b:current_syntax = 'tscore'
