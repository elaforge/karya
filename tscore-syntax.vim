" Syntax highlighting for .tscore files.

" I have to use the awkward regexes because :syn match keyword doesn't
" work because I don't want to modify iskeyword to put in %.
syn match tsDirective "%default-call\>"
syn match tsDirective "%\(dur\|meter\|negative\|scale\|f\)\>"
syn match tsDirective "%\(ky\|instruments\)\>"
hi tsDirective cterm=underline

syn match tsTrackTitle ">[!@#$%^&*]*[a-z0-9.-]*"
hi tsTrackTitle ctermfg=DarkBlue

syn region tsString start='"' skip='"(' end='"' oneline
hi tsString ctermfg=DarkBlue

syn match tsComment "--.*$"
hi tsComment cterm=bold

" Turn off >80 column highlight.
match none
