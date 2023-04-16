" layout
" let g:fzf_layout = { 'window': { 'width': 0.65, 'height': 0.65 } }

let $FZF_DEFAULT_OPTS="--ansi --layout reverse --margin=0,4"

let $FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -l -g ""'

" autocmd VimEnter * command! -bang -nargs=? GFiles call fzf#vim#gitfiles(<q-args>, {'options': '--no-preview'}, <bang>0)
"
" Preview window on the right side of the window,
" hidden by default, ctrl-/ to toggle
let g:fzf_preview_window = ['right:hidden', 'ctrl-/']
" let g:fzf_layout = 'reverse-list'

