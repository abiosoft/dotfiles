" layout
" let g:fzf_layout = { 'window': { 'width': 0.65, 'height': 0.65 } }

" preview config, use bat to take advantage of syntax highlighting
let $FZF_DEFAULT_OPTS="--ansi --preview-window 'right:60%' --layout reverse --margin=1,4 --preview 'bat --color=always --style=header,grid --line-range :300 {} --ignore node_modules'"

let $FZF_DEFAULT_COMMAND='ag -g ""'
" autocmd VimEnter * command! -bang -nargs=? GFiles call fzf#vim#gitfiles(<q-args>, {'options': '--no-preview'}, <bang>0)

