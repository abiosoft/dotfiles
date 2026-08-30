vim9script

# Environment options
$FZF_DEFAULT_OPTS = '--ansi --layout=reverse --margin=0,4'

# Use ripgrep (rg) if available, falling back to ag
if executable('rg')
    $FZF_DEFAULT_COMMAND = 'rg --files --hidden --glob "!.git"'
else
    $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -l -g ""'
endif

# Preview window layout (hidden on the right, ctrl-/ to toggle)
g:fzf_preview_window = ['right:hidden', 'ctrl-/']

# Key Mappings
nnoremap <C-p> :Files<CR>
nnoremap <silent> <leader>b :Buffers<CR>
