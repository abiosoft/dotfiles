vim9script

# Enable vertical diff splits by default
set diffopt+=vertical

# Git Branch Checkout via FZF (Replaces raw bash invocation)
def GitCheckoutFzf()
    fzf#run(fzf#wrap({
        'source': 'git branch -a --format="%(refname:short)"',
        'sink': (branch) => execute('!git checkout ' .. branch),
        'options': '--reverse --prompt="Checkout Branch> "'
    }))
enddef
command! Gcheckout GitCheckoutFzf()

# Better diff colors
highlight DiffAdd    guifg=NONE guibg=#29422f gui=NONE
highlight DiffDelete guifg=NONE guibg=#4a2b2b gui=NONE
highlight DiffChange guifg=NONE guibg=#333b4d gui=NONE
highlight DiffText   guifg=NONE guibg=#465775 gui=underline

# Better navigation
autocmd FileType git setlocal splitbelow splitright
autocmd FileType fugitive setlocal splitbelow splitright

