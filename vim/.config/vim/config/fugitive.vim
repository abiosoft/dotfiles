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
