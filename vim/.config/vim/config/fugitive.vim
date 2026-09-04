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

# Better Git log browsing
autocmd FileType git SetupFugitiveLogMappings()

def SetupFugitiveLogMappings(): void
  nnoremap <buffer> o    <ScriptCmd>OpenCommitDiff()<CR>
  nnoremap <buffer> <CR> <ScriptCmd>OpenCommitDiff()<CR>
enddef

def OpenCommitDiff(): void
  const hash = matchstr(getline('.'), '^commit\s\+\zs\x\{7,40}\ze\>')

  if empty(hash)
    echohl WarningMsg
    echomsg 'No commit hash found on this line'
    echohl None
    return
  endif

  execute 'rightbelow Gvsplit ' .. hash
enddef

# Better diff colors
highlight DiffAdd    guifg=#b5bd68 guibg=#29422f gui=NONE
highlight DiffDelete guifg=#cc6666 guibg=#4a2b2b gui=NONE
highlight DiffChange guifg=#c5c8c6 guibg=#333b4d gui=NONE
highlight DiffText   guifg=#c5c8c6 guibg=#465775 gui=underline



