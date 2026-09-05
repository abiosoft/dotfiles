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

#
# Better Git log browsing
#
autocmd FileType git SetupFugitiveLogMappings()
autocmd FileType fugitive SetupFugitiveLogMappings()

def SetupFugitiveLogMappings(): void
  nnoremap <buffer> <CR> <ScriptCmd>OpenCommitDiff()<CR>
enddef

def CommitAtCursor(): string
  const line = getline('.')
  const cursor_col = col('.') - 1
  const pattern = '\<[0-9a-fA-F]\{7,40}\>'

  var search_col = 0
  var first_hash = ''

  while true
    const result = matchstrpos(line, pattern, search_col)

    if result[1] < 0
      break
    endif

    if empty(first_hash)
      first_hash = result[0]
    endif

    const match_start = result[1]
    const match_end = result[2]

    if cursor_col >= match_start && cursor_col < match_end
      return result[0]
    endif

    search_col = match_end
  endwhile

  return first_hash
enddef

def OpenCommitDiff(): void
  const hash = CommitAtCursor()

  if empty(hash)
    echohl WarningMsg
    echomsg 'No commit hash found on this line'
    echohl None
    return
  endif

  execute 'rightbelow Gvsplit ' .. hash
enddef

