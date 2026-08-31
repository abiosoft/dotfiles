vim9script

# Netrw drawer behavior: split to the right, keep width at 25%
g:netrw_winsize = 25
g:netrw_browse_split = 4
g:netrw_altv = 1

# Toggle Netrw using ,O
nnoremap <leader>O :Lexplore %:p:h<CR>
