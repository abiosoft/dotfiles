
" fugitive
set statusline+=%{fugitive#statusline()}

if &diff
    set diffopt-=internal
    set diffopt+=vertical
endif

autocmd BufReadPost fugitive://* set bufhidden=delete

