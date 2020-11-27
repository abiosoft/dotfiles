
" fugitive
set statusline+=%{fugitive#statusline()}

if has('nvim')
    set diffopt+=vertical
else
    set diffopt-=internal
    set diffopt+=vertical
endif

autocmd BufReadPost fugitive://* set bufhidden=delete

