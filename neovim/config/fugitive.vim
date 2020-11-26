
" fugitive
set statusline+=%{fugitive#statusline()}
set diffopt+=vertical
autocmd BufReadPost fugitive://* set bufhidden=delete

