vim9script

g:oscyank_max_length = 1000000
g:oscyank_silent = 1

# Map Visual mode selection directly to OSCYank
vmap <leader>y <Plug>OSCYankVisual

# Auto-send yanked text to system clipboard via OSC 52
augroup VimOSCYank
    autocmd!
    autocmd TextYankPost * {
        if v:event.operator ==# 'y'
            var reg = empty(v:event.regname) ? '"' : v:event.regname
            execute 'OSCYankRegister ' .. reg
        endif
    }
augroup END
