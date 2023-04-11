autocmd TextYankPost *
    \ if v:event.operator is 'y' && v:event.regname is '' |
    \ execute 'OSCYankRegister +' |
    \ endif
