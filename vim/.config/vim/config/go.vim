vim9script

# ==============================================================================
# Go Core & Formatting Options
# ==============================================================================
g:go_fmt_command = "goimports"
g:go_fmt_fail_silently = 1
g:go_list_type = "quickfix"
g:go_decls_mode = 'fzf'

# Fast type info on cursor hold
set updatetime=100
g:go_auto_type_info = 1

# Syntax Highlighting
g:go_highlight_types = 1
g:go_highlight_functions = 1
g:go_highlight_methods = 1
g:go_highlight_build_constraints = 1
g:go_highlight_generate_tags = 1
g:go_highlight_space_tab_error = 1
g:go_highlight_array_whitespace_error = 1
g:go_highlight_trailing_whitespace_error = 1

# Debugger Window Layout
g:go_debug_windows = {
    'vars': 'rightbelow 60vnew',
    'stack': 'rightbelow 10new',
    'out': 'botright 20new',
}

# ==============================================================================
# Helper Functions
# ==============================================================================

# Run :GoBuild or :GoTestCompile depending on active file type
def BuildGoFiles()
    var filename = expand('%')
    if filename =~# '^\f\+_test\.go$'
        go#cmd#Test(0, 1)
    elseif filename =~# '^\f\+\.go$'
        go#cmd#Build(0)
    endif
enddef

# ==============================================================================
# FileType Autocmds & Mappings
# ==============================================================================
augroup Vim9Go
    autocmd!
    # Go standard tabs (Hard tabs, size 4)
    autocmd FileType go setlocal noexpandtab tabstop=4 shiftwidth=4

    # Alternate File Commands (:A, :AV, :AS)
    autocmd FileType go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
    autocmd FileType go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
    autocmd FileType go command! -bang AS call go#alternate#Switch(<bang>0, 'split')

    # Key Mappings
    autocmd FileType go nmap <buffer> <Leader>dd <Plug>(go-def-vertical)
    autocmd FileType go nmap <buffer> <Leader>dv <Plug>(go-doc-vertical)
    autocmd FileType go nmap <buffer> <Leader>db <Plug>(go-doc-browser)

    autocmd FileType go nmap <buffer> <leader>r  <Plug>(go-run)
    autocmd FileType go nmap <buffer> <leader>t  <Plug>(go-test)
    autocmd FileType go nmap <buffer> <Leader>gt <Plug>(go-coverage-toggle)
    autocmd FileType go nmap <buffer> <Leader>i  <Plug>(go-info)

    autocmd FileType go nmap <buffer> <C-g> :GoDecls<CR>
    autocmd FileType go imap <buffer> <C-g> <Esc>:<C-u>GoDecls<CR>
    autocmd FileType go nnoremap <buffer> <leader>rb :call BuildGoFiles()<CR>
augroup END
