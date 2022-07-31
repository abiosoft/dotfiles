local lspconfig = require'lspconfig'

local on_attach_vim = function(client)
  require'completion'.on_attach(client)
end

lspconfig.pyls.setup{on_attach=on_attach_vim}
lspconfig.gopls.setup{
    on_attach = on_attach_vim;
    cmd = { "gopls", "serve" };
    filetypes = { "go", "gomod" };
    root_dir = lspconfig.util.root_pattern("go.mod", ".git");
}

