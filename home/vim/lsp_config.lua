-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
-- https://github.com/neovim/nvim-lspconfig#suggested-configuration
local opts = {noremap = true, silent = true}

vim.keymap.set('n', '<space>lh', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[l', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']l', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>lq', vim.diagnostic.setloclist, opts)
vim.keymap.set('n', '<space>ll', vim.lsp.codelens.run, opts)

-- https://github.com/nvim-lua/lsp-status.nvim#all-together-now
local lsp_status = require('lsp-status')

local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local bufopts = {noremap = true, silent = true, buffer = bufnr}
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
    -- vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
    -- vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
    -- vim.keymap.set('n', '<space>wl', function()
    --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    -- end, bufopts)
    vim.keymap.set('n', '<space>ld', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<space>lr', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', '<space>la', vim.lsp.buf.code_action, bufopts)
    vim.keymap.set('n', '<space>lf', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', '<space>lt',
                   function() vim.lsp.buf.format {async = true} end, bufopts)

    lsp_status.on_attach(client, bufnr)
end

lsp_status.register_progress()

local lspconfig = require('lspconfig')
local cmp_capabilities = require('cmp_nvim_lsp').default_capabilities()

-- https://github.com/dlukes/dotfiles/commit/8feb47aec3a2c7ff78b8efc1e85d9580a99fd6a4
-- capabilities can be extended with tbl_extend it seems
lspconfig.hls.setup {on_attach = on_attach, capabilities = cmp_capabilities}
lspconfig.pyright.setup {on_attach = on_attach, capabilities = cmp_capabilities}

-- https://github.com/neovim/nvim-lspconfig/wiki/Autocompletion#nvim-cmp
local cmp = require('cmp')
cmp.setup {
    -- snippet ...
    mapping = cmp.mapping.preset.insert({
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        -- ['<CR>'] = cmp.mapping.confirm {
        --   behavior = cmp.ConfirmBehavior.Replace,
        --   select = true,
        -- },
        ['<Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            else
                fallback()
            end
        end, {'i', 's'}),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, {'i', 's'})
    }),
    sources = {{name = 'nvim_lsp'}, {name = 'buffer'}}
}
