{ pkgs, lib, config, unstable, ... }:
let
  np = pkgs.vimPlugins;
  unp = unstable.vimPlugins;

  snippets = {
    programs.neovim.plugins = [
      np.vim-snippets
      {
        plugin = np.UltiSnips;
        config = ''
          nn <leader>fs :Snippets<CR>
          set rtp+=${./.}
        '';
      }
    ];
  };

in
{
  imports = [ snippets ];
  programs.git.ignores = [ "*~" "*.swp" "*.swo" "tags" "TAGS" ];
  programs.neovim = {
    enable = true;
    vimAlias = true;
    vimdiffAlias = true;
    viAlias = true;
    # extraPackages = [ ];
    extraConfig = ''
      set nocompatible
      filetype indent plugin on
      syntax on
      set hidden
      set confirm
      set wildmenu
      set showcmd
      set nostartofline
      set backspace=2
      set hlsearch
      set incsearch
      set inccommand=nosplit
      set linebreak
      set mouse=a
      set copyindent
      set ignorecase
      set smartcase
      set number
      let g:tex_flavor = "latex"
      nn j gj
      nn k gk
      vn j gj
      vn k gk
      vn > >gv
      vn < <gv
      let mapleader = "\<space>"
      let maplocalleader = "\<space>\<space>"
      " set tabstop=4
      " set softtabstop=4
      " set shiftwidth=4
      au BufNewFile,BufRead *.md  set spell
      nn <leader>w :w<CR>
      nn <leader>lo :lopen<CR>
      nn <leader>hl :nohl<CR>
      inoremap hj <esc>
      vmap <leader>y :w! /tmp/vitmp<CR>
      nmap <leader>p :r! cat /tmp/vitmp<CR>
      set foldmethod=indent
      set foldcolumn=0
      set foldlevel=999
      set diffopt+=vertical
    '';
    plugins = [
      # workaround for https://github.com/nix-community/home-manager/pull/2391
      # np.commentary
      {
        plugin = np.commentary;
        config = ''
          let mapleader = "\<space>"
          let maplocalleader = "\<space>\<space>"
        '';
      }
      np.surround
      np.vim-easymotion
      np.vim-eunuch
      np.vim-indent-object
      np.vim-polyglot
      np.vim-repeat
      np.vim-unimpaired

      {
        plugin = np.goyo-vim;
        config = ''
          nnoremap <leader>mg :Goyo<CR>
        '';
      }

      {
        plugin = np.airline;
        config = ''
          let g:airline_powerline_fonts = 1
          let g:airline#extensions#branch#displayed_head_limit = 10
        '';
      }

      {
        # TODO unstable for filetypes.any, remove later
        plugin = unp.formatter-nvim;
        # https://github.com/mhartington/formatter.nvim/tree/master/lua/formatter/filetypes
        config =
          let
            formatters = {
              python = { exe = "black"; args = [ "-q" "-" ]; };
              haskell.exe = "ormolu";
              cabal.exe = "${pkgs.haskellPackages.cabal-fmt}/bin/cabal-fmt";
              markdown.exe = "md-headerfmt";
              nix.exe = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
              go.exe = "gofmt";
              sh.exe = "${pkgs.shfmt}/bin/shfmt";
              proto.exe = "${pkgs.clang-tools}/bin/clang-format";
            };
            quote = str: "\"${str}\"";
            mkFmt = ft: { exe, stdin ? true, args ? [ ] }: ''
              ${ft} = {
                function()
                  return {
                    exe = ${quote exe},
                    args = { ${lib.concatMapStringsSep ", " quote args } },
                    stdin = ${if stdin then "true" else "false"},
                  }
                end
              },
            '';
          in
          ''
            lua << EOF
            local util = require "formatter.util"
            require("formatter").setup {
              logging = true,
              log_level = vim.log.levels.WARN,
              filetype = {
                ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkFmt formatters)}
                ["*"] = {
                  require("formatter.filetypes.any").remove_trailing_whitespace
                },
              },
            }
            EOF
            augroup FormatAutogroup
              autocmd!
              autocmd BufWritePost * FormatWrite
            augroup END
          '';
      }

      {
        plugin = np.vim-localvimrc;
        config = ''
          let g:localvimrc_persistent = 1
        '';
      }
      {
        plugin = np.vim-easy-align;
        config = ''
          xmap <Enter> <Plug>(LiveEasyAlign)
        '';
      }
      {
        plugin = np.emmet-vim;
        config = ''
          let g:user_emmet_install_global = 0
          autocmd FileType html,css EmmetInstall
        '';
      }
      {
        plugin = np.vim-mundo;
        config = ''
          nnoremap <leader>u :MundoToggle<CR>
        '';
      }

      {
        plugin = np.fugitive;
        config = ''
          nn <leader>gs :Git<CR>
          nn <leader>gg :Git<CR>
        '';
      }
      {
        plugin = np.vim-gitgutter;
        config = ''
          nn <leader>ga :GitGutterStageHunk<CR>
          nn <leader>gp :GitGutterPreviewHunk<CR>
          nn <leader>gu :GitGutterUndoHunk<CR>
          nn <leader>gn :GitGutterNextHunk<CR>
          nn <leader>gp :GitGutterPrevHunk<CR>
        '';
      }
      {
        plugin = np.fzf-vim;
        config = ''
          nn <leader>ff :Files<CR>
          nn <leader>fg :Ag<CR>
          nn <leader>ft :Tags<CR>
          nn <leader>fh :Helptags<CR>
          nn <leader>fb :Buffers<CR>
          autocmd FileType haskell let g:fzf_tags_command = 'fast-tags -R'
          au BufWritePost *.hs silent! !${pkgs.haskellPackages.fast-tags}/bin/fast-tags -R . &
          let $FZF_DEFAULT_COMMAND = 'ag -g ""'
        '';
      }
      {
        plugin = np.vim-highlightedyank;
        config = ''
          let g:highlightedyank_highlight_duration = 200
        '';
      }
      {
        plugin = np.indent-blankline-nvim;
        config = ''
          let g:indent_blankline_char = '▏'
          nn <leader>ii :IndentBlanklineToggle<CR>
        '';
      }
      {
        plugin = np.nerdtree;
        config = ''
          nn <C-n> :NERDTreeToggle<CR>
        '';
      }
      {
        plugin = np.auto-pairs;
        config =
          let doubleSingle = "''"; in
          ''
            autocmd FileType nix let b:AutoPairs = AutoPairsDefine({"${doubleSingle}" : "${doubleSingle}", '=':';'}, ["'"])
          '';
      }
      {
        plugin = np.nord-vim;
        config = "colorscheme nord";
      }
      {
        plugin = np.vim-markdown;
        config = ''
          let g:vim_markdown_auto_insert_bullets = 1
          let g:vim_markdown_new_list_item_indent = 0
          let g:vim_markdown_toc_autofit = 1
          autocmd FileType markdown nnoremap o A<CR>
          autocmd FileType markdown nnoremap <leader>t :Toc<CR>
        '';
      }

      np.cmp-nvim-lsp
      {
        plugin = np.nvim-lspconfig;
        # https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
        # https://github.com/neovim/nvim-lspconfig#suggested-configuration
        config = ''
          autocmd BufEnter,CursorHold,InsertLeave <buffer> lua vim.lsp.codelens.refresh()
          lua << EOF
          local opts = { noremap=true, silent=true }

          vim.keymap.set('n', '<space>lh', vim.diagnostic.open_float, opts)
          vim.keymap.set('n', '[l', vim.diagnostic.goto_prev, opts)
          vim.keymap.set('n', ']l', vim.diagnostic.goto_next, opts)
          vim.keymap.set('n', '<space>lq', vim.diagnostic.setloclist, opts)
          vim.keymap.set('n', '<space>ll', vim.lsp.codelens.run, opts)

          -- Use an on_attach function to only map the following keys
          -- after the language server attaches to the current buffer
          local on_attach = function(client, bufnr)
            -- Enable completion triggered by <c-x><c-o>
            vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

            -- Mappings.
            -- See `:help vim.lsp.*` for documentation on any of the below functions
            local bufopts = { noremap=true, silent=true, buffer=bufnr }
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
            vim.keymap.set('n', '<space>lt', function() vim.lsp.buf.format { async = true } end, bufopts)
          end

          local lspconfig = require('lspconfig')
          local cmp_capabilities = require('cmp_nvim_lsp').default_capabilities()
          lspconfig.hls.setup{
            on_attach = on_attach,
            capabilities = cmp_capabilities,
          }
          lspconfig.pyright.setup{
            on_attach = on_attach,
            capabilities = cmp_capabilities,
          }
          EOF
        '';
      }

      # https://github.com/neovim/nvim-lspconfig/wiki/Autocompletion#nvim-cmp
      {
        plugin = np.nvim-cmp;
        config = ''
          lua << EOF
          local cmp = require('cmp')
          cmp.setup {
            -- snippet ...
            mapping = cmp.mapping.preset.insert({
              ['<C-d>'] = cmp.mapping.scroll_docs(-4),
              ['<C-f>'] = cmp.mapping.scroll_docs(4),
              ['<C-Space>'] = cmp.mapping.complete(),
              ['<CR>'] = cmp.mapping.confirm {
                behavior = cmp.ConfirmBehavior.Replace,
                select = true,
              },
              ['<Tab>'] = cmp.mapping(function(fallback)
                if cmp.visible() then
                  cmp.select_next_item()
                elseif luasnip.expand_or_jumpable() then
                  luasnip.expand_or_jump()
                else
                  fallback()
                end
              end, { 'i', 's' }),
              ['<S-Tab>'] = cmp.mapping(function(fallback)
                if cmp.visible() then
                  cmp.select_prev_item()
                elseif luasnip.jumpable(-1) then
                  luasnip.jump(-1)
                else
                  fallback()
                end
              end, { 'i', 's' }),
            }),
            sources = {
              { name = 'nvim_lsp' },
              -- { name = 'luasnip' },
            },
          }
          EOF
        '';
      }

    ];
  };
}
