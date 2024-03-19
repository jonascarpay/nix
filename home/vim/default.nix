{ pkgs, lib, unstable, config, ... }:
let
  np = pkgs.vimPlugins;
  unp = unstable.vimPlugins;

  lsp = {
    programs.neovim.plugins = [
      np.lsp-zero-nvim
      np.nvim-lspconfig
      np.nvim-cmp
      np.cmp-nvim-lsp
      np.cmp-buffer
      np.luasnip
      np.cmp_luasnip
      np.vim-snippets
      np.trouble-nvim
    ];
    programs.neovim.extraLuaConfig = ''
      local lsp_zero = require('lsp-zero')

      lsp_zero.on_attach(function(client, bufnr)
        lsp_zero.default_keymaps({buffer = bufnr})
        -- 2023-12-31: Disable LSP syntax highlighting, treesitter seems better
        client.server_capabilities.semanticTokensProvider = nil
      end)
      local lspconfig = require('lspconfig')

      local cmp = require('cmp')
      local cmp_action = lsp_zero.cmp_action()
      cmp.setup({
        sources = {
          {name = 'path'},
          {name = 'nvim_lsp'},
          {name = 'luasnip', keyword_length = 2},
          {name = 'buffer', keyword_length = 3},
        },
        mapping = cmp.mapping.preset.insert({
          -- Navigate between snippet placeholder
          ['<C-f>'] = cmp_action.luasnip_jump_forward(),
          ['<C-b>'] = cmp_action.luasnip_jump_backward(),
          -- Scroll up and down in the completion documentation
          ['<C-u>'] = cmp.mapping.scroll_docs(-4),
          ['<C-d>'] = cmp.mapping.scroll_docs(4),
        })
      })

      local luasnip = require('luasnip')
      local from_snipmate = require("luasnip.loaders.from_snipmate")
      from_snipmate.lazy_load()
      from_snipmate.load({paths = "${./snippets}"})

      ${config.programs.neovim.extraLspConfig}
    '';
  };

  tree = {
    programs.neovim = {
      plugins = [
        np.nvim-tree-lua
        np.nvim-web-devicons
      ];
      # https://github.com/nvim-tree/nvim-tree.lua#quick-start
      extraLuaConfig = ''
        vim.g.loaded_netrw = 1
        vim.g.loaded_netrwPlugin = 1
        vim.opt.termguicolors = true
        require("nvim-tree").setup()
        vim.api.nvim_set_keymap('n', '<C-n>', ':NvimTreeToggle<CR>', { noremap = true, })
      '';
    };
  };

  cursorline.programs.neovim.extraConfig = ''
    augroup CursorLine
      au!
      au VimEnter * setlocal cursorline
      au WinEnter * setlocal cursorline
      au BufWinEnter * setlocal cursorline
      au WinLeave * setlocal nocursorline
    augroup END
  '';

  workspace-symbols.programs.neovim = {
    extraPackages = [ pkgs.bat ];
    plugins = [
      np.plenary-nvim
      {
        plugin = np.fzf-lsp-nvim;
        # NOTE can't use <leader>, not yet bound
        config = ''
          nn <space>fw :WorkspaceSymbols<CR>
        '';
      }
    ];
  };

  treesitter = {
    programs.neovim.plugins = [
      np.nvim-treesitter-parsers.c
      np.nvim-treesitter-parsers.sql
      np.nvim-treesitter-parsers.lua
      np.nvim-treesitter-refactor
      {
        plugin = np.nvim-treesitter;
        type = "lua";
        config = ''
          require('nvim-treesitter.configs').setup {
            highlight = { enable = true },
            indent = { enable = true },
            refactor = {
              highlight_definitions = {
                enable = true,
                clear_on_cursor_move = true,
              },
            },
          }
        '';
      }
    ];
  };

  lang-haskell.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.haskell ];
    formatters = {
      haskell =
        {
          exe = "ormolu";
          raw_args = [
            "\"--stdin-input-file\""
            "util.escape_path(util.get_current_buffer_file_path())"
          ];
        };
      cabal.exe = "${pkgs.haskellPackages.cabal-fmt.bin}/bin/cabal-fmt";
    };
    extraLspConfig = ''
      lspconfig.hls.setup({})
    '';
  };

  lang-nix.programs.neovim = {
    extraPackages = [ pkgs.nil ];
    plugins = [ np.nvim-treesitter-parsers.nix ];
    formatters.nix.exe = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
    extraLspConfig = ''
      lspconfig.nil_ls.setup({})
    '';
  };

  lang-cpp.programs.neovim = {
    extraPackages = [ pkgs.ccls ];
    plugins = [ np.nvim-treesitter-parsers.cpp ];
    formatters.cpp.exe = "${pkgs.clang-tools}/bin/clang-format";
    extraLspConfig = ''
      lspconfig.ccls.setup({})
    '';
  };

  lang-bash.programs.neovim = {
    extraPackages = [
      pkgs.shellcheck
      pkgs.nodePackages.bash-language-server
    ];
    plugins = [ np.nvim-treesitter-parsers.bash ];
    formatters.sh.exe = "${pkgs.shfmt}/bin/shfmt";
    extraLspConfig = ''
      lspconfig.bashls.setup({})
    '';
  };

  lang-python.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.python ];
    formatters.python = { exe = "${pkgs.black}/bin/black"; args = [ "-q" "-" ]; };
    extraLspConfig = ''
      lspconfig.pyright.setup({})
      lspconfig.ruff_lsp.setup({})
    '';
  };

  lang-rust.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.rust ];
    formatters.rust = { exe = "rustfmt"; };
    extraLspConfig = ''
      lspconfig.rust_analyzer.setup({ })
    '';
  };

  lang-javascript.programs.neovim = {
    plugins = [
      np.nvim-treesitter-parsers.javascript
      np.nvim-treesitter-parsers.svelte
      np.nvim-treesitter-parsers.html
    ];
    formatters.javascript = {
      exe = "${pkgs.nodePackages.prettier}/bin/prettier";
      stdin = false;
      args = [ "--write" ];
    };
  };

in
{
  imports = [
    ./options.nix
    lsp
    tree
    workspace-symbols
    treesitter
    cursorline
    lang-bash
    lang-cpp
    lang-haskell
    lang-javascript
    lang-nix
    lang-python
    lang-rust
  ];
  programs.git.ignores = [ "*~" "*.swp" "*.swo" "tags" "!tags/" ];
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    vimAlias = true;
    vimdiffAlias = true;
    viAlias = true;
    extraConfig = ''
      set nocompatible
      filetype indent plugin on
      syntax on
      set hidden                  " hide buffers instead of closing
      set scrolloff=5             " screen padding
      set confirm                 " ask for confirmation instead of closing
      set wildmenu                " CLI buffs
      set showcmd                 " extra info in bottom bar
      set nostartofline           " make certain motions like gg preserve column
      set backspace=2             " allow backspace insert start, line break, indent, i.e. work normally
      set hlsearch
      set incsearch
      set inccommand=nosplit
      set ignorecase
      set smartcase
      set linebreak               " break at word boundaries
      set mouse=a                 " mouse in all modes
      set autoindent
      set copyindent
      set number
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
      nn <leader>w :w<CR>
      nn <leader>hl :nohl<CR>
      inoremap hj <esc>
      vmap <leader>y :w! /tmp/vitmp<CR>
      nmap <leader>p :r! cat /tmp/vitmp<CR>
      set diffopt+=vertical
    '';
    plugins = [
      {
        # workaround for https://github.com/nix-community/home-manager/pull/2391
        plugin = pkgs.hello;
        config = ''
          let mapleader = "\<space>"
          let maplocalleader = "\<space>\<space>"
        '';
      }

      np.vim-polyglot # only used for proper indent on cc
      np.commentary # TODO switch to Comment.nvim
      np.surround
      np.vim-eunuch
      np.vim-indent-object
      np.vim-repeat
      np.vim-unimpaired

      {
        plugin = np.hop-nvim;
        type = "lua";
        config = ''

          local hop = require("hop")
          hop.setup {
            keys = 'hfjdks',
          }
          local directions = require("hop.hint").HintDirection

          vim.keymap.set("", '<space><space>f', function()
            hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = false })
          end, {remap=true})
          vim.keymap.set("", '<space><space>F', function()
            hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = false })
          end, {remap=true})
          vim.keymap.set("", '<space><space>t', function()
            hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = false, hint_offset = -1 })
          end, {remap=true})
          vim.keymap.set("", '<space><space>T', function()
            hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = false, hint_offset = 1 })
          end, {remap=true})
          vim.keymap.set("", '<space><space>w', function()
            hop.hint_words({ direction = directions.AFTER_CURSOR, current_line_only = false })
          end, {remap=true})
          vim.keymap.set("", '<space><space>W', function()
            hop.hint_words({ direction = directions.BEFORE_CURSOR, current_line_only = false })
          end, {remap=true})
          vim.keymap.set("", '<space><space>b', function()
            hop.hint_words({ direction = directions.BEFORE_CURSOR, current_line_only = false })
          end, {remap=true})
          vim.keymap.set("", '<space><space>k', function()
            hop.hint_lines_skip_whitespace({ direction = directions.BEFORE_CURSOR })
          end, {remap=true})
          vim.keymap.set("", '<space><space>j', function()
            hop.hint_lines_skip_whitespace({ direction = directions.AFTER_CURSOR })
          end, {remap=true})
        '';
      }

      {
        plugin = np.formatter-nvim;
        # https://github.com/mhartington/formatter.nvim/tree/master/lua/formatter/filetypes
        config =
          let
            quote = str: "\"${str}\"";
            mkFmt = ft: { exe, stdin ? true, args ? [ ], raw_args ? [ ] }: ''
              ${ft} = {
                function()
                  return {
                    exe = ${quote exe},
                    args = { ${lib.concatStringsSep ", " (builtins.map quote args ++ raw_args) } },
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
                ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkFmt config.programs.neovim.formatters)}
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
        config = "let g:localvimrc_persistent = 1";
      }

      {
        plugin = np.vim-easy-align;
        config = "xmap <Enter> <Plug>(LiveEasyAlign)";
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
        config =
          let
            # ag does not always parse .gitignore correctly it seems, and ignores hidden files
            # git ls-files includes hidden files
            ls-files = pkgs.writeShellScript "git-ls-files-extant" ''
              for file in $(git ls-files --others --cached --exclude-standard); do
                if [ -f $file ]; then
                  echo $file
                fi
              done
            '';
          in
          ''
            nn <leader>ff :Files<CR>
            nn <leader>fg :Ag<CR>
            nn <leader>ft :Tags<CR>
            nn <leader>fh :Helptags<CR>
            nn <leader>fb :Buffers<CR>
            autocmd FileType haskell let g:fzf_tags_command = 'fast-tags -R --exclude=dist-newstye .'
            au BufWritePost *.hs silent! !${pkgs.haskellPackages.fast-tags}/bin/fast-tags -R --exclude=dist-newstyle . &
            let $FZF_DEFAULT_COMMAND = '${ls-files}'
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
        type = "lua";
        config = ''
          require("ibl").setup({
            indent = {
              char = '▏'
            }
          })
        '';
      }

      {
        plugin = np.nvim-autopairs;
        type = "lua";
        config = ''
          require("nvim-autopairs").setup({
            check_ts = true,
          })
        '';
      }

      {
        plugin = np.nord-nvim;
        config = "colorscheme nord";
      }

      {
        plugin = np.lualine-nvim;
        type = "lua";
        config = ''
          require('lualine').setup({
            options = { theme = 'nord' },
            sections = {
              lualine_c = {
                {
                  'filename',
                  path = 1,
                }
              }
            },
          })
        '';
      }

    ];
  };
}
