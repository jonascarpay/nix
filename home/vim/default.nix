{ pkgs
, unstable
, lib
, config
, ...
}:
let
  np = pkgs.vimPlugins;

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
    # Adapted from https://github.com/VonHeikemen/lsp-zero.nvim?tab=readme-ov-file#quickstart-for-the-impatient
    programs.neovim.extraLuaConfig = ''
      -- Reserve a space in the gutter
      vim.opt.signcolumn = 'yes'

      -- Add cmp_nvim_lsp capabilities settings to lspconfig
      -- This should be executed before you configure any language server
      local lspconfig_defaults = require('lspconfig').util.default_config
      lspconfig_defaults.capabilities = vim.tbl_deep_extend(
        'force',
        lspconfig_defaults.capabilities,
        require('cmp_nvim_lsp').default_capabilities()
      )

      -- This is where you enable features that only work
      -- if there is a language server active in the file
      vim.api.nvim_create_autocmd('LspAttach', {
        desc = 'LSP actions',
        callback = function(event)
          local opts = {buffer = event.buf}

          vim.keymap.set('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
          vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
          vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts)
          vim.keymap.set('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
          vim.keymap.set('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>', opts)
          vim.keymap.set('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>', opts)
          vim.keymap.set('n', 'gs', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
          vim.keymap.set('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
          vim.keymap.set({'n', 'x'}, '<F3>', '<cmd>lua vim.lsp.buf.format({async = true})<cr>', opts)
          vim.keymap.set('n', '<F4>', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts)
          vim.keymap.set('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>', opts)
          vim.keymap.set('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>', opts)
        end,
      })

      local cmp = require('cmp')
      local luasnip = require('luasnip')
      local lsp_zero = require('lsp-zero')
      local cmp_action = lsp_zero.cmp_action()

      cmp.setup({
        preselect = 'item',
        completion = {
          completeopt = 'menu,menuone,noinsert'
        },
        sources = {
          {name = 'path'},
          {name = 'nvim_lsp'},
          {name = 'luasnip', keyword_length = 2},
          {name = 'buffer', keyword_length = 3},
        },
        -- window = {
        --   completion = cmp.config.window.bordered(),
        --   documentation = cmp.config.window.bordered(),
        -- },
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ['<C-y>'] = cmp.mapping.confirm({ select = true }),
          ['<C-space>'] = cmp_action.toggle_completion(),
          -- Navigate between snippet placeholder
          ['<C-f>'] = cmp_action.luasnip_jump_forward(),
          ['<C-b>'] = cmp_action.luasnip_jump_backward(),
          -- Scroll up and down in the completion documentation
          ['<C-u>'] = cmp.mapping.scroll_docs(-4),
          ['<C-d>'] = cmp.mapping.scroll_docs(4),
        })
      })

      local from_snipmate = require("luasnip.loaders.from_snipmate")
      from_snipmate.lazy_load()
      from_snipmate.load({paths = "${./snippets}"})

      require('trouble').setup {}

      local lspconfig = require('lspconfig')
      ${config.programs.neovim.extraLspConfig}
    '';
  };

  oil.programs.neovim = {
    plugins = [
      np.oil-nvim
      np.mini-icons
      np.nvim-web-devicons
    ];
    extraConfig = ''
      :set splitright
      :set splitbelow
    '';
    extraLuaConfig = ''
      require("oil").setup({
        default_file_explorer = true,
        columns = { "icon" },
        view_options = {
          natural_order = "fast",
        }
      })
      vim.api.nvim_set_keymap('n', '<C-n>', ':Oil<CR>', { noremap = true, })
      -- Sadly, oil on startup causes more problems than it's worth
      -- -- https://github.com/stevearc/oil.nvim/issues/268#issuecomment-1880161152
      -- vim.api.nvim_create_autocmd("VimEnter", {
      --   callback = vim.schedule_wrap(function(data)
      --     if vim.fn.argc() == 0 then
      --       require("oil").open()
      --     end
      --   end),
      -- })
    '';
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

  treehopper.programs.neovim = {
    plugins = [
      (
        pkgs.vimUtils.buildVimPlugin {
          name = "treehopper";
          src = pkgs.fetchFromGitHub {
            owner = "mfussenegger";
            repo = "nvim-treehopper";
            rev = "0b9f5c8980ab1427644ff70059f7ae0fd89b547e";
            sha256 = "sha256-M2yA1wYowtA5Dqk4J9zjSUZz+ZrpqXp25k5N5adVDgE=";
          };
        }
      )
    ];
    extraConfig = ''
      omap     <silent> m :<C-U>lua require('tsht').nodes()<CR>
      xnoremap <silent> m :lua require('tsht').nodes()<CR>
    '';

  };

  lang-haskell.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.haskell ];
    formatters = {
      haskell = {
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
    extraConfig = ''
      autocmd FileType haskell let g:fzf_tags_command = 'fast-tags -R --exclude=dist-newstye .'
      au BufWritePost *.hs silent! !${pkgs.haskellPackages.fast-tags}/bin/fast-tags -R --exclude=dist-newstyle . &
    '';
  };

  lang-nix.programs.neovim = {
    extraPackages = [ pkgs.nil ];
    plugins = [ np.nvim-treesitter-parsers.nix ];
    # formatters.nix.exe = "${pkgs.nixfmt-rfc-style}/bin/nixfmt";
    formatters.nix.exe = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
    extraLspConfig = ''
      lspconfig.nil_ls.setup({})
    '';
  };

  lang-cpp.programs = {
    git.ignores = [ ".cache/clangd/" ];
    neovim = {
      plugins = [ np.nvim-treesitter-parsers.cpp ];
      formatters.cpp.exe = "${pkgs.clang-tools}/bin/clang-format";
      extraLspConfig = ''
        lspconfig.clangd.setup({})
      '';
    };
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

  # Some potential config https://users.rust-lang.org/t/any-auto-formatter-for-toml/97292/2
  lang-toml.programs.neovim.formatters.toml = {
    exe = "${pkgs.taplo}/bin/taplo format";
    args = [ "-" ];
  };

  lang-python.programs.neovim =
    let
      unified-formatter = pkgs.writeShellScript "ruff-format-and-sort" ''
        set -e
        ruff format $@
        ruff check --select I --fix $@
      '';
    in
    {
      plugins = [ np.nvim-treesitter-parsers.python ];
      formatters.python = {
        exe = "${unified-formatter}";
        args = [ ];
        stdin = false;
      };
      extraLspConfig = ''
        lspconfig.pyright.setup({})
        lspconfig.ruff.setup({})
      '';
    };

  lang-typst.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.typst ];
    formatters.typst.exe = "${pkgs.typstyle}/bin/typstyle";
    extraConfig = ''
      autocmd BufNewFile,BufRead *.typ setfiletype typst
    '';
    extraLspConfig = "lspconfig.tinymist.setup({})";
  };

  lang-rust.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.rust ];
    formatters.rust = {
      exe = "rustfmt";
    };
    extraLspConfig = ''
      lspconfig.rust_analyzer.setup({})
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

  lang-cmake.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.cmake ];
    formatters.cmake = {
      exe = "${pkgs.cmake-format}/bin/cmake-format";
      stdin = false;
      args = [ "--in-place" ];
    };
    extraLspConfig = ''
      lspconfig.cmake.setup({ cmd = { "${pkgs.cmake-language-server}/bin/cmake-language-server" } })
    '';
  };

  lang-glsl.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.glsl ];
    extraLspConfig = ''
      lspconfig.glsl_analyzer.setup({ cmd = { "${pkgs.glslls}/bin/glslls" } })
    '';
  };

  lang-vimdoc.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.vimdoc ];
  };

  spell.programs.neovim.extraConfig = ''
    autocmd FileType markdown setlocal spell
    autocmd FileType text setlocal spell
    autocmd FileType gitcommit setlocal spell
  '';
in
{
  imports = [
    ./options.nix
    lsp
    oil
    workspace-symbols
    treesitter
    treehopper
    cursorline
    lang-bash
    lang-cmake
    lang-cpp
    lang-glsl
    lang-haskell
    lang-javascript
    lang-nix
    lang-python
    lang-rust
    lang-toml
    lang-typst
    lang-vimdoc
    spell
  ];
  programs.git.ignores = [
    "*~"
    "*.swp"
    "*.swo"
    "tags"
    "!tags/"
  ];
  programs.neovim = {
    package = unstable.neovim-unwrapped;
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
      nn <leader>w :silent w<CR>
      nn <leader>hl :nohl<CR>
      inoremap hj <esc>
      vmap <leader>y :w! /tmp/vitmp<CR>
      nmap <leader>p :r! cat /tmp/vitmp<CR>
      set diffopt+=vertical
      set fdm=indent
      set foldlevel=99
      set nowrap
      noremap <2-LeftMouse> :lua vim.lsp.buf.definition()<CR>
      nnoremap z0 :set foldlevel=0<cr>
      nnoremap z1 :set foldlevel=1<cr>
      nnoremap z2 :set foldlevel=2<cr>
      nnoremap z3 :set foldlevel=3<cr>
      nnoremap z4 :set foldlevel=4<cr>
      nnoremap z5 :set foldlevel=5<cr>
      nnoremap z6 :set foldlevel=6<cr>
      nnoremap z7 :set foldlevel=7<cr>
      nnoremap z8 :set foldlevel=8<cr>
      nnoremap z9 :set foldlevel=99<cr>
      nnoremap z] :set foldlevel+=1<cr>
      nnoremap z[ :set foldlevel-=1<cr>
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

      # np.vim-signature
      # np.vim-polyglot # only used for proper indent on cc
      np.commentary # TODO switch to Comment.nvim
      np.surround
      np.vim-indent-object
      np.vim-repeat
      np.vim-unimpaired
      np.vim-sleuth

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
            mkFmt =
              ft: { exe, stdin ? true, args ? [ ], raw_args ? [ ] }: ''
                ${ft} = {
                  function()
                    return {
                      exe = ${quote exe},
                      args = { ${lib.concatStringsSep ", " (builtins.map quote args ++ raw_args)} },
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
        config = ''
          nn <leader>ff :Files<CR>
          nn <leader>fg :Ag<CR>
          nn <leader>ft :Tags<CR>
          nn <leader>fh :Helptags<CR>
          nn <leader>fb :Buffers<CR>
          let $FZF_DEFAULT_COMMAND = '${pkgs.ripgrep}/bin/rg --files'
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
        plugin = np.everforest;
        config = "colorscheme everforest";
      }

      {
        plugin = np.lualine-nvim;
        type = "lua";
        config = ''
          require('lualine').setup({
            options = { theme = 'everforest' },
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
