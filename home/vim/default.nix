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
    programs.neovim.extraLuaConfig = /* lua */ ''
      -- Reserve a space in the gutter
      vim.opt.signcolumn = 'yes'

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
          ['<C-space>'] = cmp.mapping.complete(),
          -- Navigate between snippet placeholder
          -- ['<C-f>'] = cmp_action.luasnip_jump_forward(),
          -- ['<C-b>'] = cmp_action.luasnip_jump_backward(),
          -- Scroll up and down in the completion documentation
          ['<C-u>'] = cmp.mapping.scroll_docs(-4),
          ['<C-d>'] = cmp.mapping.scroll_docs(4),
        })
      })

      local from_snipmate = require("luasnip.loaders.from_snipmate")
      from_snipmate.lazy_load()
      from_snipmate.load({paths = "${./snippets}"})

      require('trouble').setup {}
    '';
  };

  oil.programs.neovim = {
    plugins = [
      np.oil-nvim
      np.oil-git-status-nvim
      np.mini-icons
      np.nvim-web-devicons
    ];
    extraConfig = ''
      :set splitright
      :set splitbelow
    '';
    extraLuaConfig = /* lua */ ''
      ---- https://github.com/stevearc/oil.nvim/blob/master/doc/recipes.md#hide-gitignored-files-and-show-git-tracked-hidden-files
      -- helper function to parse output
      local function parse_output(proc)
        local result = proc:wait()
        local ret = {}
        if result.code == 0 then
            for line in vim.gsplit(result.stdout, "\n", { plain = true, trimempty = true }) do
            -- Remove trailing slash
            line = line:gsub("/$", "")
            ret[line] = true
          end
        end
        return ret
        end

      -- build git status cache
      local function new_git_status()
        return setmetatable({}, {
          __index = function(self, key)
            local ignore_proc = vim.system(
              { "git", "ls-files", "--ignored", "--exclude-standard", "--others", "--directory" },
              {
                cwd = key,
                text = true,
              }
            )
            local tracked_proc = vim.system({ "git", "ls-tree", "HEAD", "--name-only" }, {
              cwd = key,
              text = true,
            })
            local ret = {
              ignored = parse_output(ignore_proc),
              tracked = parse_output(tracked_proc),
            }

            rawset(self, key, ret)
            return ret
          end,
        })
      end
      local git_status = new_git_status()

      -- Clear git status cache on refresh
      local refresh = require("oil.actions").refresh
      local orig_refresh = refresh.callback
      refresh.callback = function(...)
        git_status = new_git_status()
        orig_refresh(...)
      end
      local function git_hidden_file(name, bufnr)
        local dir = require("oil").get_current_dir(bufnr)
        local is_dotfile = vim.startswith(name, ".") and name ~= ".."
        -- if no local directory (e.g. for ssh connections), just hide dotfiles
        if not dir then
          return is_dotfile
        end
        if is_dotfile then
          return not git_status[dir].tracked[name]
        else
          return git_status[dir].ignored[name]
        end
      end
      ----

      require("oil").setup({
        default_file_explorer = true,
        columns = { "icon" },
        win_options = {
          signcolumn = "yes:2", -- For oil-git-status-nvim
        },
        view_options = {
          natural_order = "fast",
          is_hidden_file = git_hidden_file,
        },
      })
      local git_symbols = {
        [" "] = " ", -- Unmodified
        ["!"] = "", -- Ignored: nf-mdi-dots_horizontal
        ["?"] = "", -- Untracked: nf-fa-question_circle
        ["A"] = "", -- Added: nf-fa-plus
        ["C"] = "", -- Copied: nf-fa-copy
        ["D"] = "", -- Deleted: nf-fa-minus
        ["M"] = "", -- Modified: nf-mdi-circle_medium
        ["R"] = "", -- Renamed: nf-fa-arrow_right
        ["T"] = "", -- TypeChanged: nf-fa-info_circle
        ["U"] = "", -- Unmerged: nf-fa-exclamation_triangle
      }

      require('oil-git-status').setup({
        show_ignored = true,
        symbols = {
          index = git_symbols,
          working_tree = git_symbols,
        },
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
      np.vim-illuminate
      {
        plugin = np.nvim-treesitter;
        type = "lua";
        config = ''
          require('nvim-treesitter.configs').setup {
            highlight = { enable = true },
            indent = { enable = true },
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
    # plugins = [ np.nvim-treesitter-parsers.haskell ]; # Haskell treesitter appears to be broken...
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
    extraLuaConfig = ''
      vim.lsp.config('hls', {
        filetypes = { 'haskell', 'lhaskell', 'cabal' },
      })
      vim.lsp.enable('hls')
    '';
  };

  lang-nix.programs.neovim = {
    extraPackages = [ pkgs.nil ];
    plugins = [ np.nvim-treesitter-parsers.nix ];
    # formatters.nix.exe = "${pkgs.nixfmt-rfc-style}/bin/nixfmt";
    formatters.nix.exe = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
    extraLuaConfig = ''
      vim.lsp.enable('nil_ls')
    '';
  };

  lang-cpp.programs = {
    git.ignores = [ ".cache/clangd/" ];
    neovim = {
      plugins = [ np.nvim-treesitter-parsers.cpp ];
      formatters.cpp.exe = "${pkgs.clang-tools}/bin/clang-format";
      extraLuaConfig = ''
        vim.lsp.enable('clangd')
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
    extraLuaConfig = ''
      vim.lsp.enable('bashls')
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
      extraLuaConfig = ''
        vim.lsp.enable('pyright')
        vim.lsp.enable('ruff')
      '';
    };

  lang-typst.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.typst ];
    formatters.typst.exe = "${pkgs.typstyle}/bin/typstyle";
    extraConfig = ''
      autocmd BufNewFile,BufRead *.typ setfiletype typst
    '';
    extraLuaConfig = "vim.lsp.enable('tinymist')";
  };

  # lang-openscad.programs.neovim = {
  #   extraLuaConfig = ''
  #     vim.lsp.enable('openscad_lsp')
  #   '';
  # };

  lang-rust.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.rust ];
    formatters.rust = {
      exe = "rustfmt";
    };
    extraLuaConfig = ''
      vim.lsp.enable('rust_analyzer')
    '';
  };

  lang-lean.programs.neovim = {
    # No treesitter as of 2026-04-19
    plugins = [ np.lean-nvim ];
    extraLuaConfig = ''
      require('lean').setup{
        mappings = true,
      }
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
    extraLuaConfig = ''
      vim.lsp.config('cmake', {
        cmd = { "${pkgs.cmake-language-server}/bin/cmake-language-server" }
      })
      vim.lsp.enable('cmake')
    '';
  };

  lang-glsl.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.glsl ];
    extraLuaConfig = ''
      vim.lsp.config('glsl_analyzer', {
        cmd = { "${pkgs.glslls}/bin/glslls" }
      })
      vim.lsp.enable('glsl_analyzer')
    '';
  };

  lang-vimdoc.programs.neovim = {
    plugins = [ np.nvim-treesitter-parsers.vimdoc ];
  };

  spell.programs.neovim.extraConfig = ''
    set nowrap
    augroup filetype_text
      autocmd!
      autocmd FileType markdown,text,gitcommit setlocal wrap spell
    augroup END
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
    lang-lean
    lang-nix
    # lang-openscad
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
    extraLuaConfig = /* lua */ ''
      vim.keymap.set('n', '<leader>yf', ':let @@ = expand("%:.")<CR>', { desc = 'Yank file path' })
      vim.keymap.set('n', '<leader>yF', ':let @@ = expand("%:.") . ":" . line(".")<CR>', { desc = 'Yank file path with position' })
    '';
    extraConfig = /* vim */ ''
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
      np.nvim-treesitter-context

      {
        plugin = np.hop-nvim;
        type = "lua";
        config = /* lua */ ''

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
          let $FZF_DEFAULT_COMMAND = '${pkgs.ripgrep}/bin/rg --files --hidden --follow --glob "!.git/*"'
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
