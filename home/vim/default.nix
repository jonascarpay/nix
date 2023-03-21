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
          nn <space>fs :Snippets<CR>
          set rtp+=${./.}
        '';
      }
    ];
  };

  workspace-symbols = {
    home.packages = [
      pkgs.bat
    ];
    programs.neovim.plugins = [
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

in
{
  imports = [
    snippets
    workspace-symbols
    lsp
  ];
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
              haskell = { exe = "ormolu"; stdin = false; args = [ "-i" ]; };
              cabal.exe = "${pkgs.haskellPackages.cabal-fmt}/bin/cabal-fmt";
              markdown.exe = "md-headerfmt";
              nix.exe = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
              go.exe = "gofmt";
              sh.exe = "${pkgs.shfmt}/bin/shfmt";
              proto.exe = "${pkgs.clang-tools}/bin/clang-format";
              lua = { exe = "${pkgs.luaformatter}/bin/lua-format"; stdin = false; args = [ "-i" ]; };
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
      np.cmp-buffer
      np.nvim-cmp
      np.lsp-status-nvim
      {
        plugin = np.nvim-lspconfig;
        config = ''
          autocmd BufEnter,CursorHold,InsertLeave <buffer> lua vim.lsp.codelens.refresh()
          " set completeopt=menu,menuone,noselect
          :luafile ${./lsp_config.lua}

          function! LspStatus() abort
            let status = luaeval("require('lsp-status').status()")
            return trim(status)
          endfunction
          call airline#parts#define_function('lsp_status', 'LspStatus')
          call airline#parts#define_condition('lsp_status', 'luaeval("#vim.lsp.buf_get_clients() > 0")')
          let g:airline#extensions#nvimlsp#enabled = 0
          let g:airline_section_warning = airline#section#create_right(['lsp_status'])
        '';
      }

    ];
  };
}
