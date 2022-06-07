{ pkgs, config, ... }:
let
  np = pkgs.vimPlugins;
in
{
  imports = [ ./init.nix ];

  programs.git.ignores = [ "*~" "*.swp" "*.swo" "tags" "TAGS" ];

  xdg.configFile."nvim/coc-settings.json".text = builtins.readFile ./coc-settings.json;
  home.packages = [ pkgs.nodejs ];

  programs.neovim = {
    enable = true;
    vimAlias = true;
    package = pkgs.neovim-unwrapped;
    vimdiffAlias = true;
    viAlias = true;
    init = {
      enable = true;
      modules = {

        preamble = {
          plugins = [
            np.commentary
            np.surround
            np.vim-easymotion
            np.vim-eunuch
            np.vim-indent-object
            np.vim-polyglot
            np.vim-repeat
            np.vim-unimpaired
          ];
          precedence = 5;
          config = ''
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
          '';
        };

        airline.config = ''
          let g:airline_powerline_fonts = 1
          let g:airline#extensions#branch#displayed_head_limit = 10
        '';

        vim-localvimrc.config = ''
          let g:localvimrc_persistent = 1
        '';

        vim-easy-align.config = ''
          xmap <Enter> <Plug>(LiveEasyAlign)
        '';

        emmet-vim.config = ''
          let g:user_emmet_install_global = 0
          autocmd FileType html,css EmmetInstall
        '';

        vim-mundo.config = ''
          nnoremap <leader>u :MundoToggle<CR>
        '';

        git = {
          plugins = [ np.fugitive np.vim-gitgutter ];
          config = ''
            nn <leader>gs :Git<CR>
            nn <leader>gg :Git<CR>
            nn <leader>ga :GitGutterStageHunk<CR>
            nn <leader>gp :GitGutterPreviewHunk<CR>
            nn <leader>gu :GitGutterUndoHunk<CR>
            nn <leader>gn :GitGutterNextHunk<CR>
            nn <leader>gp :GitGutterPrevHunk<CR>
            set diffopt+=vertical
          '';
        };

        fzf = {
          plugins = [ np.fzf-vim ];
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
        };

        vim-highlightedyank.config = ''
          let g:highlightedyank_highlight_duration = 200
        '';

        hoogle = {
          plugins = [ ];
          config = ''
            function! HoogleSearch()
             let searchterm = expand("<cword>")
             silent exec "silent !(firefox \"http://localhost:8080/?hoogle=" . searchterm . "\" > /dev/null 2>&1) &"
            endfunction
            autocmd FileType haskell nn K :call HoogleSearch()<CR><C-l>
          '';
        };

        indent-blankline-nvim.plugins = [ np.indent-blankline-nvim ];
        indent-blankline-nvim.config = ''
          let g:indent_blankline_char = '▏'
          nn <leader>ii :IndentBlanklineToggle<CR>
        '';

        neoformat = {
          packages = [
            pkgs.nixpkgs-fmt
            pkgs.haskellPackages.cabal-fmt
            pkgs.shfmt
            pkgs.uncrustify
            pkgs.clang-tools
          ];
          config = ''
            let g:neoformat_basic_format_trim = 1
            augroup fmt
              autocmd!
              autocmd BufWritePre * silent Neoformat
            augroup END
            let g:neoformat_enabled_haskell = ['ormolu']
          '';
        };

        nerdtree.config = ''
          nn <C-n> :NERDTreeToggle<CR>
        '';

        snippets = {
          plugins = [
            np.UltiSnips
            np.vim-snippets
          ];
          config = ''
            nn <leader>fs :Snippets<CR>
            set rtp+=${./.}
          '';
        };

        auto-pairs.config =
          let doubleSingle = "''"; in
          ''
            autocmd FileType nix let b:AutoPairs = AutoPairsDefine({"${doubleSingle}" : "${doubleSingle}", '=':';'}, ["'"])
          '';

        colors = {
          plugins = [ np.nord-vim ];
          config = "colorscheme nord";
        };

        coc = {
          plugins = [
            np.coc-nvim
            np.coc-pyright
          ];
          config = ''
            set hidden
            set nobackup
            set nowritebackup
            set cmdheight=2
            set updatetime=300
            set shortmess+=c
            nmap <silent> [l <Plug>(coc-diagnostic-prev)
            nmap <silent> ]l <Plug>(coc-diagnostic-next)
            nnoremap <silent> <leader>lh :call CocActionAsync('doHover')<cr>
            nmap <leader>la <Plug>(coc-codeaction)
            nmap <leader>ll <Plug>(coc-codelens-action)
          '';
        };

        nvim-lsp.enable = false;
        nvim-lsp = {
          plugins = [ np.nvim-lspconfig ];
          config = ''
            lua << EOF
              require'lspconfig'.pyright.setup{}
              require'lspconfig'.hls.setup{
                -- overrides 'haskell-language-server-wrapper'
                cmd = { 'haskell-language-server', '--lsp' }, 
              }
              require'lspconfig'.rnix.setup{
                cmd = { '${pkgs.rnix-lsp}/bin/rnix-lsp' },
              }
              require'lspconfig'.clangd.setup{
                cmd = { "${pkgs.clang-tools}/bin/clangd", "--background-index" }
              }
              require'lspconfig'.tsserver.setup{}
              require'lspconfig'.rust_analyzer.setup{}
            EOF
            nn <leader>la <cmd> lua vim.lsp.buf.code_action()<cr>
            nn <leader>ld <cmd> lua vim.lsp.buf.definition()<cr>
            nn <leader>lD <cmd> lua vim.lsp.buf.declaration()<cr>
            nn <leader>lr <cmd> lua vim.lsp.buf.rename()<cr>
            nn <leader>lh <cmd> lua vim.lsp.buf.hover()<cr>
            nn <leader>lk <cmd> lua vim.lsp.buf.signature_help()<cr>
            nn [l <cmd> lua vim.lsp.diagnostic.goto_prev()<cr>
            nn ]l <cmd> lua vim.lsp.diagnostic.goto_next()<cr>
          '';
        };

      };
    };
  };
}
