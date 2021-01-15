args@{ pkgs ? (import <nixpkgs> { }), ... }:
let
  unstable = args.unstable or (import <unstable> { });
  np = pkgs.vimPlugins;
  unp = unstable.vimPlugins;

in
{
  imports = [ ./vim-init.nix ];

  programs.git.ignores = [ "*~" "*.swp" "*.swo" "tags" "TAGS" ];

  home.packages = [
    # for coc
    pkgs.nodejs

    # formatters
    pkgs.nixpkgs-fmt
    unstable.haskellPackages.cabal-fmt
    pkgs.shfmt
    pkgs.uncrustify
    pkgs.clang-tools
  ];

  programs.neovim = {
    enable = true;
    vimAlias = true;
    vimdiffAlias = true;
    viAlias = true;
    init = {
      enable = true;
      preConfig = ''
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
        let maplocalleader = "\<space>"
        set tabstop=4
        set softtabstop=4
        set shiftwidth=4
        au BufNewFile,BufRead *.md  set spell
        nn <leader>w :w<CR>
        nn <leader>lo :lopen<CR>
        nn <leader>hl :nohl<CR>
      '';
      plugins = [
        np.sleuth
        np.surround
        np.commentary
        np.vim-indent-object
        np.vim-polyglot
        np.vim-repeat
        np.vim-unimpaired
        np.vim-eunuch
        np.vim-easymotion
      ];
      modules = gh: {

        airline = {
          plugins = [ np.vim-airline ];
          config = ''
            let g:airline_powerline_fonts = 1
            let g:airline#extensions#branch#displayed_head_limit = 10
          '';
        };

        easy-align = {
          plugins = [ np.vim-easy-align ];
          config = ''
            xmap <Enter> <Plug>(LiveEasyAlign)
          '';
        };
        sharedClipboard = {
          config = ''
            vmap <leader>y :w! /tmp/vitmp<CR>
            nmap <leader>p :r! cat /tmp/vitmp<CR>
          '';
        };

        escapeKeys = { config = "inoremap hj <esc>"; };

        emmet = {
          plugins = [ np.emmet-vim ];
          config = ''
            let g:user_emmet_install_global = 0
            autocmd FileType html,css EmmetInstall
          '';
        };

        git = {
          plugins = [ np.fugitive np.vim-gitgutter ];
          config = ''
            nn <leader>gs :Gstatus<CR>
            nn <leader>ga :GitGutterStageHunk<CR>
            nn <leader>gp :GitGutterPreviewHunk<CR>
            nn <leader>gu :GitGutterUndoHunk<CR>
            nn <leader>gn :GitGutterNextHunk<CR>
            nn <leader>gp :GitGutterPrevHunk<CR>
            set diffopt+=vertical
          '';
        };

        fzf = {
          plugins = [ np.fzf-vim np.fzfWrapper ];
          config = ''
            nn <leader>ff :Files<CR>
            nn <leader>fg :Ag<CR>
            nn <leader>ft :Tags<CR>
            nn <leader>fh :Helptags<CR>
            autocmd FileType haskell let g:fzf_tags_command = 'fast-tags -R'
            au BufWritePost *.hs silent! !${pkgs.haskellPackages.fast-tags}/bin/fast-tags -R . &
            let $FZF_DEFAULT_COMMAND = 'ag -g ""'
          '';
        };

        hoogle = {
          config = ''
            function! HoogleSearch()
             let searchterm = expand("<cword>")
             silent exec "silent !(firefox \"http://localhost:8080/?hoogle=" . searchterm . "\" > /dev/null 2>&1) &"
            endfunction
            autocmd FileType haskell nn K :call HoogleSearch()<CR><C-l>
          '';
        };

        neoformat = {
          # plugins = [ np.neoformat ];
          plugins = [ (gh "sbdchd/neoformat" { rev = "7e458dafae64b7f14f8c2eaecb886b7a85b8f66d"; }) ];
          config = ''
            let g:neoformat_basic_format_trim = 1
            augroup fmt
            autocmd!
            autocmd BufWritePre * silent Neoformat
            augroup END
            let g:neoformat_enabled_javascript = []
            nn <leader>fm :Neoformat<CR>
            nn <leader>fo :Neoformat ormolu<CR>
            nn <leader>fs :Neoformat stylishhaskell<CR>
          '';
        };

        nerdtree = {
          plugins = [ np.nerdtree ];
          config = "nn <C-n> :NERDTreeToggle<CR>";
        };

        snippets = {
          plugins = [ np.vim-snipmate np.vim-snippets ];
          config = ''
            imap <C-c> <Plug>snipMateNextOrTrigger
            imap <C-b> <Plug>snipMateBack
            set rtp+=~/nix/vim/snippets
          '';
        };

        colors = {
          plugins = [ np.nord-vim ];
          config = "colorscheme nord";
        };

        languageclient-neovim = {
          enable = false;
          plugins = [ unp.LanguageClient-neovim ];
          config = ''
            let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']
            let g:LanguageClient_serverCommands = { 'haskell': ['ghcide', '--lsp'] }
            let g:LanguageClient_floatingHoverHighlight = "Normal:NormalFloat"
            " let g:LanguageClient_hoverPreview = "Always"
            nnoremap <leader>lc :call LanguageClient_contextMenu()<CR>
            nnoremap <silent> <leader>lh :call LanguageClient#textDocument_hover()<CR>
            nnoremap <silent> <leader>ld :call LanguageClient#textDocument_definition()<CR>
            nnoremap <silent> <leader>lr :call LanguageClient#textDocument_rename()<CR>
          '';
        };

        # note; uses the ~/.config/nvim/coc-settings.json file.
        # ideally we'd have that declaratively, or just not use such a crappy system.
        # or emacs
        coc = {
          plugins = [ unp.coc-nvim unp.coc-python ];
          config = ''
            set hidden
            set nobackup
            set nowritebackup
            set cmdheight=2
            set updatetime=300
            nmap <silent> ga :CocAction<cr>
            nmap <silent> <leader>ga <Plug>(coc-fix-current)
            nmap <leader>lf :<C-u>CocList diagnostics<cr>
            nmap <silent> [g <Plug>(coc-diagnostic-prev)
            nmap <silent> ]g <Plug>(coc-diagnostic-next)
            nmap <silent> gd <Plug>(coc-definition)
            nmap <silent> gy <Plug>(coc-type-definition)
            nmap <silent> gi <Plug>(coc-implementation)
            nnoremap <silent> gh :call CocAction('doHover')<cr>
            " Highlight the symbol and its references when holding the cursor.
            autocmd CursorHold * silent call CocActionAsync('highlight')
          '';
        };

        scheme = {
          plugins = [
            (gh "guns/vim-sexp" {
              rev = "12292941903d9ac8151513189d2007e1ccfc95f0";
            })
            (gh "sjl/tslime.vim" {
              rev = "113b1f14a1fb92b9c026a71485c9ed402a2045f8";
            })
            (gh "tpope/vim-sexp-mappings-for-regular-people" {
              rev = "f5b66e73c81a3b4c0c8f5a8141fb09a5a213f711";
            })
          ];
          config = ''
            let g:tslime_ensure_trailing_newlines = 1
            let g:tslime_normal_mapping = '<leader>t'
            let g:tslime_visual_mapping = '<leader>t'
            let g:tslime_vars_mapping = '<leader>T'
            let g:sexp_mappings = { 'sexp_round_head_wrap_element': '<LocalLeader>r' ,
            \ 'sexp_round_tail_wrap_element': '<LocalLeader>R' } " avoid clashes with <Leader>{w,W}
          '';
        };
      };
    };
  };
}
