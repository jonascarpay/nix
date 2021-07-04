args @{ pkgs ? (import <nixpkgs> { }), config, ... }:
let
  unstable = args.unstable or config.channels.unstable;
  np = pkgs.vimPlugins;
  unp = unstable.vimPlugins;
in
{
  imports = [ ./init.nix ];

  programs.git.ignores = [ "*~" "*.swp" "*.swo" "tags" "TAGS" ];

  programs.neovim = {
    enable = true;
    vimAlias = true;
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
            let maplocalleader = "\<space>"
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
          '';
        };

        airline.config = ''
          let g:airline_powerline_fonts = 1
          let g:airline#extensions#branch#displayed_head_limit = 10
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
          plugins = [ unp.fzf-vim unp.fzfWrapper ];
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

        neoformat = {
          packages = [
            pkgs.nixpkgs-fmt
            unstable.haskellPackages.cabal-fmt
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
            let g:neoformat_enabled_javascript = []
            nn <leader>fm :Neoformat<CR>
            nn <leader>fo :Neoformat ormolu<CR>
            nn <leader>fs :Neoformat stylishhaskell<CR>
          '';
        };

        nerdtree.config = ''
          nn <C-n> :NERDTreeToggle<CR>
        '';

        snippets = {
          plugins = [
            np.vim-snipmate
            np.vim-snippets
          ];
          config = ''
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

        # note; uses the ~/.config/nvim/coc-settings.json file.
        # ideally we'd have that declaratively, or just not use such a crappy system.
        # or emacs
        coc = {
          plugins = [ unp.coc-nvim unp.coc-python ];
          packages = [ pkgs.nodejs ];
          config = ''
            set hidden
            set nobackup
            set nowritebackup
            set cmdheight=2
            set updatetime=300
            set shortmess+=c
            nmap <silent> <leader>la :CocAction<cr>
            nmap <silent> <leader>lr :CocRestart<cr>
            nmap <silent> <leader>ll :<C-u>CocList diagnostics<cr>
            nmap <silent> [l <Plug>(coc-diagnostic-prev)
            nmap <silent> ]l <Plug>(coc-diagnostic-next)
            nmap <silent> <leader>ld <Plug>(coc-definition)
            nmap <silent> <leader>ly <Plug>(coc-type-definition)
            nmap <silent> <leader>li <Plug>(coc-implementation)
            nnoremap <silent> <leader>lh :call CocAction('doHover')<cr>
            " Highlight the symbol and its references when holding the cursor.
            autocmd CursorHold * silent call CocActionAsync('highlight')
          '';
        };

      };
    };
  };
}
