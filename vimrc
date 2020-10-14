""" Plugins
call plug#begin('~/.vim/plugged')
" Plug 'vim-syntastic/syntastic' " syntax errors for python
Plug 'chriskempson/base16-vim'
Plug 'tpope/vim-commentary' " better commenting
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar' " better file organizer
Plug 'tpope/vim-fugitive' " git wrapper
Plug 'airblade/vim-gitgutter' " git changes in gutter
Plug 'itchyny/lightline.vim' " menu bar
Plug 'darfink/vim-plist' " plist support
Plug 'lervag/vimtex' " latex support
Plug 'junegunn/goyo.vim' " writing
Plug 'jceb/vim-orgmode' " org-mode
Plug 'vim-scripts/SyntaxRange' " highlight codeblocks
Plug 'tpope/vim-speeddating' " needed for orgmode
Plug 'honza/vim-snippets/'
Plug 'JuliaEditorSupport/julia-vim'
call plug#end()

""" Basic  Configuration
filetype plugin on
filetype indent on
filetype on
autocmd filetype crontab setlocal nobackup nowritebackup
syntax on
set number
set mouse=a		" enable Mouse Support
set nocompatible
set showcmd             " show command in bottom bar
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to.
set ttyfast             " faster cursor movements
set ttimeout            " faster speed
set ttimeoutlen=250
set notimeout
set showmatch           " highlight matching [{()}]
set incsearch           " search as characters are entered
set hlsearch            " highlight matches
set ic                  " ignore Case
set display=lastline    " show everything in wrapped text instead of @ symbol
set linebreak		" soft textwrap

""" File Sorting
let g:netrw_list_hide = '.*\.swp$,.DS_Store,.*\.pyc,.*\.git/' " 
" let g:ctrlp_working_path_mode = '~/Desktop/Python' " limits Ctrl-P search to just directory

" Key Mappings & Notes
" map foo boo; does boo when press foo
" imap, nmap, vmap, map when in insert/normal/visual
" *noremap = nonrecursive; use instead of *map

command! W write " allows :W
nnoremap j gj|" navigate wrapped text
nnoremap k gk
nnoremap J jzz|" centered scrolling
nnoremap K kzz
inoremap jk <Esc>|" easy escapes
nnoremap yyy :%w !pbcopy<cr><cr>|" yank entire document

""" Leader Mappings
let mapleader="\<space>"
let maplocalleader="\<space>m"
nnoremap <leader><space> :nohl<cr>
nnoremap <leader>ev :vsp $MYVIMRC<cr>
nnoremap <leader>sv :so $MYVIMRC<cr>

""" Vim Splits
set splitbelow " open split down and to the right
set splitright

""" Theme
set t_Co=256
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head'
      \ },
      \ }

set laststatus=2 " always have lightline on

""" Latex
let g:vimtex_view_method = 'skim'
let g:vimtex_fold_enabled = 1
let g:vimtex_echo_ignore_wait = 1
let g:vimtex_quickfix_open_on_warning = 0
let g:syntastic_tex_chktex_showmsgs = 0
let g:tex_flavor = 'tex'
nnoremap <tab> za |" toggle fold


""" Snips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"


""" Writing Specific Features
function! s:goyo_leave()
    set background=dark
    set showcmd
    GitGutterBufferEnable
endfunction
autocmd! User GoyoLeave nested call <SID>goyo_leave()

function! s:goyo_enter()
  set noshowcmd
  GitGutterBufferDisable
  " ...
endfunction

augroup writing
    au BufNewFile,BufRead *.markdown,*.mdown,*.mkd,*.mkdn,*.mdwn,*.md set ft=markdown
    au FileType markdown,tex setlocal spell 
augroup END

""" Python specific Features
augroup Python
    au FileType python set expandtab " enter spaces when tab is pressed
    " au FileType python set colorcolumn=80
    au FileType python nmap <F9> :!python3 %<cr>
    au FileType python set tabstop=4           " use 4 spaces to represent tab
    set softtabstop=4		" TODO learn softtabstop
    set autoindent          " copy indent from current line when starting a new line
    set shiftwidth=4        " number of spaces to use for auto indent
augroup END

""" Desktop Features
set guioptions= " remove the scrollbars
set noerrorbells " remove the bell
set novisualbell
let &t_SI.="\e[5 q"
let &t_SR.="\e[4 q"
let &t_EI.="\e[1 q"


""" Copy / Paste Mode
nnoremap <F10> :setlocal paste!<cr>
