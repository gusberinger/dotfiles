" Plugins
call plug#begin('~/.vim/plugged')
Plug 'vim-syntastic/syntastic' | Plug 'kevinw/pyflakes-vim' " syntax errors for python
Plug 'tpope/vim-commentary' " better commenting
Plug 'tpope/vim-vinegar' " better file organizer
Plug 'justinmk/vim-sneak' " move around doc quickly
Plug 'itchyny/lightline.vim' " menu bar
call plug#end()



" Plugins
" Not anything but the essential shit:
" lightline: menu bar
" vinegar: file sorter that is actually useful
" sneak: move around document quickly

" Basic  Configuration
filetype plugin on
filetype indent on      " load filetype-specific indent files
filetype on
syntax on
set number
set mouse=a				" enable Mouse Support
set nocompatible
set showcmd             " show command in bottom bar
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to.
set ttyfast             " faster cursor movements
set showmatch           " highlight matching [{()}]
set incsearch           " search as characters are entered
set hlsearch            " highlight matches
set ic                  " ignore Case
set display=lastline    " show everything in wrapped text instead of @ symbol

" File Sorting
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+' " hides everything in netrw
let g:ctrlp_working_path_mode = '~/Desktop/Python' " limits Ctrl-P search to just directory

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

" Leader Mappings
let mapleader="\<space>"
nnoremap <leader><space> :nohl<cr>
nnoremap <leader>ev :vsp $MYVIMRC<cr>
nnoremap <leader>sv :so $MYVIMRC<cr>

" Vim Splits
set splitbelow " open split down and to the right
set splitright

" Theme
set background=light
colorscheme solarized
let g:lightline = {
\ 'colorscheme': 'solarized',
\ }
set laststatus=2 " always have lightline on

" Writing Specific Features
augroup writing
	au BufNewFile,BufRead *.markdown,*.mdown,*.mkd,*.mkdn,*.mdwn,*.md set ft=markdown
augroup END

" Python specific Features
augroup Python
	au FileType python set expandtab " enter spaces when tab is pressed
	au FileType python set colorcolumn=80
	au FileType python nmap <F9> :!python %<cr>
	au FileType python set tabstop=4           " use 4 spaces to represent tab
	set softtabstop=4		" TODO learn softtabstop
	set autoindent          " copy indent from current line when starting a new line
	set shiftwidth=4        " number of spaces to use for auto indent
augroup END
