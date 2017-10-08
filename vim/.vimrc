set nocompatible
set hidden
set encoding=utf-8
set fileencoding=utf-8

"Fish doesnt work well with vim, use bash
if &shell =~# 'fish$'
    set shell=bash
endif

"vim-plug
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-sensible'
Plug 'itchyny/lightline.vim'
Plug 'tomasr/molokai'
Plug 'junegunn/seoul256.vim'

Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'

Plug 'luochen1990/rainbow'
let g:rainbow_active = 1

"Editing
Plug 'terryma/vim-multiple-cursors'
Plug 'Yggdroot/indentLine'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'MicahElliott/vrod'
Plug 'bhurlow/vim-parinfer'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }

"languages
Plug 'dag/vim-fish'
Plug 'wlangstroth/vim-racket'
Plug 'fasiha/pollen.vim'

"linter
Plug 'vim-syntastic/syntastic'

call plug#end()

function! s:goyo_enter()
    set noshowmode
    set noshowcmd
    set scrolloff=999
    Limelight
endfunction

function! s:goyo_leave()
    set showmode
    set showcmd
    set scrolloff=5
    Limelight!
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

syntax on
filetype plugin indent on
colorscheme seoul256
set noshowmode
set number
set relativenumber
set path+=** " recursive completion

"Tab
set tabstop=8
set softtabstop=0
set expandtab " ^T->space
set shiftwidth=4
set smarttab

set list
set listchars=eol:¬,tab:>-,nbsp:⎵

set incsearch " Increment search, search as chars are entered

"Folding
set foldenable
set foldlevelstart=10
set foldnestmax=10
nnoremap <space> za
set foldmethod=indent

"Wrap
set whichwrap=b,s,<,>,[,]

"Mouse support
set mouse=a

"Switch buffers
nnoremap <C-l> :bn<CR>
nnoremap <C-h> :bp<CR>

"Language specific settings
augroup configgroup
    autocmd!
    autocmd VimEnter * highlight clear SignColumn
    autocmd FileType java setlocal noexpandtab
    autocmd FileType java setlocal list
    autocmd FileType java setlocal listchars=tab:+\ ,eol:-
    autocmd FileType java setlocal formatprg=par\ -w80\ -T4
    autocmd FileType php setlocal expandtab
    autocmd FileType php setlocal list
    autocmd FileType php setlocal listchars=tab:+\ ,eol:-
    autocmd FileType php setlocal formatprg=par\ -w80\ -T4
    autocmd FileType fish setlocal tabstop=4
    autocmd FileType fish setlocal softtabstop=4
    autocmd FileType fish setlocal textwidth=79
    autocmd FileType fish setlocal foldmethod=expr
    autocmd FileType fish compiler fish
    autocmd FileType ruby setlocal tabstop=2
    autocmd FileType ruby setlocal shiftwidth=2
    autocmd FileType ruby setlocal softtabstop=2
    autocmd FileType ruby setlocal commentstring=#\ %s
    autocmd FileType python setlocal commentstring=#\ %s
    autocmd BufEnter *.cls setlocal filetype=java
    autocmd BufEnter *.zsh-theme setlocal filetype=zsh
    autocmd BufEnter custom_phrases.txt setlocal noexpandtab
    autocmd BufEnter Makefile setlocal noexpandtab
    autocmd BufEnter markdown setlocal tabstop=2
    autocmd BufEnter markdown setlocal shiftwidth=2
    autocmd BufEnter markdown setlocal softtabstop=2
    autocmd BufEnter sh setlocal tabstop=2
    autocmd BufEnter sh setlocal shiftwidth=2
    autocmd BufEnter sh setlocal softtabstop=2
    autocmd BufEnter javascript setlocal tabstop=2
    autocmd BufEnter javascript setlocal shiftwidth=2
    autocmd BufEnter javascript setlocal softtabstop=2
augroup END

"Save file with sudo with :w!!
cmap w!! w !sudo tee > /dev/null %

"toggle gundo
nnoremap <leader>u :GundoToggle<CR>

"statusline
set laststatus=2
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:lightline = {
    \ 'colorscheme': 'seoul256',
    \ }

"syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_enable_racket_racket_checker = 1
