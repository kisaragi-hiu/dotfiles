set nocompatible

"Fish doesnt work well with vim, use bash
if &shell =~# 'fish$'
    set shell=bash
endif

"vim-plug
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-sensible'
Plug 'bhurlow/vim-parinfer'
Plug 'wlangstroth/vim-racket'
Plug 'fasiha/pollen.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

syntax on
filetype plugin indent on
set number
set lazyredraw " Don't redraw when not needed

"Tab
set tabstop=4
set softtabstop=4
set expandtab " <tab>->space

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
        autocmd FileType ruby setlocal tabstop=2
        autocmd FileType ruby setlocal shiftwidth=2
        autocmd FileType ruby setlocal softtabstop=2
        autocmd FileType ruby setlocal commentstring=#\ %s
        autocmd FileType python setlocal commentstring=#\ %s
        autocmd BufEnter *.cls setlocal filetype=java
        autocmd BufEnter *.zsh-theme setlocal filetype=zsh
        autocmd BufEnter Makefile setlocal noexpandtab
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

"Airline
set laststatus=2
let g:airline_powerline_fonts = 1
