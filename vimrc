" Fish doesnt work well with vim, use bash
if &shell =~# 'fish$'
    set shell=bash
endif

" Pathogen
execute pathogen#infect()

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
        autocmd BufEnter *.sh setlocal tabstop=2
        autocmd BufEnter *.sh setlocal shiftwidth=2
        autocmd BufEnter *.sh setlocal softtabstop=2
augroup END

"Solarized
"set background=dark
"colorscheme solarized

"Save file with sudo with :w!!
cmap w!! w !sudo tee > /dev/null %

"toggle gundo
nnoremap <leader>u :GundoToggle<CR>

"Powerline
set laststatus=2
set t_Co=256
" `powerline-vim` package on manjaro set it up for me...
let g:powerline_pycmd = "py3"
"python from powerline.vim import setup as powerline_setup
"python powerline_setup()
"python del powerline_setup
