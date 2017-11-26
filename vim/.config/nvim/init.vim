set hidden
set encoding=utf-8
set fileencoding=utf-8

"Fish doesnt work well with vim, use bash
if &shell =~# 'fish$'
    set shell=bash
endif

"vim-plug
call plug#begin('~/.vim/plugged')

"Config
Plug 'tpope/vim-sensible'

"UI
Plug 'vim-airline/vim-airline'

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1

let g:airline#extensions#whitespace#enabled = 1

Plug 'vim-airline/vim-airline-themes'
let g:airline_theme = 'zenburn' "matches seoul256 somewhat

"Plug 'itchyny/lightline.vim'
"Plug 'ap/vim-buftabline'
"Plug 'Yggdroot/indentLine'

Plug 'tomasr/molokai'
Plug 'junegunn/seoul256.vim'
Plug 'junegunn/goyo.vim'
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

Plug 'junegunn/limelight.vim'
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' } "helm equivalent for vim

Plug 'luochen1990/rainbow'
let g:rainbow_active = 1

"Jumping around
if executable("ag")
    Plug 'mileszs/ack.vim'
    let g:ackprg = 'ag --vimgrep'
endif

if executable("fzf")
    Plug 'junegunn/fzf.vim'
endif

"Editing
Plug 'bhurlow/vim-parinfer'
Plug 'jiangmiao/auto-pairs'
let g:AutoPairs = {'(':')', '[':']', '{':'}',"'":"'",'"':'"', '`':'`', '「':'」'}
Plug 'tpope/vim-repeat'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1

"Motions, Objects
Plug 'tpope/vim-surround' "object: s. ex: c s ( '
Plug 'christoomey/vim-sort-motion' "verb: gs. ex: gs i (
Plug 'tpope/vim-commentary' "verb: gc, gcc

"Easymotion
Plug 'easymotion/vim-easymotion'
let g:EasyMotion_use_migemo = 1 "match kana with romaji

map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
let g:EasyMotion_startofline = 0 " keep cursor column when JK motion

"vim-textobj-user based
"a_ vs i_ is analogous to UTAU C-a vs C-w
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-datetime' "{a,i}d{a,f,d,t,z} [a]uto [f]ull [d]ate [t]ime [z]one
Plug 'kana/vim-textobj-entire' "object: ae, ie for entire file
Plug 'kana/vim-textobj-line' "object: al, il -> line
Plug 'kana/vim-textobj-indent' "object: {a,i}i {a,i}I similar or exact indent level
Plug 'whatyouhide/vim-textobj-xmlattr' "object: ax, ix for html/xml attrs

"filetypes
Plug 'sheerun/vim-polyglot' "lang pack
Plug 'MicahElliott/vrod'
Plug 'fasiha/pollen.vim'
Plug 'Firef0x/PKGBUILD.vim'

"linter
Plug 'vim-syntastic/syntastic'

"apps
Plug 'airblade/vim-gitgutter' "<leader>hs <leader>hp <leader>hu -> stage, preview, unstage
Plug 'tpope/vim-fugitive'
Plug 'mrtazz/simplenote.vim'
Plug 'EinfachToll/DidYouMean'

call plug#end()

" http://vim.wikia.com/wiki/Different_syntax_highlighting_within_regions_of_a_file
function! TextEnableCodeSnip(filetype,start,end,textSnipHl) abort
  let ft=toupper(a:filetype)
  let group='textGroup'.ft
  if exists('b:current_syntax')
    let s:current_syntax=b:current_syntax
    " Remove current syntax definition, as some syntax files (e.g. cpp.vim)
    " do nothing if b:current_syntax is defined.
    unlet b:current_syntax
  endif
  execute 'syntax include @'.group.' syntax/'.a:filetype.'.vim'
  try
    execute 'syntax include @'.group.' after/syntax/'.a:filetype.'.vim'
  catch
  endtry
  if exists('s:current_syntax')
    let b:current_syntax=s:current_syntax
  else
    unlet b:current_syntax
  endif
  execute 'syntax region textSnip'.ft.'
  \ matchgroup='.a:textSnipHl.'
  \ start="'.a:start.'" end="'.a:end.'"
  \ contains=@'.group
endfunction

let g:lightline = { 'colorscheme': 'seoul256', }
colorscheme seoul256
set noshowmode "mode is shown in statusline already, hide it
set number
set relativenumber
set path+=** " recursive completion
set wrap
set formatoptions+=mM

"Tab
set tabstop=8
set softtabstop=0
set expandtab " ^T->space
set shiftwidth=4

set list
set listchars=eol:¬,tab:>-,nbsp:⎵

"Folding
set foldenable
set foldlevelstart=10
set foldnestmax=10
nnoremap <tab> za
set foldmethod=indent

"Wrap
set whichwrap=b,s,<,>,[,]

"Mouse support
set mouse=a

"Keys
"Space -> leader
nnoremap <space> <nop>
let mapleader = "\<space>"

"leader based stuff
nnoremap <leader>c :noh<CR>
nnoremap <leader>eb :so %<CR>

"Switch buffers
nnoremap <M-l> :bn<CR>
nnoremap <M-h> :bp<CR>

"Terminal
tnoremap <Esc><Esc> <C-\><C-n>

"Language specific settings
augroup main
    autocmd!
    autocmd VimEnter * highlight clear SignColumn
augroup END

augroup java
    autocmd!
    autocmd FileType java setlocal noexpandtab
    autocmd FileType java setlocal list
    autocmd FileType java setlocal listchars=tab:+\ ,eol:-
    autocmd FileType java setlocal formatprg=par\ -w80\ -T4
augroup END

augroup php
    autocmd!
    autocmd FileType php setlocal expandtab
    autocmd FileType php setlocal list
    autocmd FileType php setlocal listchars=tab:+\ ,eol:-
    autocmd FileType php setlocal formatprg=par\ -w80\ -T4
augroup END

augroup fish
    autocmd!
    " https://stackoverflow.com/questions/6366049/vim-in-file-commands
    autocmd FileType sh call TextEnableCodeSnip('racket', '<<RKT', 'RKT', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('racket', '<<''RKT''', 'RKT', 'SpecialComment')

    autocmd FileType fish setlocal tabstop=4
    autocmd FileType fish setlocal softtabstop=4
    autocmd FileType fish setlocal textwidth=79
    autocmd FileType fish setlocal foldmethod=expr
    autocmd FileType fish compiler fish
augroup END

augroup ruby
    autocmd!
    autocmd FileType ruby setlocal tabstop=2
    autocmd FileType ruby setlocal shiftwidth=2
    autocmd FileType ruby setlocal softtabstop=2
    autocmd FileType ruby setlocal commentstring=#\ %s
augroup END

augroup python
    autocmd!
    autocmd FileType python setlocal commentstring=#\ %s
    autocmd BufEnter *.cls setlocal filetype=java
augroup END

augroup racket
    autocmd!
    autocmd FileType racket setlocal commentstring=;;\ %s
augroup END

augroup config
    autocmd!
    autocmd BufEnter *.zsh-theme setlocal filetype=zsh
    autocmd BufEnter custom_phrases.txt setlocal noexpandtab
    autocmd BufEnter Makefile setlocal noexpandtab
augroup END

augroup markdown
    autocmd!
    autocmd BufEnter markdown setlocal tabstop=2
    autocmd BufEnter markdown setlocal shiftwidth=2
    autocmd BufEnter markdown setlocal softtabstop=2
augroup END

augroup shell
    autocmd!
    autocmd BufEnter sh setlocal tabstop=2
    autocmd BufEnter sh setlocal shiftwidth=2
    autocmd BufEnter sh setlocal softtabstop=2
augroup END

augroup javascript
    autocmd!
    autocmd BufEnter javascript setlocal tabstop=2
    autocmd BufEnter javascript setlocal shiftwidth=2
    autocmd BufEnter javascript setlocal softtabstop=2
augroup END

augroup xml
    autocmd!
    autocmd FileType xml setlocal foldmethod=indent foldlevelstart=999 foldminlines=0
augroup END

"Save file with sudo with :w!!
cmap w!! w !sudo tee > /dev/null %

"statusline
if &statusline ==# ''
    set statusline+=%#warningmsg#
    set statusline+=%{SyntasticStatuslineFlag()}
    set statusline+=%*
endif

"syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_enable_racket_racket_checker = 1
let g:syntastic_sh_shellcheck_args = "-x"
