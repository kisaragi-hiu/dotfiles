" == init settings ==
set hidden
set encoding=utf-8
set fileencoding=utf-8

" == notes ==
" set scrollbind: binds two views together. useful for eg. translation

" == plugins ==
call plug#begin('~/.vim/plugged')

" Behavior
Plug 'tpope/vim-sensible'
Plug 'EinfachToll/DidYouMean'
Plug 'tpope/vim-repeat'
Plug 'ConradIrwin/vim-bracketed-paste'

" UI
Plug 'vim-airline/vim-airline'
Plug 'jeffkreeftmeijer/vim-numbertoggle' " relativenumber off where it makes no sense

" Theme
Plug 'tomasr/molokai'
Plug 'skielbasa/vim-material-monokai'
Plug 'luochen1990/rainbow'
let g:rainbow_active = 1

"Editing
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'bhurlow/vim-parinfer'
Plug 'jiangmiao/auto-pairs'
Plug 'mbbill/undotree'
Plug 'osyo-manga/vim-over' " :s highlighting or :OverCommandLine; check :help over

"Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

"Motions, Objects
Plug 'tpope/vim-surround' "object: s. ex: c s ( '
Plug 'christoomey/vim-sort-motion' "verb: gs. ex: gs i (
Plug 'tpope/vim-commentary' "verb: gc, gcc

Plug 'junegunn/vim-easy-align'
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

"Easymotion
Plug 'easymotion/vim-easymotion' " <leader><leader>[motion]

"vim-textobj-user based
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-datetime' "{a,i}d{a,f,d,t,z} [a]uto [f]ull [d]ate [t]ime [z]one
Plug 'kana/vim-textobj-line' "object: al, il -> line
Plug 'kana/vim-textobj-indent' "object: {a,i}i {a,i}I similar or exact indent level
Plug 'whatyouhide/vim-textobj-xmlattr' "object: ax, ix for html/xml attrs
Plug 'rhysd/vim-textobj-word-column' "object: {a,i}{v,V}

Plug 'guns/vim-sexp' "object: {a,i}{fFse} for forms, toplevel forms, strings, and elements (s-exp)
" (): nearest bracket pair
" <M-{b,w}>: element-wise 'b'/'w' motion
" {g,}<M-e>: element-wise 'ge'/'e' motion (b/w but to the end of word)
" [[, ]]: adjacent toplevel element
" [e , ]e: select adjacent element
" ==: indent form
" =-: indent toplevel form
" more at github.com/guns/vim-sexp

"filetypes / languages
Plug 'sheerun/vim-polyglot' "lang pack
Plug 'MicahElliott/vrod' "Racket omnicompletion
Plug 'otherjoel/vim-pollen', { 'for': 'pollen' } " Requires vim-racket (provided by vim-polyglot)
Plug 'tpope/vim-speeddating' | Plug 'jceb/vim-orgmode'
Plug 'Firef0x/PKGBUILD.vim', { 'for': 'PKGBUILD' }
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'godlygeek/tabular'
Plug 'kovetskiy/sxhkd-vim'
Plug 'vim-scripts/gnuplot-syntax-highlighting'
Plug 'wilsaj/chuck.vim'
"linter
Plug 'vim-syntastic/syntastic'

"apps
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
" :term shortcuts
Plug 'DanySpin97/ttab.vim'
" <C-b>{n,p,c,x}: tab: next, previous, new buffer, close current
" <C-b>{t,|,-}: term: new in new tab, new in :vs, new in :split
" <C-b>{0..9}: move to nth tab
let g:ttab_prefix = '<C-b>'

if executable('wakatime')
    Plug 'wakatime/vim-wakatime'
endif

" git
Plug 'airblade/vim-gitgutter' "<leader>hs <leader>hp <leader>hu -> stage, preview, unstage
nnoremap <c-N> :GitGutterNextHunk<CR>
nnoremap <c-P> :GitGutterPrevHunk<CR>
nnoremap <c-U> :GitGutterUndoHunk<CR>
Plug 'tpope/vim-fugitive'
Plug 'jreybert/vimagit'

Plug 'mrtazz/simplenote.vim'
source ~/.simplenote.vim

call plug#end()

" == function definitions ==
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

" == settings ==
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#whitespace#enabled = 1
let g:AutoPairs = {'(':')', '[':']', '{':'}',"'":"'",'"':'"', '`':'`', '「':'」', '“':'”'}
set background=dark
set termguicolors
colorscheme molokai

set autochdir " cd to file path (like emacs evil)

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

" == keys ==
"Space -> leader
nnoremap <space> <nop>
let mapleader = " "

"leader based stuff
nnoremap <leader>c :noh<CR>
nnoremap <leader>eb :so %<CR>

"Switch buffers
nnoremap <leader>l :bn<CR>
nnoremap <leader>h :bp<CR>

"Terminal
tnoremap <leader><Esc> <C-\><C-n>

" highlight >120 char lines & trailing whitespaces
match ErrorMsg '\%>120v.\+'
match ErrorMsg '\s\+$'

"statusline
if &statusline ==# ''
    set statusline+=%#warningmsg#
    set statusline+=%{SyntasticStatuslineFlag()}
    set statusline+=%*
endif

"deoplete
let g:deoplete#enable_at_startup = 1

"syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_enable_racket_racket_checker = 1
let g:syntastic_sh_shellcheck_args = "-x"
" == augroups ==
augroup main
    autocmd!
    autocmd VimEnter * highlight clear SignColumn
augroup END

augroup web
    autocmd!
    autocmd FileType html call TextEnableCodeSnip('css', '<style>', '</style>', 'SpecialComment')
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
    autocmd FileType fish setlocal tabstop=4
    autocmd FileType fish setlocal softtabstop=4
    autocmd FileType fish setlocal textwidth=79
    autocmd FileType fish setlocal foldmethod=expr
    autocmd FileType fish compiler fish
augroup END

augroup pollen
    autocmd!
    autocmd FileType pollen setlocal commentstring=◊;\ %s
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
    " https://stackoverflow.com/questions/6366049/vim-in-file-commands
    autocmd FileType sh call TextEnableCodeSnip('racket', '<<RKT', 'RKT', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('racket', '<<''RKT''', 'RKT', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('c', '<<C', 'C', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('c', '<<''C''', 'C', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('javascript', '<<JS', 'JS', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('javascript', '<<''JS''', 'JS', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('json', '<<JSON', 'JSON', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('json', '<<''JSON''', 'JSON', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('python', '<<PYTHON', 'PYTHON', 'SpecialComment')
    autocmd FileType sh call TextEnableCodeSnip('python', '<<''PYTHON''', 'PYTHON', 'SpecialComment')
    autocmd BufEnter sh setlocal tabstop=2
    autocmd BufEnter sh setlocal shiftwidth=2
    autocmd BufEnter sh setlocal softtabstop=2
augroup END

augroup org
    autocmd!
    autocmd FileType org call TextEnableCodeSnip('sh', '#+BEGIN_SRC sh', '#+END_SRC', 'SpecialComment')
    autocmd FileType org call TextEnableCodeSnip('racket', '#+BEGIN_SRC racket', '#+END_SRC', 'SpecialComment')
    autocmd FileType org call TextEnableCodeSnip('clojure', '#+BEGIN_SRC clojure', '#+END_SRC', 'SpecialComment')
augroup END

augroup javascript
    autocmd!
    autocmd FileType html call TextEnableCodeSnip('javascript', '<script>', '</script>', 'SpecialComment')
    autocmd BufEnter javascript setlocal tabstop=2
    autocmd BufEnter javascript setlocal shiftwidth=2
    autocmd BufEnter javascript setlocal softtabstop=2
augroup END

augroup xml
    autocmd!
    autocmd FileType xml setlocal foldmethod=indent foldlevelstart=999 foldminlines=0
    autocmd FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null\ \|\ tail\ --lines=+2 " makes gg=G work
augroup END

