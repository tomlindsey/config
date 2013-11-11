" Tabs and indenting.
set smarttab
set shiftwidth=4
set tabstop=4
set expandtab
set smartindent
"set autoindent

" I HATE the beeping!!!
set visualbell

" Status Line
set statusline=%f\ %r\ %m%=%-14(Char:\ %b/0x%B%)\ \ %-10(Col:\ %c%)\ \ %17(Line:\ %l/%L%)
set laststatus=2  " Always show the status line.

" Searching
set incsearch
set hlsearch
map <F6> :nohlsearch<Enter>:<Esc>  " Clear the highlights.
" I want a mapping or function to only search for a complete word - "/\<word\>"

" Allow backspace to remove characters before the cursor, including an indent or end of line.
set backspace=start,indent,eol

" Correct some common spelling errors.
abbreviate teh the

" Scrolling
set scrolloff=5

" Colors
syntax enable
hi Comment ctermfg=Green  " All comments should be green

" Matching
set showmatch    " show matching brackets
set matchtime=4  " for this many 10ths of a second
