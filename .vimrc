set nu
set autoindent
set ts=2
set shiftwidth=2
au BufReadPost *
			\ if line ("'\"") > 0 && line ("'\"") <= line ("$") | 
			\ exe "norm g'\"" |
			\ endif
set laststatus=2
set showmatch
set wmnu
set title
set incsearch
syntax on
filetype indent on
