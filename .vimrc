set nu
set autoindent
set ts=4
set shiftwidth=4
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
