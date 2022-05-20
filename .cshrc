# $FreeBSD: releng/12.0/share/skel/dot.cshrc 337497 2018-08-08 19:24:20Z asomers $
#
# .cshrc - csh resource script, read at beginning of execution by each shell
#
# see also csh(1), environ(7).
# more examples available at /usr/share/examples/csh/
#

alias h		history 25
alias j		jobs -l
alias la	ls -aF
alias lf	ls -FA
alias ll	ls -lAF

# These are normally set through /etc/login.conf.  You may override them here
# if wanted.
set path = (/sbin /bin /usr/sbin /usr/bin /usr/local/sbin /usr/local/bin $HOME/bin)
# setenv	BLOCKSIZE	K
# A righteous umask
# umask 22

setenv	EDITOR	vim
setenv	PAGER	less

if ($?prompt) then
	# An interactive shell -- set some stuff up
	set prompt = "%N@%m:%~ \n%# "
	set promptchars = "%#"

	set filec
	set history = 1000
	set savehist = (1000 merge)
	set autolist = ambiguous
	# Use history to aid expansion
	set autoexpand
	set autorehash
	set mail = (/var/mail/$USER)
	if ( $?tcsh ) then
		bindkey "^W" backward-delete-word
		bindkey -k up history-search-backward
		bindkey -k down history-search-forward
	endif

endif

setenv LANG ko_KR.UTF-8
setenv XIM ibus
setenv GTK_IM_MODULE ibus
setenv QT_IM_MODULE xim
setenv XMODIFIERS @im=ibus
setenv XIM_PROGRAM ibus-daemon
setenv XIM_ARGS "-r --daemonize --xim"