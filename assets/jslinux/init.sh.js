var _jslinux_init_sh_str =
	"stty intr undef" + "\n" +
	"clear" + "\n" +
	"echo \"Initializing...\"" + "\n" +
	"export HISTCONTROL=ignoreboth" + "\n" +
	"cd /root/" + "\n" +
	"rm -f *" + "\n" +
	"alias l='ls -CF'" + "\n" +
	"alias la='ls -A'" + "\n" +
	"alias ll='ls -hAlF'" + "\n" +
	"export PS1='\\[\\e[91m\\]\\h\\[\\e[0m\\]:\\w\\$ '" + "\n" +
	"echo \"Waiting for connection...\"" + "\n" +
	"while ! ping -c 1 github.com >/dev/null 2>&1; do" + "\n" +
	"  sleep 0.2" + "\n" +
	"done" + "\n" +
	"echo \"Getting SuperC compiler...\"" + "\n" +
	"wget http://github.com/bruneo32/superc/raw/refs/heads/gh-pages/_bin/superc_jslinux.tar.xz \\" + "\n" +
	"       -O /root/superc_jslinux.tar.xz && \\" + "\n" +
	"  tar xJf /root/superc_jslinux.tar.xz -C / && \\" + "\n" +
	"  rm /root/superc_jslinux.tar.xz && \\" + "\n" +
	"  alias superc='superc -include /usr/local/include/stdarg.h '" + "\n" +
	"stty intr ^C" + "\n"
	"clear" + "\n"
