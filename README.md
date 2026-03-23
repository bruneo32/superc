# SuperC documentation pages
[![Pages Status](https://img.shields.io/github/actions/workflow/status/bruneo32/superc/gh-pages.yml?branch=gh-pages)](https://github.com/bruneo32/superc/actions)

# Get Ruby
```sh
sudo apt install rbenv
rbenv install 3.1.2
rbenv global 3.1.2
eval "$(rbenv init -)" # ~/.bashrc
```

# Local build and test
```sh
./_script/bootstrap
bundle exec jekyll serve
```

### Enable playground in local build
```sh
mkdir -p _site/assets/jslinux/os/files
tar -xJf _jslinux_files.tar.xz -C _site/assets/jslinux/os/files
```

> Based on [Jekyll Minimal Theme](https://github.com/pages-themes/minimal)
> Note: **_jslinux_files.tar.xz** build with `tar -cJf _jslinux_files.tar.xz assets/jslinux/os/files/*`
