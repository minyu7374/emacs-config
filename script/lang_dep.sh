#!/bin/bash
# set -x

os_name="$(grep '^NAME=' /etc/os-release | cut -d= -f2 | xargs)"
[ -z "$os_name" ] && { os_name="$(uname -a)"; }

OS=Linux
[[ "$os_name" =~ "Darwin" ]] && {
    OS=Mac
}

DISTRO=""
[[ "$os_name" =~ "Gentoo" ]] && {
    DISTRO=Gentoo
}
[[ "$os_name" =~ "Arch" ]] && {
    DISTRO=Arch
}
[[ "$os_name" =~ "Ubuntu" || "$os_name" =~ "Debian" ]] && {
    DISTRO=Debian
}
[[ "$os_name" =~ "Centos" || "$os_name" =~ "Fedora" || "$os_name" =~ "RHEL" || "$os_name" =~ "Oracle" ]] && {
    DISTRO=RHEL
}

function for_c() {
    # +lsp need one of clangd v9+ or ccls.
    # clangd 目前最稳定健壮，但是尚无索引系统, ccls 可搜索引用
    if [ "$OS" == "Mac" ]; then
        sudo port install llvm
    else 
        case "$DISTRO" in
            Gentoo)
                # USE extra  : Build extra tools (clangd, clang-tidy and a few more)
                sudo emerge --update clang
                sudo emerge --update ccls
                ;;
            Arch)
                sudo pacman -Sy --noconfirm clang
                sudo pacman -Sy --noconfirm ccls
                ;;
            Debian)     
                sudo apt-get install clangd-11
                sudo apt-get install ccls
                ;;
            RHEL)
                sudo dnf install clang-tools-extra
                sudo dnf install ccls
                ;;
        esac
    fi
    pip install cmake-language-server
}

function for_go() {
    go install golang.org/x/tools/gopls@latest
    go install github.com/x-motemen/gore/cmd/gore@latest
    go install github.com/stamblerre/gocode@latest
    go install golang.org/x/tools/cmd/godoc@latest
    go install golang.org/x/tools/cmd/goimports@latest
    go install golang.org/x/tools/cmd/gorename@latest
    go install golang.org/x/tools/cmd/guru@latest
    go install github.com/cweill/gotests/gotests@latest
    go install github.com/fatih/gomodifytags@latest
    
    if [ $OS == "Mac" ]; then
        sudo port install golangci-lint
    else
        # binary will be $(go env GOPATH)/bin/golangci-lint
        curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.54.1
        golangci-lint --version
    fi
}

function for_haskell() {
    if [ "$DISTRO" == "Gentoo" ]; then
        sudo emerge --update ghc haskell-language-server hoogle hlint #app-emacs/haskell-mode
    elif [ "$DISTRO" == "Arch" ]; then
        sudo pacman -Sy --noconfirm ghc haskell-language-server hoogle hlint
    fi

    ghcup install ghc
    # for +lsp
    ghcup install hls
    #cabal install haskell-mode

    cabal install hlint
    cabal install hoogle
}

function for_markdown() {
    ##Markdown-specific
    sudo npm install -g markdownlint --force
    # sudo gem install mdl

    ## General (natural language)
    pip install proselint
    sudo npm install -g textlint --force
    
    ## MarkedJS
    sudo npm install -g marked --force
    
    # pandoc/markdown
    if [ "$DISTRO" == "Gentoo" ]; then
        sudo emerge --update discount app-text/pandoc-bin
    elif [ "$DISTRO" == "Arch" ]; then
        sudo pacman -Sy --noconfirm discount pandoc-cli
    elif [ "$OS" == "Mac" ]; then
        sudo port install pandoc discount multimarkdown
    else
        echo "software to install: discount pandoc"
    fi
}

function for_python() {
    pip install pytest
    #pip install nose
    
    pip install black pyflakes isort
    pip install "python-language-server[all]"
    pip install "python-lsp-server[all]"
    sudo npm i -g pyright --force
}

function for_rust() {
    # for +lsp, need rust-analyzer
    if [ "$DISTRO" == "Gentoo" ]; then
        echo -e "USE config for rust:\n\t dev-lang/rust clippy rust-analyzer rustfmt rust-src"
    elif [ "$DISTRO" == "Arch" ]; then
        sudo pacman -Sy --noconfirm rust rust-src rust-analyzer
    fi

    # rustup component add rustfmt-preview
    # rustup component add clippy-preview
    cargo install cargo-check
}

function for_shell() {
    if [ "$DISTRO" == "Gentoo" ]; then
        sudo emerge --update shellcheck
    elif [ "$DISTRO" == "Arch" ]; then
        sudo pacman -Sy --noconfirm shellcheck
    else
        cabal install ShellCheck --overwrite-policy=always
        # stack install ShellCheck
    fi

    sudo npm i -g bash-language-server --force
}

function for_json() {
    # sudo npm i -g vscode-langservers-extracted --force
    sudo npm install -g vscode-json-languageserver --force
}

function for_docker() {
    sudo npm install -g dockerfile-language-server-nodejs
}

if [ -z "$1" ]; then
    for_c
    for_go
    for_python
    for_haskell
    for_markdown
    for_rust
    for_shell
    for_json
    for_docker
else
    eval "for_$1"
fi
