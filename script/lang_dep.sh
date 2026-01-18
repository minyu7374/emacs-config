#!/usr/bin/env bash
# set -x

case "$(uname -s)" in
Linux)
    OS=Linux
    os_name="$(grep '^NAME=' /etc/os-release | cut -d= -f2 | xargs)"
    [ -z "$os_name" ] && { os_name="$(uname -a)"; }
    ;;
Darwin)
    OS=Mac
    ;;
esac

if [ "$OS" == Linux ]; then
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
    [[ "$os_name" =~ "openSUSE" ]] && {
        DISTRO=SUSE
    }
fi

if [ "$OS" == Mac ]; then
    which port 2>/dev/null && DISTRO="MacPorts"
    which brew 2>/dev/null && DISTRO="Homebrew"
fi

function pre_task() {
    cabal update
    pip install --upgrade pip

    npm_cmd=npm
    which cnpm >/dev/null && npm_cmd=cnpm
}

function for_treesit() {
    case "$DISTRO" in
    Gentoo)
        sudo emerge --update dev-util/tree-sitter-cli
        ;;
    Arch)
        sudo pacman -Sy --noconfirm tree-sitter
        ;;
    SUSE)
        sudo zypper in -y tree-sitter
        ;;
    MacPorts)
        sudo port install tree-sitter-cli
        ;;
    Homebrew)
        brew install tree-sitter-cli
        ;;
    *)
        # The command "cabal install [TARGETS]" doesn't expose libraries.
        cabal install --lib tree-sitter
        ;;
    esac
}

function for_c() {
    # +lsp need one of clangd v9+ or ccls.
    # clangd 目前最稳定健壮，但是尚无索引系统, ccls 可搜索引用
    case "$DISTRO" in
    Gentoo)
        # USE extra  : Build extra tools (clangd, clang-tidy and a few more)
        sudo emerge --update llvm-core/clang
        sudo emerge --update ccls
        ;;
    Arch)
        sudo pacman -Sy --noconfirm clang
        sudo pacman -Sy --noconfirm ccls
        ;;
    SUSE)
        sudo zypper in -y clang
        sudo zypper in -y ccls
        ;;
    Debian)
        sudo apt-get install clangd-11
        sudo apt-get install ccls
        ;;
    RHEL)
        sudo dnf install clang-tools-extra
        sudo dnf install ccls
        ;;
    MacPorts)
        sudo port install tree-sitter-cli
        sudo port install llvm ccls
        ;;
    Homebrew)
        brew install tree-sitter-cli
        brew install llvm ccls
        ;;
    esac
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

    case "$DISTRO" in
    MacPorts)
        sudo port install golangci-lint
        ;;
    Homebrew)
        brew install golangci-lint
        ;;
    *)
        # binary will be $(go env GOPATH)/bin/golangci-lint
        curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh |
            sh -s -- -b "$(go env GOPATH)/bin" #v1.54.1
        golangci-lint --version
        ;;
    esac
}

function for_haskell() {
    case "$DISTRO" in
    Gentoo)
        sudo emerge --update ghc haskell-language-server hoogle hlint #app-emacs/haskell-mode
        ;;
    Arch)
        sudo pacman -Sy --noconfirm ghc haskell-language-server hoogle hlint cabal-install
        ;;
    SUSE)
        sudo zypper in -y ghc ghc-hlint cabal-install
        ;;
    *)
        # ghcup install ghc hls
        cabal install haskell-language-server hlint hoogle
        #cabal install haskell-mode
        ;;
    esac
}

function for_markdown() {
    ##Markdown-specific
    sudo $npm_cmd install -g markdownlint --force
    # sudo gem install mdl

    ## General (natural language)
    pip install proselint
    sudo $npm_cmd install -g textlint --force

    ## MarkedJS
    sudo $npm_cmd install -g marked --force

    ## latex math preview
    sudo $npm_cmd install -g git+https://gitlab.com/matsievskiysv/math-preview

    # pandoc/markdown
    case "$DISTRO" in
    Gentoo)
        sudo emerge --update discount app-text/pandoc-bin
        ;;
    Arch)
        sudo pacman -Sy --noconfirm discount pandoc-cli
        ;;
    SUSE)
        sudo zypper in -y discount pandoc
        # sudo zypper in -y MultiMarkdown-6
        ;;
    MacPorts)
        sudo port install pandoc discount multimarkdown
        ;;
    Homebrew)
        brew install pandoc discount
        ;;
    *)
        echo "software to install: discount pandoc"
        ;;
    esac
}

function for_python() {
    #pip install nose
    pip install pytest

    pip install autopep8 pylint yapf
    pip install black isort
    pip install pipenv poetry
    pip install "python-language-server[all]"
    pip install "python-lsp-server[all]"
    #pip install pyright
    sudo $npm_cmd i -g pyright --force
    uv tool install ruff@latest
}

function for_rust() {
    # for +lsp, need rust-analyzer
    case "$DISTRO" in
    Gentoo)
        echo -e "USE config for rust:\n\t dev-lang/rust clippy rust-analyzer rustfmt rust-src"
        ;;
    Arch)
        sudo pacman -Sy --noconfirm rust rust-src rust-analyzer
        ;;
    SUSE)
        sudo zypper in -y rust cargo
        ;;
    esac

    # rustup component add rustfmt-preview
    # rustup component add clippy-preview
    cargo install cargo-check
}

function for_shell() {
    case "$DISTRO" in
    Gentoo)
        sudo emerge --update shellcheck
        ;;
    Arch)
        sudo pacman -Sy --noconfirm shellcheck
        ;;
    SUSE)
        sudo zypper in -y ShellCheck
        ;;
    MacPorts)
        sudo port install shellcheck
        ;;
    Homebrew)
        brew install shellcheck
        ;;
    *)
        cabal install ShellCheck --overwrite-policy=always
        # stack install ShellCheck
        ;;
    esac

    sudo $npm_cmd i -g bash-language-server --force
    go install mvdan.cc/sh/v3/cmd/shfmt@latest
}

function for_web() {
    case "$DISTRO" in
    Gentoo)
        sudo emerge --update app-text/htmltidy
        ;;
    Arch)
        sudo pacman -Sy --noconfirm tidy
        ;;
    esac
}

function for_graph() {
    case "$DISTRO" in
    Gentoo)
        sudo emerge --update media-gfx/graphviz
        ;;
    Arch)
        sudo pacman -Sy --noconfirm graphviz
        ;;
    MacPorts)
        sudo port install graphviz
        ;;
    Homebrew)
        brew install graphviz
        ;;
    esac

    go install oss.terrastruct.com/d2@latest
    sudo $npm_cmd install -g @mermaid-js/mermaid-cli
}

function for_json() {
    # sudo $npm_cmd i -g vscode-langservers-extracted --force
    sudo $npm_cmd install -g vscode-json-languageserver --force
}

function for_docker() {
    if [ "$OS" == "Linux" ]; then
        sudo $npm_cmd install -g dockerfile-language-server-nodejs
    fi

    go install github.com/jessfraz/dockfmt@latest
}

pre_task

[ -z "$*" ] &&
    # grep -oP '(?<=for_).*(?=\(\) \{)' ./script/lang_dep.sh | xargs
    set -- treesit c go haskell markdown python rust shell web graph json docker

for lang in "$@"; do
    eval "for_$lang"
done
