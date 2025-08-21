#!/bin/bash
# set -x

typeset -A PARSER_ARGS=(
    [bash]="tree-sitter/tree-sitter-bash"
    [c]="tree-sitter/tree-sitter-c"
    [cpp]="tree-sitter/tree-sitter-cpp"
    [css]="tree-sitter/tree-sitter-css"
    [cmake]="uyha/tree-sitter-cmake"
    [csharp]="tree-sitter/tree-sitter-c-sharp"
    [dockerfile]="camdencheek/tree-sitter-dockerfile"
    [elisp]="Wilfred/tree-sitter-elisp"
    [go]="tree-sitter/tree-sitter-go"
    [gomod]="camdencheek/tree-sitter-go-mod"
    [html]="tree-sitter/tree-sitter-html"
    [java]="tree-sitter/tree-sitter-java"
    [javascript]="tree-sitter/tree-sitter-javascript"
    [json]="tree-sitter/tree-sitter-json"
    [lua]="Azganoth/tree-sitter-lua"
    [make]="alemuller/tree-sitter-make"
    [markdown]="MDeiml/tree-sitter-markdown tree-sitter-markdown"
    [markdown-inline]="MDeiml/tree-sitter-markdown tree-sitter-markdown-inline"
    [ocaml]="tree-sitter/tree-sitter-ocaml ocaml"
    [org]="milisims/tree-sitter-org"
    [haskell]="tree-sitter/tree-sitter-haskell"
    [python]="tree-sitter/tree-sitter-python"
    [php]="tree-sitter/tree-sitter-php"
    [typescript]="tree-sitter/tree-sitter-typescript typescript"
    [tsx]="tree-sitter/tree-sitter-typescript tsx"
    [ruby]="tree-sitter/tree-sitter-ruby"
    [rust]="tree-sitter/tree-sitter-rust"
    [sql]="m-novikov/tree-sitter-sql"
    [vue]="merico-dev/tree-sitter-vue"
    [yaml]="ikatyang/tree-sitter-yaml"
    [toml]="tree-sitter/tree-sitter-toml"
    [zig]="GrayJack/tree-sitter-zig"
)

LOCAL_TREESIT_LIB_DIR="$HOME/.local/lib/treesit"
EMACS_TREESIT_LIB_DIR="$HOME/.config/emacs/.local/cache/tree-sitter"
TREESIT_PARSER_CODE_DIR="/tmp/treesit_parser_$(uuidgen)"

mkdir -p "$LOCAL_TREESIT_LIB_DIR"
ln -sfn "$LOCAL_TREESIT_LIB_DIR" "$EMACS_TREESIT_LIB_DIR"
mkdir -p "$TREESIT_PARSER_CODE_DIR"
trap 'rm -rf "$TREESIT_PARSER_CODE_DIR"' EXIT

[[ "$(uname -s)" = "Darwin" ]] && so_suffix="dylib" || so_suffix="so"

function install_parser() {
    lang="$1"
    repo="$2"
    parser_dir="$3"
    code_dir="$TREESIT_PARSER_CODE_DIR/$lang"
    so="libtree-sitter-$lang.$so_suffix"

    git clone git@github.com:"$repo".git "$code_dir" &&
        cd "$code_dir/$parser_dir" || return 1
    [[ "$lang" = "toml" ]] && npm install regexp-util

    tree-sitter generate
    srcs=("src/parser.c")
    [[ -f "src/scanner.c" ]] && srcs+=("src/scanner.c")
    [[ -f "src/scanner.cc" ]] && srcs+=("src/scanner.cc")

    if [[ " ${srcs[*]} " == *".cc"* ]]; then
        g++ -fPIC -O2 -shared -o "$so" "${srcs[@]}"
    else
        gcc -fPIC -O2 -shared -o "$so" "${srcs[@]}"
    fi
    mv "$so" "$LOCAL_TREESIT_LIB_DIR/"
}

[ -z "$*" ] && set -- "${!PARSER_ARGS[@]}"

for lang in "$@"; do
    # args=(${PARSER_ARGS[$lang]})
    read -r -a args <<<"${PARSER_ARGS[$lang]}"
    install_parser "$lang" "${args[@]}"
done
