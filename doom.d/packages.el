;;; $DOOMDIR/packages.el -*- lexical-binding: t; no-byte-compile: t -*-

;; To install a package:
;;
;;   1. Declare them here in a `package!' statement,
;;   2. Run 'doom sync' in the shell,
;;   3. Restart Emacs.
;;
;; Use 'C-h f package\!' to look up documentation for the `package!' macro.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")

;; (package! benchmark-init)

;; (package! fcitx :ignore (not (eq system-type 'gnu/linux)))
(package! rime)
;; (package! pyim)
;; (package! pyim-greatdict :recipe (:host github :repo "tumashu/pyim-greatdict"))
;; (package! pangu-spacing)

(package! math-preview)
(package! mermaid-mode)
(package! d2-mode)

;; Protobuf: tree-sitter 编辑模式（LSP/格式化走 buf CLI，见 protobuf-conf.el / format-conf.el）
(package! protobuf-ts-mode)

;; (package! gmpl-mode)
;; (package! tmux-pane)

;; (package! treemacs-evil)

;; eglot 悬浮文档的 childframe 渲染（见 eglot-conf.el）。
;; consult-eglot 由 tools/lsp +eglot 自动按需安装；诊断走 checkers/syntax +flymake 直通。
(package! eldoc-box)

;; shell 环境改用 `doom env` 快照（见 mac-opt.el），exec-path-from-shell 可弃用。
;; (package! exec-path-from-shell :ignore (not (eq system-type 'darwin)))

;; GitLens
(package! blamer :recipe (:host github :repo "artawower/blamer.el"))

;; 翻译
(package! pdd :recipe (:host github :repo "lorniu/pdd.el"))
(package! gt :recipe (:host github :repo "lorniu/gt.el"))

;; gptel: tools/llm 中启用
;; (package! gptel)

;; chatgpt-shell: 加载效率太低，功能和gptel差不多，暂时不用了
;; (package! chatgpt-shell
;;   :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el")))

;; AI code completion
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

;; ;; (package! transient)  ;; tool/magit 中已添加
;; (package! aider :recipe (:host github :repo "tninja/aider.el"))
;; (package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs"))

(package! eat) ;; 终端后端，实测比 vterm 稳；切换见 ai-claude.el
(package! claude-code-ide :recipe (:host github :repo "manzaltu/claude-code-ide.el")) ;; 基于 MCP 的 IDE 集成（ediff 审查、暴露 Emacs 工具给 Claude）
;; (package! claude-code :recipe (:host github :repo "stevemolitor/claude-code.el")) ;; 切到 claude-code-ide
;; (package! claudemacs :recipe (:host github :repo "cpoile/claudemacs"))
;; Codex 对应物（纯 buffer 客户端，见 ai-codex.el）；bin/ 是 Emacs MCP 桥脚本，
;; 排除仓库自带的陈旧 autoloads（straight 会重新生成）。
(package! codex-ide :recipe (:host github :repo "dgillis/emacs-codex-ide"
                             :files (:defaults "bin" (:exclude "*-autoloads.el"))))

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)
