#lang pollen
◊; generate my .pam_environment
◊; ◊(getenv ENV) uses the current environment when this file is run through pollen

◊(require racket/string
          racket/port
          racket/list
          racket/system
          racket/file)

◊(define (defpam_env name . contents)
   (define content (string-join contents ""))
   ◊string-append{◊|name| DEFAULT=◊|content|})

◊; this makes the defined variable usable in Racket/Pollen context as well
◊; example: (define/pam RACKET "racket.test")
◊(define-syntax (define/pam stx)
   (syntax-case stx ()
     [(_ name body ...)
      #'(begin
          (define name (string-append body ...)) ; side effect, but well.
          (defpam_env (symbol->string 'name) body ...))]))

◊(define HOME (getenv "HOME"))
◊(define USER (getenv "USER"))
◊(define racket-version (version))
◊(define ruby-version
   ; this, or sed | cut? I don't know which I prefer really.
   ((compose1 first
              (λ ($1) (string-split $1 "p"))
              second
              (λ ($1) (string-split $1 " ")))
    (with-output-to-string (λ () (system "ruby --version")))))
◊(define (pathlist-string . args)
   (string-join (string-split (string-join args "") "\n") ":"))

◊; == directory shortcuts ==
◊defpam_env["D"]{/run/media/◊|USER|/Data}
◊;defpam_env["C"]{/run/media/◊|USER|/Windows}
◊defpam_env["G"]{◊|HOME|/ドキュメント}
◊defpam_env["M"]{/run/media/◊|USER|/Data/mega}
◊defpam_env["P"]{/run/media/◊|USER|/Data/mega/Projects}
◊defpam_env["XDG_DESKTOP_DIR"]{◊|HOME|/デスクトップ}
◊defpam_env["XDG_DOWNLOAD_DIR"]{◊|HOME|/ダウンロード}
◊defpam_env["XDG_TEMPLATES_DIR"]{◊|HOME|/テンプレート}
◊defpam_env["XDG_PUBLICSHARE_DIR"]{◊|HOME|/公開}
◊defpam_env["XDG_DOCUMENTS_DIR"]{◊|HOME|/ドキュメント}
◊defpam_env["XDG_MUSIC_DIR"]{◊|HOME|/音楽}
◊defpam_env["XDG_PICTURES_DIR"]{◊|HOME|/画像}
◊defpam_env["XDG_VIDEOS_DIR"]{◊|HOME|/ビデオ}
◊defpam_env["DOTFILES_DIR"]{◊|HOME|/.dotfiles/}

◊; == ime ==
◊defpam_env["GTK_IM_MODULE"]{fcitx}
◊defpam_env["QT_IM_MODULE"]{fcitx}
◊defpam_env["XMODIFIERS"]{@im=fcitx}

◊; == UI ==
◊defpam_env["QT_QPA_PLATFORMTHEME"]{qt5ct}

◊; == PATH ==
◊defpam_env["GOPATH"]{◊|HOME|/.gopath}

◊defpam_env["PATH"]{◊pathlist-string{
◊|HOME|/git/scripts
◊|HOME|/git/Sudocabulary
◊|HOME|/bin
◊|HOME|/.racket/◊|racket-version|/bin
◊|HOME|/.local/share/npm-global/bin
◊|HOME|/.gem/ruby/◊|ruby-version|/bin
$◊"{"PATH◊"}"
}}

◊; == app behavior controls ==
◊defpam_env["VISUAL"]{nvim}
◊defpam_env["EDITOR"]{nvim}
◊defpam_env["SSH_ASKPASS"]{/usr/bin/ksshaskpass} # ssh with kwallet
◊defpam_env["OCIO"]{${DOTFILES_DIR}/_filmic-blender/config.ocio}
◊defpam_env["WINEDEBUG"]{-all}
◊defpam_env["vblank_mode"]{0} # global vsync off

◊defpam_env["TERMINAL"]{alacritty}

◊; == "sourcing" private env
◊define/pam[PRIVATE_ENVIRONMENT]{◊|HOME|/.pam_private}
◊(file->string PRIVATE_ENVIRONMENT)
◊; vim: filetype=pollen
