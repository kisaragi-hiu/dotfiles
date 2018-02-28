#lang racket
(require xml
         threading)

(provide (all-defined-out))
(module+ test
  (require rackunit))

;; I want a contracted version of this, but simply passing the contract breaks
;; as a new input is added
(define-syntax (def/xexpr-switch stx)
  (syntax-case stx ()
    [(_ (name args ... . rest) body ...)
     #'(begin
         (define (name #:return-xexpr? [return-xexpr? #f] args ... . rest)
           ;; this is inconsistent with normal define as now all bodies are wrapped in one
           ;; hence the inconsistent name def/xexpr-switch
           ((if return-xexpr? identity xexpr->string*) body ...)))]))

(define/contract (xexpr->string* . xexprs)
  ;; xexpr->string only takes one xml tree. Work around that, take multiple xexprs and turn them into a string.
  (->* () () #:rest (listof xexpr?) string?)
  (~> (cons 'root xexprs)
      xexpr->string
      (string-replace _ #rx"^<root>" "")
      (string-replace _ #rx"</root>$" "")))

(def/xexpr-switch (action/undecorate-and-maximize)
  '(action ([name "Undecorate"]))
  '(action ([name "Maximize"])))
(def/xexpr-switch (action/decorate-and-unmaximize)
  '(action ([name "Decorate"]))
  '(action ([name "Unmaximize"])))

(module+ test
  (check-equal? (action/decorate-and-unmaximize)
                "<action name=\"Decorate\"></action><action name=\"Unmaximize\"></action>")
  (check-equal? (action/undecorate-and-maximize)
                "<action name=\"Undecorate\"></action><action name=\"Maximize\"></action>"))

(def/xexpr-switch (action/execute . arguments)
  `(action ([name "Execute"])
           (command ,(string-join (flatten arguments)))))

(define (osu-select-sorting sorting)
  ;; item: artist, bpm, creator, date, difficulty, length, rank, title
  (define item-position
    (case sorting
      [("artist") "30"]
      [("bpm") "60"]
      [("creator") "100"]
      [("date") "130"]
      [("difficulty") "170"]
      [("length") "200"]
      [("rank") "230"]
      [("title") "280"]))
  (define wait "sleep 0.1")
  (string-append "bash -c '" 
                 (string-join
                   `("xdotool mousemove 1750 55"
                     "xdotool click 1"
                     ,wait
                     ,(string-append "xdotool mousemove_relative 0 " item-position)
                     "xdotool click 1")
                    ";")
                 "'"))

(def/xexpr-switch (action/execute/terminal . arguments)
  (action/execute #:return-xexpr? #t
                  (or (getenv "TERMINAL") "alacritty") "-e"
                  arguments))

(def/xexpr-switch (action/notify summary [body " "] #:icon [icon #f])
  (let ([icon-arg1 (if icon "--icon" "")] [icon-arg2 (if icon (string-append "\"" icon "\"") "")])
    (action/execute #:return-xexpr? #t
             "notify-send"
             (string-append "\"" summary "\"")
             (string-append "\"" body "\"")
             icon-arg1
             icon-arg2)))

(def/xexpr-switch (action/plasma-osd text #:icon icon) ; plasma's osd requires an icon
  `(action ([name "Execute"])
           (command (string-append "qdbus org.kde.plasmashell /org/kde/osdService org.kde.osdService.showText "
                                   ,icon
                                   " "
                                   ,text))))

(module+ test
  (check-equal? (action/notify "hello" #:icon "retry")
                "<action name=\"Execute\"><command>notify-send \"hello\" \" \" --icon \"retry\"</command></action>")
  (check-equal? (action/execute "echo" "b" "c")
                "<action name=\"Execute\"><command>echo b c</command></action>"))

;; this is only used in one section, no need to use xexpr switch (probably)
(define/contract (font place name [size 11]
                       #:weight [weight "normal"]
                       #:slant [slant "normal"])
  (->* (string? string?)
       (number? #:weight (or/c "bold" "normal")
                #:slant (or/c "italic" "normal"))
       string?)
  (xexpr->string `(font ([place ,place]) (name ,name) (size ,(number->string size)) (weight ,weight) (slant ,slant))))

(module+ test
  (check-equal? (font "ActiveWindow" "Roboto")
                "<font place=\"ActiveWindow\"><name>Roboto</name><size>11</size><weight>normal</weight><slant>normal</slant></font>"))

(def/xexpr-switch (keybind key . actions)
  `(keybind ([key ,key]) ,@actions))

