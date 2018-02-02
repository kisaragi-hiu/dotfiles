#lang racket
(require xml
         threading)

(provide (all-defined-out))

(define/contract (xexpr->string* . xexprs)
  ;; xexpr->string only takes one xml tree. Work around that, take multiple xexprs and turn them into a string.
  (->* () () #:rest (listof xexpr?) string?)
  (~> (cons 'root xexprs)
      xexpr->string
      (string-replace _ #rx"^<root>" "")
      (string-replace _ #rx"</root>$" "")))

(define (action/undecorate-and-maximize)
  (xexpr->string* '(action ([name "Undecorate"]))
                  '(action ([name "Maximize"]))))
(define (action/decorate-and-unmaximize)
  (xexpr->string* '(action ([name "Decorate"]))
                  '(action ([name "Unmaximize"]))))
(define (action/toggle-maximize-and-decorations)
  (xexpr->string `(action ([name "If"])
                          (maximized "yes")
                          (then (action ([name "Decorate"]))
                                (action ([name "Unmaximize"])))
                          (else (action ([name "Undecorate"]))
                                (action ([name "Maximize"]))))))

(define/contract (action/notify summary [body " "] #:icon [icon #f])
  (->* (string?) (string? #:icon (or/c #f string?)) string?)
  (define command (string-join `("notify-send"
                                 ,(string-append "\""summary"\"")
                                 ,(string-append "\""body"\"")
                                 ,(if icon
                                      (string-append "--icon " "\""icon"\"")
                                      ""))))
  (xexpr->string
   `(action ([name "Execute"])
            (command ,command))))

(define (action/plasma-osd text #:icon icon) ; plasma's osd requires an icon
  `(action ([name "Execute"])
           (command (string-append "qdbus org.kde.plasmashell /org/kde/osdService org.kde.osdService.showText "
                                   ,icon
                                   " "
                                   ,text))))

(define/contract (font place name [size 11]
                       #:weight [weight "normal"]
                       #:slant [slant "normal"])
  (->* (string? string?)
       (number? #:weight (or/c "bold" "normal")
                #:slant (or/c "italic" "normal"))
       string?)
  (xexpr->string `(font ([place ,place]) (name ,name) (size ,(number->string size)) (weight ,weight) (slant ,slant))))

(module+ test
  (require rackunit)
  (check-equal? (font "ActiveWindow" "Roboto")
                "<font place=\"ActiveWindow\"><name>Roboto</name><size>11</size><weight>normal</weight><slant>normal</slant></font>")
  (check-equal? (action/notify "hello" #:icon "retry")
                "<action name=\"Execute\"><command>notify-send \"hello\" \" \" --icon \"retry\"</command></action>"))
