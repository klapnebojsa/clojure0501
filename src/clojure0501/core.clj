(ns clojure0501.core
  (:use clojure.pprint))

; M A C R O
(defmacro foreach [[sym coll] & body]
  ;(println sym coll body)   
             ;sym=x   coll=[1 2 3]     body=((println x))
 ;` trebako bi da se stavlja na pocetku maroa
                            ;ovo (tj. # i ~) ima veze izgleda sa ` ili/i makroom
  `(loop [coll# ~coll]       ;~coll je vrednost koja je doneta kao coll i u nasem primeru je [1 2 3]  tj. ~x  -> vrednost promenjive x
                             ;coll# je vrednost koja se formira prilikom izvrsenja makroa i u svakom koraku je kraca za prvog clana liste
                                ;i u prvom koraku ima vrednost ~coll
                                ;promenjiva coll# se moze nazvati bilo kako ali na kraju mora imati znak #
     ;(println "coll# =" coll# "   ~coll =" ~coll "   (seq coll#) =" (seq coll#))
              ;coll# = [1 2 3]    ~coll = [1 2 3]    (seq coll#) = (1 2 3)
              ;coll# = (2 3)      ~coll = [1 2 3]    (seq coll#) = (2 3)
              ;coll# = (3)        ~coll = [1 2 3]    (seq coll#) = (3)
              ;coll# = nil        ~coll = [1 2 3]    (seq coll#) = nil
     (when-let [[~sym & xs#] (seq coll#)]        ;(seq coll#) pravi listu od vrednosti coll#
                                                 ;prvu vrednost iz (seq coll#) stavlja u ~sym a ostatak u xs#
       ;(println "xs#=" xs#)
               ;xs#= (2 3)
               ;xs#= (3)
               ;xs#= nil
       ;~@body           ;izvrsava komandu    -    println x    sa znakovima ~@ izvrsi donetu KOMANDU 
                        ;moja omiljena igracka
       (recur xs#))))     ;uslov za loop - smesta xs# u coll#
                          ;moglo se napisati umesto xs# coll# i radilo bi isto
(foreach [x [1 2 3]] (println x))
;1
;2
;3

(list 100 101 102)
;(100 101 102)
`(100 101 102)
;(100 101 102)
`(~(list 201 202 203))    ; znak ~ unquote
;((201 202 203))
`(~@(list 301 302 303))
;(301 302 303)
;(def pp (println 45))
;`(~@pp)
;45
(def x 5)
(def lst '(a b c))
`(       x             ~x            lst             ~@lst     7 8 :nine)
;(clojure0501.core/x    5    clojure0501.core/lst    a b c     7 8 :nine)

(defmacro print-keyword [x]
  `(println (keyword ~x)))

;(print-keyword "foo")
;:foo

(require '(clojure [string :as str]
                   [walk :as walk]))
(defmacro reverse-it
  [form]
  (walk/postwalk #(if (symbol? %)
                    (symbol (str/reverse (name %)))
                    %)
                 form))
#_(reverse-it
   (qesod [gra (egnar 5)]
          (nltnirp (cni gra))))
;1
;2
;3
;4
;5

(defmacro oops [arg] `(frobnicate ~arg))
;(oops 123)
;CompilerException java.lang.RuntimeException: No such var: clojure0501.core/frobnicate, compiling:(clojure0501\core.clj:73:1)
(macroexpand-1 '(oops 123))
;(clojure0501.core/frobnicate 123)

#_(macroexpand-1 '(reverse-it
                     (qesod [gra (egnar 5)]
                            (nltnirp (cni gra)))))
;(doseq [arg (range 5)] (println (inc arg)))

#_(pprint (macroexpand '(reverse-it
                         (qesod [gra (egnar 5)]
                                (nltnirp (cni gra))))))

#_(loop*
 [seq_11992
  (clojure.core/seq (range 5))
  chunk_11993
  nil
  count_11994
  0
  i_11995
  0]
 (if
  (clojure.core/< i_11995 count_11994)
  (clojure.core/let
   [arg (.nth chunk_11993 i_11995)]
   (do (println (inc arg)))
   (recur
    seq_11992
    chunk_11993
    count_11994
    (clojure.core/unchecked-inc i_11995)))
  (clojure.core/when-let
   [seq_11992 (clojure.core/seq seq_11992)]
   (if
    (clojure.core/chunked-seq? seq_11992)
    (clojure.core/let
     [c__4356__auto__ (clojure.core/chunk-first seq_11992)]
     (recur
      (clojure.core/chunk-rest seq_11992)
      c__4356__auto__
      (clojure.core/int (clojure.core/count c__4356__auto__))
      (clojure.core/int 0)))
    (clojure.core/let
     [arg (clojure.core/first seq_11992)]
     (do (println (inc arg)))
     (recur (clojure.core/next seq_11992) nil 0 0))))))

(macroexpand '(cond a b c d))
;(if a b (clojure.core/cond c d))

(require '[clojure.walk :as w])
;(w/macroexpand-all '(cond a b c d))
;(if a b (if c d nil))

;(w/macroexpand-all ''(when x a))
;(quote (if x (do a)))

(defmacro hello
  [name]
  (list 'println name))
(macroexpand '(hello "Brian"))
;(println "Brian")
;(hello "Brian")
;Brian

(defmacro while1
  [test & body]
  (list 'loop []
        (concat (list 'when test) body)
        '(recur)))
;PRAVA SINTAKSA ZA MAKRO-e
(defmacro while1
  [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(def foo 123)
 [foo (quote foo) 'foo           `foo]
;[123    foo       foo    clojure0501.core/foo]

(list `map `println [foo])
;(clojure.core/map clojure.core/println [123])

`(map println [~foo])                                   ;Pametnije
;(clojure.core/map clojure.core/println [123])
`(map println ~[foo])
;(clojure.core/map clojure.core/println [123])

`(println ~(keyword (str foo)))
;(clojure.core/println :123)

(let [defs '((def x 123)       ; 'quote - string
              (def y 456))]
  (concat (list 'do) defs))     ;concat spaja u listu string do i vrednost u stringu defs=(def x 123) (def y 456)
;(do (def x 123) (def y 456))

(let [defs '((def x 123)
              (def y 456))]
  `(do ~@defs))
;(do (def x 123) (def y 456))

(defmacro foo
  [& body]
  `(do-something ~@body))
(macroexpand-1 '(foo (doseq [x (range 5)]
                       (println x))
                     :done))
#_(clojure0501.core/do-something 
    (doseq [x (range 5)] 
      (println x)) 
    :done)

(defn fn-hello [x]
  (str "Hello, " x "!"))
(defmacro macro-hello [x]
  `(str "Hello, " ~x "!"))
(fn-hello "Brian")
;"Hello, Brian!"
(macro-hello "Brian")
;"Hello, Brian!"

(map fn-hello ["Brian" "Not Brian"])
;("Hello, Brian!" "Hello, Not Brian!")
;(map macro-hello ["Brian" "Not Brian"])
;CompilerException java.lang.RuntimeException: Can't take value of a macro: #'clojure0501.core/macro-hello, compiling:(clojure0501\core.clj:201:1)
(map #(macro-hello %) ["Brian" "Not Brian"])
;("Hello, Brian!" "Hello, Not Brian!")

(defmacro unhygienic
  [& body]
  `(let [x :oops]        ;Problem :oops bi trebalo da je vrednost stringa
     ~@body))            ;izvrsi donetu komandu
;(unhygienic (println "x:" x))
;CompilerException java.lang.RuntimeException: Can't let qualified name: clojure0501.core/x, compiling:(clojure0501\core.clj:210:1)
(macroexpand-1 `(unhygienic (println "x:" x)))
#_(clojure.core/let [clojure0501.core/x :oops] 
    (clojure.core/println "x:" clojure0501.core/x))

(defmacro still-unhygienic
  [& body]
  `(let [~'x :oops]         ;~x je vrednost 5 koja je nekada pre definisana
                            ;trebalo bi: 'x je string x a ~'x je isto x ali s samo koristi pod znakom` pa mora da se doda ~
     ~@body))
#_(still-unhygienic (println "x:" x))
;x: :oops

(macroexpand-1 '(still-unhygienic
                  (println "x:" x)))
#_(clojure.core/let [x :oops] 
    (println "x:" x))

#_(let [x :this-is-important]
   (still-unhygienic
     (println "x:" x)))
;x: :oops

(gensym)
;G__13457
(gensym "sym")
;sym13608

(defmacro hygienic
  [& body]
  (let [sym (gensym)]               ;*1
    `(let [~sym :macro-value]       ;*2
       ~@body
       )))
#_(let [x :important-value]
    (hygienic (println "x:" x)))
;x: :important-value

`(x# x# x222#)
#_(x__8770__auto__ 
   x__8770__auto__ 
   x222__8771__auto__)

(defmacro auto-gensyms
  [& numbers]
  `(let [x# (rand-int 10)]                    ;*1
     ;(println x#)                            ;*2
     (+ x# ~@numbers)))

(auto-gensyms 1 2 3 4 5)
;1
;16
(macroexpand-1 '(auto-gensyms 1 2 3 4 5))     ;*3
#_(clojure.core/let 
    [x__11882__auto__ (clojure.core/rand-int 10)] 
    (clojure.core/+ x__11882__auto__ 1 2 3 4 5))

(defmacro our-doto [expr & forms]
  `(let [obj# ~expr]                           ;*1
     ~@(map (fn [[f & args]]
              `(~f obj# ~@args)) forms)        ;*2
     obj#))
#_(our-doto "It works"
           (println "I can't believe it"))
;CompilerException java.lang.RuntimeException: Unable to resolve symbol: obj__13204__auto__ in this context, compiling:(clojure0501\core.clj:273:1)

(defmacro our-doto [expr & forms]
  ;(println "expr=" expr "  forms=" forms)   ;expr= It works   
                                             ;forms= ((println I can't believe it) (println I still can't believe it))
  (let [obj (gensym "obj")]         ;punimo promenjivu obj sa slucajno imenom koje pocinje sa obj
    ;(println obj)                            ;obj14157
    `(let [~obj ~expr]              ;punimo vrednost promenjive obj sa donetom vresnoscu
       ;(println ~obj)                        It works  -  doneta vrednost
       ~@(map (fn [[f & args]]
                ;(println "f=" f "args=" args)    ;f= println args= (I can't believe it)
                                                  ;f= println args= (I still can't believe it)                             
                `(~f ~obj ~@args)) forms)      ;*1    ;formiramo rec   f-println
                                                                   ;obj-It works
                                               ;izvrsavamo komandu u args-println "I can't believe it"           -prva komanda
                                               ;izvrsavamo komandu u args-println "I still can't believe it"     -druga komanda
       ~obj)))                      ;ispisujemo donetu vrednost promenjive obj

#_(our-doto "It works"
          (println "I can't believe it")
          (println "I still can't believe it"))
#_(It works I can't believe it
   It works I still can't believe it
   "It works")

#_(macroexpand-1 '(our-doto "It works"
                          (println "I can't believe it")
                          (println "I still can't believe it")))
#_(clojure.core/let [obj18855 "It works"] 
    (println obj18855 "I can't believe it") 
    (println obj18855 "I still can't believe it") 
    obj18855)
#_(macroexpand '(our-doto "It works"
                        (println "I can't believe it")
                        (println "I still can't believe it")))
#_(let* [obj19230 "It works"] 
    (println obj19230 "I can't believe it") 
    (println obj19230 "I still can't believe it") 
    obj19230)

(defmacro with
  [name & body]
  `(let [~name 5]
     ~@body))
(with bar (+ 10 bar))
;15
(with bar (* 10 bar))
;50

(defmacro spy [x]
  `(do
     (println "spied" '~x ~x)    ;'~x -doneta vrednost (tj doneta tekstualna komanda)
                                 ;~x - izvrsava donetu komandu u polju x
     ~x))                        ;~x - ponovo izvrsava donetu komandu x    TU NASTAJE PROBLEM I ZATO IMAMO DVE RAZLICITE VREDNOSTI IZLAZA
;(spy 2)
;spied 2 2
;2
;(spy (rand-int 10))
;spied (rand-int 10) 4
;0

(macroexpand-1 '(spy (rand-int 10)))
#_((do (clojure.core/println "spied" (quote (rand-int 10)) (rand-int 10)) 
     (rand-int 10)))

(defmacro spy [x]
  `(let [x# ~x]                   ;lokalnoj promenjivoj x# dodeljujemo vrednost izvrsenja donete komande
     (println "spied" '~x x#)     ;ispisujemo denoetu komandu i vrednosr koja je vec izvrsena
     x#))                         ;ispisujemo vrednost koja je vec izracunata i dodeljena lokalnoj promenjivoj x#
;(spy (rand-int 10))
;spied (rand-int 10) 7
;7
(macroexpand-1 '(spy (rand-int 10)))
#_((clojure.core/let [x__10937__auto__ (rand-int 10)] 
     (clojure.core/println "spied" (quote (rand-int 10)) x__10937__auto__) 
     x__10937__auto__))

(defn spy-helper [expr value]         ;value je dolazna vrednost iz makroa tako da je ona u jednom prolazu vec izracunata i konstantna
  (println "spy-helper" expr value)
  value)
(defmacro spy [x]
  `(spy-helper '~x ~x))   ;u ovom koraku racunamo vrednost i saljemo je u funkciju.
;(spy (rand-int 10))
;spy-helper (rand-int 10) 7
;7

(defmacro spy-env []
  (let [ks (keys &env)]
    `(prn (zipmap '~ks [~@ks]))))
#_(let [x 1 y 2]
   (spy-env)
   (+ x y))
;{x 1, y 2}
;3

(defmacro simplify
  [expr]
  (println "expr: " expr)
  (let [locals (set (keys &env))]
    (println "locals: " locals)
    (if (some locals (flatten expr))
      expr                                    ;*1
      (do                                     ;*2
        (println "Precomputing: " expr)
        (list `quote (eval expr))))))         ;*3
#_(defn f
    [a b c]
    (+ a b c (simplify (apply + (range 5e7)))))      ;(apply + (range 5e7))  zbir prvih 5*10na7-mu brojeva
                                                   ;i na sve to se jos doda zbir a b c tj 1+2+3=6
;(time (f 1 2 3))
;expr:  (apply + (range 5.0E7))
;locals:  #{a c b}
;Precomputing:  (apply + (range 5.0E7))
;"Elapsed time: 0.05724 msecs"
;1249999975000006

#_(defn f'
  [a b c]
  (simplify (apply + a b c (range 5e7))))
;(time (f' 1 2 3))
;expr:  (apply + a b c (range 5.0E7))
;locals:  #{a c b}
;"Elapsed time: 1662.401059 msecs"
;1249999975000006

(@#'simplify nil {} '(inc 1))
;expr:  (inc 1)
;locals:  #{}
;Precomputing:  (inc 1)
;(quote 2)
(@#'simplify nil {'x nil} '(inc x))
;expr:  (inc x)
;locals:  #{x}
;(inc x)










