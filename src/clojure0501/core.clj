(ns clojure0501.core
  (:use clojure.pprint))

; M A C R O
(defmacro foreach [[sym coll] & body]
  ;(println sym coll body)   
             ;sym=x   coll=[1 2 3]     body=((println x))
 ;` trebako bi da se stavlja na pocetku maroa
                            ;ovo (tj. # i ~) ima veze izgleda sa ` ili/i makroom
  `(loop [coll# ~coll]       ;~coll je vrednost koja je doneta kao coll i u nasem primeru je [1 2 3]  tj. ~x  -> vrednost promenjive x
                             ;coll# je vrednost koja se formira prilokom izvrsenja makroa i u svakom koraku je kraca za prvog clana liste
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









