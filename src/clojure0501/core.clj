(ns clojure0501.core)

(defmacro foreach [[sym coll] & body]
  ;(println sym coll body)   
             ;sym=x   coll=[1 2 3]     body=((println x))
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
























