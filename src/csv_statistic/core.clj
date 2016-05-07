(ns csv-statistic.core
  (:gen-class))
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])
(use 'clojure.string)
		 
(def garbage-pattern #"(\w*day)|No|Yes|(N\/A)|(^[0-9])") ; where day pattern word
(def number-pattern #"^[0-9]+$")
(def us-phone-pattern #"^\D?(\d{3})\D?\D?(\d{3})\D?(\d{4})$")
(def date-pattern #"^(((0?[1-9]|1[012])/(0?[1-9]|1\d|2[0-8])|(0?[13456789]|1[012])/(29|30)|(0?[13578]|1[02])/31)/(19|[2-9]\d)\d{2}|0?2/29/((19|[2-9]\d)(0[48]|[2468][048]|[13579][26])|(([2468][048]|[3579][26])00)))$")
(def word-split-pattern #"\s+")
		 
(def collection (atom {:ver "0.0.1" }))
		 
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Consumer Complaints Database  (parsing..)!")
  
  (defn csv-map
  "ZipMaps header as keys and values from lines."
  [head & line]
    (map #(zipmap (map keyword head) %1) line))
   

 (defn csv-map-ext 
   [head & rest-lines]
   ; (map csv-map rest-lines)
    (for [x rest-lines] (csv-map head x) )
 ) 
 
(defn cartesian-pairs 
"Function read col and return list of all possible pairs [ [x1 x2] .. [xi xj] ] where xi not = xj and xi not number   "
[ coll ]
;(def collection (atom {:ver "0.0.1"}))
  (->  
  (for [x coll  y coll  :when ( < (count x) 20)  :when (not= x y) :when (and (not (number? x)) ( not (number? y))) 
  :when ( < (.indexOf coll x)  (.indexOf coll y))  ] 
   (str ":" x y )  ) 
 
  ) 
) 
  
(defn match-vector [myVec]
(loop [data myVec, index 0]
     (def first-data (first data))
     (if-not (nil? (re-find #"(APR)" first-data))
      first-data
      (recur (rest data) (inc index)))
    )
)

(defn printObject 
" get object from the list assuming it is map and print all it's elements"
[& param]
; debug (println (nth param 1))
           ( def object (nth param 1) )
 		   ( ->
		     (println)
		     (print (object :Company) " - " (object :Product)  ) 
		    
		   )
)  


(defn ws-replce
"Replace whitespace in string  - 'par'"
[param]
;(clojure.string/replace param #"(\w+)" "\U$0" )
(clojure.string/replace
(clojure.string/replace param #"([\s\w\-_]|^)([a-z0-9-_]+) "
     (fn [[b]]
       (str  (clojure.string/upper-case  (get b 0 ))  (get b 1 ) (get b 2 ) (get b 3 ) (get b 4 ) (get b 5 ) (get b 6 )(get b 7 )
           )))
 #"[ ]" "")
)



(defn add-keyvalue-to-map
"add a new key to map or if exist increment value corresponding for this key"
[param  collection]
    (def col-key (read-string (str ":" param)))
    ;( println "debug add-keyvalue-to-map.1")
	;( println  "col-key =" col-key )
	;( println  "collection = " collection )
	;( println  "@collection = " @collection )
	;( println  "(get @collection col-key) = " (get @collection col-key))
    (if-not (nil? (get @collection col-key ))
    ( swap! collection update-in [col-key] inc)
		;; notice that the value swapped in, is part of the returned value
    ( swap! collection assoc col-key 1)
	) ; if
	
	;(println "after if")
)

(defn alones
[ coll ]
"Function read col and return list of [x] "
   (->
   (for [x coll] ; :when (not ( blank? x)) :when( < (count x) 15 ) :when (nil? (re-find number-pattern x)) :when (nil? (re-find date-pattern x)) :when (nil? (re-find garbage-pattern x))  ]
    (str ":" (ws-replce (trim x)) ))  
   ;println
   )	 
)


 
(defn validator
"Function validator used to agregate condition by AND boolean multiplication"
[x]
 (and (not (number? x)) (not ( blank? x)) ( < (count x) 15 ) (nil? (re-find number-pattern x)) (nil? (re-find date-pattern x)) (nil? (re-find garbage-pattern x))  )
 
) 

(defn validated-indexes
[ coll ]
"Function read col and return list of [clollection x indexes which passed validation [ 1,5, ..k,.. l] where k,l <n , n - size of collection x "
(time (doall
   (->
   (for [x coll  :when (validator x)  ]
    (.indexOf coll x))
    ;println
   )  
 ))  
)

;(def myfunc-memo (memoize add-cartesian-pairs-to-map))
  (def row-n (atom 0))

(defn reg-exp-filter
 [x y]
 ( and (nil? (re-find number-pattern x )) (nil? (re-find date-pattern x)) (nil? (re-find garbage-pattern x ))
     (nil? (re-find number-pattern y )) (nil? (re-find date-pattern y)) (nil? (re-find garbage-pattern y )) )
)
(defn cartesian-pairs 
"Function read col and return list of all possible pairs [ [x1 y1] .. [xi yj] ] where (x not equal y) and x,y not number for [xi yj] where i<j"
[ coll ]
  (->  
 
 (time (doall 
  (for [x coll  y coll :when (and (not ( blank? x))(not ( blank? y))(not= x y)) :when (and ( < (.indexOf coll x)(.indexOf coll y)) ) ]
;  :when ( reg-exp-filter x y ) ] 
   (str ":" (ws-replce (trim x)) "|" (ws-replce (trim y)) )  ) 
))
   ) 
  ;(println (swap! row-n inc) )
)





(defn reduce-csv-row
    "Accepts a csv-row (a vector) a list of columns to extract, 
     and reduces (and returns) a csv-row to a subset based on 
     selection using the values in col-nums (a vector of integer 
     vector positions.)"
 
    [csv-rows col-nums]
 
    (reduce 
        (fn [out-csv-row col-num]
            ; Don't consider short vectors containing junk.
            ;(println "col-num: " col-num " out-csv-row: " out-csv-row)
            (def row-num 0)
            (if-not (<= (count csv-rows) 1)
               (conj out-csv-row 
                 (for [x csv-rows ]
                       (nth x col-num nil)
                 )
               )    
            ))
        []
        col-nums))

 
(defn add-cartesian-pairs-to-map
;"add a new key of Cartesian pairs  to map or if exist increment value corresponding for this key
;Result of this function: {:bf 2, :bd 1, :be 1, :fd 1, :fe 1, :ff 1, :de 1, :df 1, :ef 1}  
; where collection parameter is : [123 45 "b" "f"  "d" "e" 'f 123 1234 4534] "
[my-coll indexes]
;(reduce #(assoc %1 %2  (inc (%1 %2 0))) 
(reduce #(swap! collection assoc %2 (inc (%1 %2 0)))
        {}
		;(alones (reduce into  ( reduce-csv-row  my-coll indexes )))) ;only [x]
        ;(reduce conj (cartesian-pairs my-coll) (alones my-coll) ))   ; both [x] [x y]
		(cartesian-pairs (reduce into (reduce-csv-row  my-coll indexes )))) ; only [x y] pairs
)

(defn add-uniquekeys-to-map
;"add a new key of Cartesian pairs  to map or if exist increment value corresponding for this key
;Result of this function: {:bf 2, :bd 1, :be 1, :fd 1, :fe 1, :ff 1, :de 1, :df 1, :ef 1}  
; where collection parameter is : [123 45 "b" "f"  "d" "e" 'f 123 1234 4534] "
[my-coll indexes]
;(reduce #(assoc %1 %2  (inc (%1 %2 0))) 
(reduce #(swap! collection assoc %2 (inc (%1 %2 0)))
        {}
		(alones (reduce into  ( reduce-csv-row  my-coll indexes )))) ;only [x]
        ;(reduce conj (cartesian-pairs my-coll) (alones my-coll) ))   ; both [x] [x y]
		;(cartesian-pairs (reduce into (reduce-csv-row  my-coll indexes )))) ; only [x y] pairs
)

 
(with-open [in-file (io/reader "resources/book.csv")]
 (doall
    ;(csv/read-csv in-file))
  (let [csv (csv/read-csv in-file)] ; :separator \;)]
    (println  "rows count:" (count csv) )
    (println "columns count"(count (first csv)))
	(def first-data (first csv))
	(println first-data)
	
	(def csv-file-one-row (nth csv 1))
	
	(def indexes (validated-indexes csv-file-one-row))

		(time (doall  
		 (add-uniquekeys-to-map (rest csv) indexes) 
        ))	
		
       (println collection)	

  ; (for [x (rest csv)] 
         ;(add-keyvalue-to-map (ws-replce (get x 1)) collection)
		(time (doall  
		 (add-cartesian-pairs-to-map (rest csv) indexes) 
		 ; (add-cartesian-pairs-to-map x) ;(list ws-replce x ) ;"Elapsed time: 16135.905126 msecs"
		;( myfunc-memo x) ;"Elapsed time: 20594.399339 msecs"
        ))	
		;	) ;for
     
;(println "row: " (inc row)

 	

   ) ; let  
  ) ;doall
) ; with	
(println collection)
 ) ;main
