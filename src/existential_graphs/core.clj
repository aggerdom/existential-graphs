(ns existential-graphs.core)

;; Helpers
(defn path-depth [path] (+ 2 (count (take-while #(not (nil? %)) path))))
(defn is-double-cut? [G path]
  (let [node (get-in G path)]
    (and (< 0 (count node))
         (= (first node) :cut) ;; Node value is :cut
         (= (count node) 2) ;; No other children but second cut
         (= (first (second node)) :cut))))

(defn is-sa-or-cut? [G path]
  (some? (#{:SA :cut} (first (G path)))))

(defn ancestor-paths [path]
  "Get ancestor paths from a path.
    EXAMPLE:
    (ancestor-paths []) ;; => nil
    (ancestor-paths [1]) ;; => '([])
    (ancestor-paths [1 2]) ;; => '((1) [])"
  (if (empty? path) nil
      (->> path
           (#(iterate butlast %))
           (take-while #(not (nil? %)))
           (drop 1)
           (into '())
           (cons '[])
           (reverse))))

(defn exists-ancestral-copy? [G path]
  "Determine whether a node in tree `G` at `path`
   has a copy within any ancestral node. (A node on which n depends)"
  (let [node (get-in G path)
        [parent-path & ancestor-paths] (ancestor-paths path)
        parent (get-in G parent-path)
        ancestors (map #(get-in G %) ancestor-paths)
        fnum-equal (fn [coll] (count (filter #(= node %) coll)))]
    (cond (< 1 (fnum-equal parent)) true ;; parent has copy
          (some #(<= 1 (fnum-equal %)) ancestors) true
          :else false)))

(exists-ancestral-copy? [:A [:B]] [1]) ;; => false
(exists-ancestral-copy? [:A [:B] [:B]] [1]) ;; =>  true
(exists-ancestral-copy? [:A [:B] [:C]] [1]) ;; =>  false
(exists-ancestral-copy? [:A [:B] [:cut [:B]]] [2 1]) ;; => true
(exists-ancestral-copy? [:A [:cut [:C]] [:cut [:C]]] [2 1]) ;; => false
(exists-ancestral-copy? [:A [:cut [:C]] [:cut [:C]]] [2]) ;; => true

(let [G [:A [:B] [:B]]
      path [1]]
  (->> (iterate butlast path)
       (take-while #(< 0 (count %)))
       (rest)
       (take 10)))


(defn insert [G subG path]
  "Insert node as a child of a specific path into graph.
   Empty path places node as child of the root."
  (cond (empty? path) (conj G subG)
        (nil? (first path)) (conj G subG)
        :else (update G (first path)
                      #(insert % subG (rest path)))))

(defn replace-node [G f path]
  "Replace node at a specific position with a new node obtained by
   calling f on the existing val"
  (if (= (count path) 1) (update G (first path) #(f %))
      (update (first path) #(replace-node % f (rest path)))))

(defn drop-vec-position [v ix]
  (into (subvec v 0 ix)
        (subvec v (inc ix))))

(defn erase [G path]
  {:pre [(< 0 (count path))]}
  (if (= (count path) 1) (drop-vec-position G (first path))
      (update G (first path) #(erase % (rest path)))))


;; Rules ==================================================================
;;      SA     -- 2
;;     cut     -- 3
;; A B cut     -- 4 (Insertion "in odd"=add B as child of odd number of cuts) [1 nil]
;;      C      -- 5 (Erasure "in even" = we can erase C) [1 1 1] (<- path is odd len)
;; Specify path to place child (path points to parent)


(defn insertion [G subG path]
  {:pre [(odd? (path-depth path))]
   :post []
   :docstring "Any subgraph may be inserted at an odd numbered depth"}
  (insert G subG path))


;; Specify path of child to erase (path points to node to erase)


(defn erasure [G path]
  {:pre [(even? (dec (path-depth path)))] ;; We can erase at any even depth (root is 2)
   :post []
   :docstring "Any subgraph in an even depth may be erased"}
  (erase G path))

(defn double-cut [G path]
  ;; If path points to a node, then:
  ;;  - double cut will be placed around that node
  ;;  - That node will be placed as a child under two cuts in its present pos
  ;; If path ends in nil, then:
  ;;  - two cuts will be placed as a child of (butlast node)
  ;;  - A double cut will be created without any children
  {:pre []
   :post []
   :docstring (str "A pair of cuts w nothing between them may be drawn"
                   "around any subgraph (including empty graph)")}
  (cond (= path []) (throw "Not understood")
        ;; CASE: Empty double-cut
        (and (= 1 (count path)) (nil? (first path)))
        (conj G [:cut [:cut]])
        ;; CASE: Cut around node
        (= 1 (count path))
        (update G (first path) (fn [cur-node] [:cut [:cut cur-node]]))
        ;; CASE: We're not drilled down yet
        :else (update G (first path) #(double-cut % (rest path)))))

(defn double-cut-erasure [G path]
  {:pre [(is-double-cut? G path)]
   :post []
   :docstring "A pair of cuts w nothing between them may be erased"}
  ;; Path points to first cut
  (let [G1 G
        [_ [_ & children]] (get-in G path)
        ;; Append all children of second cut to the parent
        G2 (into (get-in G1 (butlast path)) children)
        ;; Remove the second cut from the altered parent
        G3 (erase G2 path)]
    G3))


;; ==========---------------------------------------------------------------------------
(defn is-leaf? [node] (= (count node) 1))
(defn cuts-crossed [G path] (->> (ancestor-paths path)
                                 (map #(get-in G %))
                                 (map first)
                                 (filter #(= :cut %))
                                 (count)))
(defn is-start-of-path? [a b] (and (<= (count a) (count b))
                                   (every? #(apply = %) (map vector a b))))
(is-start-of-path? [1 2] [1]) ;; -> false
(is-start-of-path? [1] [1 2]) ;; -> true
(defn is-root? [G path] (= path []))
(defn have-common-parent? [G src-path dest-path]
  (let [parent-path (first (ancestor-paths src-path))]
    (or (is-start-of-path? src-path dest-path) ;; Node within itself
        (is-start-of-path? parent-path dest-path))))

(have-common-parent? [:SA [:A]] [1] [1 nil]) ;; [:SA [:A]] -> [:SA [:A [:A]]] (true)
(have-common-parent? [:SA [:A]] [1] [nil]) ;; [:SA [:A]] -> [:SA [:A] [:A]] (true)
(have-common-parent? [:SA [:A]] [] [nil]) ;; [:SA [:A]] -> [:SA [:A] [:A]] (not meaningful really, but outlaw seperately)
(have-common-parent? [:SA [:cut [:A]]] [1 1] [1 nil]) ;; -> [:SA [:cut [:A] [:A]]]
(have-common-parent? [:SA [:cut [:A] [:cut [:B]]]]
                     [1 2 1] [1]) ;; -> false



;; ==========---------------------------------------------------------------------------

;; http://users.clas.ufl.edu/jzeman/graphicallogic/introduction.htm
;; LEGAL:
;;   [:SA [:A]] -> [:SA [:A] [:A]]
;;   [:SA [:A [:A]]] <- Not semantically meaningful (only allowing atoms and cuts)
;;   [:SA [:cut [:A]]]


(cuts-crossed [:SA
               [:A]
               [:cut [:A]]
               [:cut [:cut [:B]]]] [2 1])

(have-common-parent? [:SA [:A]]
                     [1]
                     [1 nil]) ;; Insert a node as a child of itself



;; A node may be copied if the copy is enclosed by at least all the same cuts


(defn valid-child-loc? [G path]
  (boolean
   (and
    ;; Child path must end with nil (for place as child)
    (nil? (last path))
    ;; Propositions or cuts are children of cuts or the sheet of assertion
    (#{:SA :cut} (first (get-in G (first (ancestor-paths path))))))))

(defn iteration [G src-path dest-path]
  {:pre [(not (is-root? G src-path)) ;; Protect root of our tree (:SA is itself a node)
         (have-common-parent? G src-path dest-path) ;; It's not
         (valid-child-loc? G dest-path)]
   :post []
   :docstring (str "Any subgraph P depending on node n may be copied"
                   "into any node depending on n.")}
  (insert G (get-in G src-path) dest-path))


(defn deiteration [G src-path]
  {:pre [(exists-ancestral-copy? G src-path)]
   :post []
   :docstring (str "Any subgraph P in node n may be erased if there is"
                   "a copy of it in a node ancestral to n."
                   "(A node on which n depends)")}
  (erase G src-path))


