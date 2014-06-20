(ns hungarian.core)

;; Sorry for the few comments and explanations but I just carved it out of my program to publish it and I still got lots of other things to do.

;; The algo is based on the https://en.wikipedia.org/wiki/Hungarian_algorithm article while the assignment of zeros is based on the explanations in https://community.topcoder.com/tc?module=Static&d1=tutorials&d2=maxFlow

;; The algo is IMO O(n^5) which is fast enough for my intents and purposes. 

;; Testing was only done marginally.
;; t1, t2 and t4 were checked with 
;; http://www.hungarianalgorithm.com/

;; http://www.hungarianalgorithm.com/solve.php?c=4-30-83-57--13-36-84-68--40-24-84-38--79-49-29-71&random=1
(def t1 [[4	30	83	57]
        [13	36	84	68]
        [40	24	84	38]
        [79	49	29	71]
        ]) ;;107

;; http://www.hungarianalgorithm.com/solve.php?c=23-72-76-60-90-26-77-2-82-70--74-2-49-44-55-67-49-68-59-77--63-43-10-66-30-37-2-71-13-58--50-23-60-6-38-92-75-77-12-8--71-40-22-38-78-31-37-15-4-40--98-99-64-25-88-33-60-28-61-10--61-49-61-73-17-29-43-10-54-44--92-23-69-12-97-62-15-99-80-70--36-10-66-45-64-1-92-73-44-34--22-97-3-16-37-49-31-69-26-66&random=1
(def t2 [ [ 23	72	76	60	90	26	77	2	82	70]
          [74	2	49	44	55	67	49	68	59	77]
          [63	43	10	66	30	37	2	71	13	58]
          [ 50	23	60	6	38	92	75	77	12	8]
          [ 71	40	22	38	78	31	37	15	4	40]
          [ 98	99	64	25	88	33	60	28	61	10]
          [ 61	49	61	73	17	29	43	10	54	44]
          [ 92	23	69	12	97	62	15	99	80	70]
          [ 36	10	66	45	64	1	92	73	44	34]
          [22	97	3	16	37	49	31	69	26	66]]) ;;89

(def t3 [[43 98 65 46 71 25 73 9 3 92 30 93 54 92 44 47 13 33 31 23] [18 23 62 58 26 5 99 72 28 63 7 97 74 83 7 47 33 37 80 23] [10 48 82 90 11 21 49 90 74 98 93 94 28 60 74 42 6 45 54 13] [65 41 72 12 30 52 53 61 48 67 44 3 86 39 32 5 61 98 85 78] [84 48 40 0 39 45 31 16 16 76 54 98 77 38 44 90 21 89 42 49] [84 8 15 78 25 89 99 79 90 51 38 21 87 89 94 87 64 56 4 82] [2 31 1 32 16 22 87 67 81 44 5 71 89 11 50 22 78 13 4 85] [50 78 83 12 46 39 50 5 55 3 37 41 45 83 2 14 44 10 43 73] [24 30 79 97 70 36 5 72 60 21 32 57 97 5 16 55 19 42 93 35] [4 96 47 28 80 64 45 77 80 36 85 75 24 57 34 56 8 41 38 78] [89 53 24 38 60 75 30 15 48 11 43 42 60 37 43 71 49 11 96 40] [80 57 93 32 18 82 32 97 82 77 47 63 47 37 82 49 23 63 97 96] [76 90 24 57 76 39 74 69 69 40 32 99 37 71 62 91 41 83 7 82] [35 30 26 29 63 76 13 87 45 8 88 40 87 44 56 78 78 70 4 2] [36 40 80 36 10 73 26 96 72 69 71 4 53 67 55 67 31 51 89 53] [96 74 14 53 79 78 77 3 64 19 3 60 46 93 3 55 72 23 12 27] [21 98 47 26 72 20 4 7 35 58 36 93 21 45 91 56 51 72 84 79] [18 89 76 4 43 26 93 40 88 50 22 66 33 39 24 51 14 96 69 87] [22 67 44 76 33 25 28 61 24 48 2 83 62 19 3 49 93 74 97 42] [20 75 29 81 15 41 37 8 42 98 35 47 80 7 6 88 21 87 48 74]])

;; http://www.hungarianalgorithm.com/solve.php?c=51-33-96-94--9-50-60-94--96-57-24-8--24-2-19-14&random=1
(def t4 (mapv vec (partition 4 [51	33	96	94
                                9	50	60	94
                                96	57	24	8
                                24	2	19	14])))
;; cost 69


(def t5 [[55333 50777 55180 54881 100000 100000] [58479 58812 58583 60020 100000 100000] [33333 58666 33437 34875 100000 100000] [0 54555 25152 26881 100000 100000] [33354 58687 33458 34895 100000 100000] [51527 55305 51375 50354 100000 100000]])

(defn- zero-indizes [m]
  (let [cnt (count (first m))]
    (loop [i 0
           acc (transient [])]
      (if (< i (* cnt cnt))
        (let [r (quot i cnt)
              c (mod i cnt)]
          (if (zero? (get-in m [r c]))
            (recur (inc i) (conj! acc i))
            (recur (inc i) acc)))
        (persistent! acc)))))

(defn- find-path [r2c c2r fringe]
  (when (seq fringe)
    (let [[n rows-visited cols-visited path] (first fringe)
          row? (= (count rows-visited) (count cols-visited))
          rest-fringe (subvec fringe 1)
          reachable (if row?
                      (remove cols-visited (get r2c n))
                      (remove rows-visited (get c2r n)))
          continue? (seq reachable)
          npath (when continue?
                  (conj path n))
          [nrv ncv] (when continue?
                      (if row?
                        [(conj rows-visited n) cols-visited]
                        [rows-visited (conj cols-visited n)]))
          add-fringe (map #(vector % nrv ncv npath) reachable)]
      (if row?
        (recur r2c c2r (reduce conj rest-fringe add-fringe))
        (if (some #(== -1 %) reachable)
          npath
          (recur r2c c2r (reduce conj rest-fringe add-fringe)))))))

(defn- continuous-augment-path [r2c c2r assignments]
  (let [start-fringe (->> (keys r2c)
                          (remove assignments)
                          (mapv #(vector % #{} #{} [])))
        p (find-path r2c c2r start-fringe)
        [nr2c nc2r] (->> (map vector p (rest p) (range))
                         (reduce (fn [[r1 c1] [s t i]]
                                   (if (even? i)
                                     [(update-in r1 [s] #(remove #{t} %))
                                      (assoc c1 t [s])]
                                     [(update-in r1 [t] conj s)
                                      c1]))
                                 [r2c c2r]))
        na (into assignments (map #(subvec p % (+ % 2)) (range 0 (dec (count p)) 2)))]
    (if p
      (recur nr2c nc2r na)
      assignments))
  )

(defn- find-max-zero-assignment [idz cnt]
  (let [idc-ps (map (juxt #(quot % cnt) #(mod % cnt)) idz)
        r2c (->> (group-by first idc-ps)
                  (map (fn [[k v]]
                         [k (mapv second v)]))
                  (into {}))
        c2r (->> (group-by second idc-ps)
                 (map (fn [[k v]]
                        [k (conj (mapv first v) -1)]))
                 (into {}))
        assignments (continuous-augment-path r2c c2r {})]
    assignments))

(defn- mark-columns [idz cnt marked-rows]
  (->> idz
       (filter #(contains? marked-rows (quot % cnt)))
       (map #(mod % cnt))
       set))

(defn- mark-rows [assigned-cells cnt marked-cols]
  (->> (filter #(contains? marked-cols (mod % cnt)) assigned-cells)
       (map #(quot % cnt))
       set))

(defn- cover-zeros [idz assignments cnt]
  (let [mza (for [[r c] assignments]
              (+ c (* cnt r)))
        marked-rows (set (remove assignments (range cnt)))
        union #(reduce conj %1 %2)
        [mr mc] (loop [mr marked-rows
                       mc #{}
                       last-rcnt 0
                       last-ccnt 0]
                  (if (and (== (count mc) last-ccnt)
                           (== (count mr) last-rcnt))
                    [mr mc]
                    (let [mcn (mark-columns idz cnt mr)
                          mrn (mark-rows mza cnt mcn)]
                      (recur (union mr mrn) (union mc mcn) (count mr) (count mc)))))]
    [(set (remove mr (range cnt))) mc]))

(defn- subtract-min-from-row [row]
  {:pre [(every? number? row)]}
  (let [m (reduce min (get row 0) (subvec row 1))]
    (if (pos? m)
      (mapv #(- % m) row)
      row)))

(defn- subtract-min-from-cols [m]
  {:pre [(every? vector? m)]}
  (let [r (count m)
        c (count (first m))
        col-mins (for [i (range c)]
                   (reduce min (map #(get % i) m)))]
    (mapv (fn [row]
           (mapv - row col-mins)) m)))

(defn- prepare-matrix [m]
  {:pre [(every? (every-pred vector? #(every? number? %)) m)]}
  (->> m
       (mapv subtract-min-from-row)
       (subtract-min-from-cols)))

(defn- minimize-step [m]
  (let [cnt (count (first m))
        idz (zero-indizes m)
        assignments (find-max-zero-assignment idz cnt)]
    (if (< (count assignments) cnt) 
      (let [[cvd-rows cvd-cols] (cover-zeros idz assignments cnt)
            uncovered-idc (for [i (range cnt)
                                j (range cnt)
                                :when (not (or (cvd-rows i)
                                               (cvd-cols j)))]
                            [i j])
            new-min (reduce min (map #(get-in m %) uncovered-idc))
            m0 (reduce #(update-in %1 %2 - new-min) m uncovered-idc)
            m1 (->> (for [r cvd-rows
                          c cvd-cols]
                      [r c])
                    (reduce #(update-in %1 %2 + new-min) m0))]
        (recur m1))
      assignments)
    ))

(defn minimize [m]
  {:pre [(= (count m) (count (first m)))]
   :post [(= (count m)
             (count (:assignments %)))
          (= (range (count m)) (sort (vals (:assignments %))))]}
  (let [pm (prepare-matrix m)
        mza (minimize-step pm)
        cnt (count (first m))]
    {:cost (reduce + (map #(get-in m %) mza))
     :assignments mza}))

(defn minimize-rectangle [r]
  (let [rs (count r)
        cs (count (first r))
        mx-value (reduce max (map #(reduce max %) r))
        m (if (< rs cs)
            (reduce conj r (repeat (- cs rs) (vec (repeat cs mx-value))))
            (if (== rs cs)
              r
              (mapv #(reduce conj % (repeat (- rs cs) mx-value)) r)))]
    (minimize m)))
