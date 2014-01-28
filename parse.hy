#!/usr/bin/env hy

(import json)
(import re)
(import sys)

(defn cons [head tail]
    (do (setv ret [head])
        (.extend ret tail)
        ret))

(defn head [xs]
    (get xs 0))

(defn tail [xs]
    (slice xs 1))

(defn unnest [xs]
    (do (setv ret [])
        (for (x xs)
             (.extend ret x))
        ret))

;;;;;;;;;;;;

(defn get-data [path]
    (with [[f (open path)]]
        (do (setv text (.read f))
            (setv text (.decode text "utf8"))
            (setv text (clean-control-chars text))
            (->> (-> (get-headers text)
                     (get-name-indexes))
                 (get-group-slices text)
                 (to-dict)
                 (fix-sidepanel)
                 (split-rows)
                 (split-names)
                 ))))

(defn clean-control-chars [text]
    (->> (re.sub r"\r\n" "\n" text)
         (re.sub r"\\\n" "\n")
         (re.sub r"\\[^'u\\\s][^\\\s]*" "")
         (re.sub r" *\n *\n+ *" "\n\n")
         (re.sub r"\s*$" "")))

(defn get-headers [text]
    (list (re.finditer r"side (\d|one) (upper|lower)" text)))

(defn get-name-indexes [text-matches]
    (cond
        [(empty? text-matches) []]
        [(= (len text-matches) 1) [[(.start (get text-matches 0)) -1]]]
        [True (cons [(.start (get text-matches 0)) (.start (get text-matches 1))]
                    (get-name-indexes (slice text-matches 1)))]))

(defn get-group-slices [text slices]
    (list-comp (slice text (get s 0) (get s 1))
               (s slices)))

(defn to-dict [section-texts]
    (if (empty? section-texts)
        []
        (cons (->> (let [[text (head section-texts)]
                         [split (.split text "\n\n" 1)]]
                        {"sidepanel" (get split 0)
                         "names" (get split 1)}))
              (to-dict (tail section-texts)))))

(defn fix-sidepanel [sections]
    (if (empty? sections)
        []
        (cons (let [[sec (head sections)]
                    [sidepanel (get sec "sidepanel")]
                    [sidepanel (.replace sidepanel "one" "1")]
                    [split (.split sidepanel " ")]]
                   {"side" (get split 1)
                    "panel" (get split 2)
                    "names" (get sec "names")})
              (fix-sidepanel (tail sections)))))

(defn split-rows [sections]
    (do (setv acc [])
        (for [sec sections]
             (.extend acc (list-comp {"side" (get sec "side")
                                      "panel" (get sec "panel")
                                      "row" row
                                      "names" names}
                                     [(, row names) (enumerate (.split (get sec "names") "\n\n"))])))
        acc))

(defn split-names [rows]
    (do (setv acc [])
        (for [row rows] 
             (.extend acc (list-comp {"side" (get row "side")
                                      "panel" (get row "panel")
                                      "row" (get row "row")
                                      "col" col
                                      "name" name}
                                     [(, col name) (enumerate (.split (get row "names") "  "))])))
        acc))


(if (= __name__ "__main__")
    (print (-> (get-data (get sys.argv 1))
               (json.dumps False True True True None True))))

