(ns user
  (:import [java.net URL]
           [java.util.regex Pattern])
  (:require [clojure.contrib.str-utils2 :as str2]))

; === URL helpers ===

(defn query-to-map [query]
  (when query
    (let [decode #(java.net.URLDecoder/decode % "UTF-8")]
      (apply array-map
             (mapcat #(let [[k v] (str2/split % #"=" 2)]
                        [(decode k) (when v (decode v))])
                     (str2/split query #"&"))))))

(defn map-to-query [qmap]
  (let [encode #(java.net.URLEncoder/encode % "UTF-8")]
    (apply str (next (mapcat #(concat ["&" (encode (key %))]
                                      (when (val %)
                                        ["=" (encode (val %))]))
                             qmap)))))

(defn mergeurl [initurl & diffqparts]
  (let [[base oldqstr] (str2/split initurl #"\?" 2)
        diffqmap       (apply array-map diffqparts)
        newqmap        (merge (query-to-map oldqstr) diffqmap)]
    (str base (when (seq newqmap) (str "?" (map-to-query newqmap))))))

(assert (= (mergeurl "http://com/" "foo" "bar") "http://com/?foo=bar"))
(assert (= (mergeurl "http://com/") "http://com/"))
(assert (= (mergeurl "http://com/?") "http://com/?"))
(assert (= (mergeurl "http://com/?" "foo" "bar") "http://com/?foo=bar&"))
(assert (= (mergeurl "http://com/?foo=baz" "foo" "bar") "http://com/?foo=bar"))
(assert (= (mergeurl "http://com/?foo=baz&bing=bang" "foo" "bar")
           "http://com/?foo=bar&bing=bang"))
(assert (= (mergeurl "http://com/?foo=baz&bing=bang")
           "http://com/?foo=baz&bing=bang"))
(assert (= (mergeurl "http://com/?foo=baz&bing=") "http://com/?foo=baz&bing="))
(assert (= (mergeurl "http://com/?foo=baz&bing") "http://com/?foo=baz&bing"))
(assert (= (mergeurl "http://com/?foo=baz") "http://com/?foo=baz"))

(defn series-url [state-id]
  (str "http://data.bls.gov/PDQ/servlet/SurveyOutputServlet?"
       (map-to-query
         {"output_type" "default",
          "years_option" "all_years",
          "output_view" "data",
          "periods_option" "all_periods",
          "output_format" "text",
          "delimiter" "comma",
          "reformat" "true",
          "request_action" "get_data",
          "initial_request" "false",
          "data_tool" "JavaSelAccess",
          "series_id" (format "LAUST%02d000003" state-id)})))

(defn fetch-content [url-str]
  (let [stream (.getContent (URL. url-str))]
    (apply str (map char (take-while #(>= % 0) (repeatedly #(.read stream)))))))

(defn extract-table [s]
  (map #(str2/split % #",") (re-seq #"(?m)^\d.*" s)))

(defn extract-meta [meta-name s]
  (let [ptn (re-pattern (str #"(?m)^"
                             (Pattern/quote meta-name)
                             #":(?:&nbsp;)*(.*)"))]
    (second (re-find ptn s))))

(defn area-series [area-id]
  (let [string (fetch-content (series-url area-id))]
    [(extract-meta "Area" string)
     (for [[year period ttl emp unemp unemp-rate] (extract-table s)
           :when (not= period "Annual")]
       [(str year " " period)
        (Float/parseFloat (str2/replace unemp-rate #"\(.*\)" ""))])]))

; order-preserving maximal subset:
; (reduce #(filter %2 %1) (first periods-2d) (map set (next periods-2d)))

(defn merge-series [series-map]
  (let [periods-2d (map #(map first (val %)) series-map)
        period-set (set (reduce #(filter %2 %1) (map set periods-2d)))]
    {:periods (filter period-set (map first (val (first sm))))
     :areas (keys series-map)
     :grid (vec (map (fn [series]
                         (vec (map second (filter #(period-set (first %))
                                                  series))))
                     (vals series-map)))}))


;(def sm (into {} (map area-series (range 1 4))))
;(merge-series (into {} (map area-series (range 1 57))))
