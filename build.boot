(set-env!
  :source-paths   #{"src"}
  :resource-paths #{"resources"}
  :dependencies
  '[[org.clojure/clojurescript "1.7.48"]
    [adzerk/boot-cljs          "0.0-3308-0" :scope "test"]
    [adzerk/boot-reload        "0.3.1"      :scope "test"]
    [pandeiro/boot-http        "0.6.3"      :scope "test"]])

(require
  '[adzerk.boot-cljs      :refer [cljs]]
  '[adzerk.boot-reload    :refer [reload]]
  '[pandeiro.boot-http    :refer [serve]])

(deftask dev []
  (comp (serve :dir "target/")
        (watch)
        (reload :on-jsload 'hack.core/main)
        (cljs :source-map true :optimizations :none)))
