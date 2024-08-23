(ns com.ambrosebs.map.util)

(defmacro dbg [f]
  (let [m (meta &form)]
    `(let [r# ~f]
       (println "DBG: " ~(str *ns* ":" (:line m) ":" (:column m))
                '~f r#)
       r#)))
