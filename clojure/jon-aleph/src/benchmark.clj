(ns benchmark
  (:import (java.security MessageDigest))
  (:use lamina.core aleph.tcp gloss.core)
  (:gen-class))

(defn verify [input nonce]
  (let [md (. MessageDigest (getInstance "SHA-256"))]
       (. md (update (.getBytes input)))
       (. md (update (.getBytes nonce)))
       (= 0 (last (. md digest)))))

(defn message-received [message]
  (let [nonce (some #(when (verify message %) %)
                    (map #(. Integer (toHexString %)) (iterate inc 0)))]
    (str message ":" nonce)))

(defn handler [ch client-info]
  (enqueue ch "ok\n")
  (receive-all ch #(enqueue ch (message-received %))))

(defn -main [& args]
  (println "ready")
  (start-tcp-server handler {:port 1337, :frame (string :ascii)}))
