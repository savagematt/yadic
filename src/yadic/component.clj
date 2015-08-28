(ns yadic.component
  (:require [yadic.core :as yadic])
  (:import [java.lang AutoCloseable]
           [yadic.core Container]))

(defprotocol
  Lifecycle
  (start [this])
  (stop [this]))

(defn component->activator [component dependencies]
  (if (not (satisfies? Lifecycle component))
    (yadic/concrete component)
    (let [dependencies-map (if (map? dependencies)
                             dependencies
                             (zipmap dependencies dependencies))]
      (reify
        yadic/Activator
        (activate [_this container]
          (->> dependencies-map
               (map (fn [[alias k]]
                      [alias (get container k)]))
               (reduce
                 (fn [c [k v]] (assoc c k v))
                 component)
               (start)))

        (close [_this instance]
          (stop instance))))))

(defn using [component dependencies]
  (component->activator component dependencies))

(extend-protocol Lifecycle
  Container
  (start [this]
    (yadic/eagerly-instantiate this))
  (stop [^AutoCloseable this]
    (.close this))
  Object
  (start [this]
    this)
  (stop [^AutoCloseable this]
    this))

(defn- system-map->container* [m]
  (yadic/->container
    (reduce-kv
      (fn [as k x]
        (assoc as
          k (if (satisfies? yadic/Activator x)
              x
              (component->activator x []))))
      (yadic/->activators)
      m)))

(defn start-system [system]
  (-> (if (instance? Container system)
        system
        (system-map->container* system))
      (yadic/eagerly-instantiate)))

(defn stop-system [^AutoCloseable system]
  (.close system))

(defn system-map [& keyvals]
  (system-map->container* (apply hash-map keyvals)))
