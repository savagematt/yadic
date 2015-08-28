(ns yadic.component-test
  (:require [clojure.test :refer (deftest is are)]
            [clojure.set :refer (map-invert)]
            [yadic.component :as component]))

(def ^:dynamic *log* nil)

(defn- log [& args]
  (when (thread-bound? #'*log*)
    (set! *log* (conj *log* args))))

(defn- ordering
  "Given an ordered collection of messages, returns a map from the
  head of each message to its index position in the collection."
  [log]
  (into {} (map-indexed (fn [i [message & _]] [message i]) log)))

(defn before?
  "In the collection of messages, does the message beginning with
  symbol a come before the message begging with symbol b?"
  [log sym-a sym-b]
  (let [order (ordering log)]
    (< (get order sym-a) (get order sym-b))))

(defn started? [component]
  (true? (::started? component)))

(defn stopped? [component]
  (false? (::started? component)))

(defrecord ComponentA [state]
  component/Lifecycle
  (start [this]
    (log 'ComponentA.start this)
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentA.stop this)
    (assoc this ::started? false)))

(defn component-a []
  (->ComponentA (rand-int Integer/MAX_VALUE)))

(defrecord ComponentB [state a]
  component/Lifecycle
  (start [this]
    (log 'ComponentB.start this)
    (assert (started? a))
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentB.stop this)
    (assert (started? a))
    (assoc this ::started? false)))

(defn component-b []
  (component/using
    (map->ComponentB {:state (rand-int Integer/MAX_VALUE)})
    [:a]))

(defrecord ComponentC [state a b]
  component/Lifecycle
  (start [this]
    (log 'ComponentC.start this)
    (assert (started? a))
    (assert (started? b))
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentC.stop this)
    (assert (started? a))
    (assert (started? b))
    (assoc this ::started? false)))

(defn component-c []
  (component/using
    (map->ComponentC {:state (rand-int Integer/MAX_VALUE)})
    [:a :b]))

(defrecord ComponentD [state my-c b]
  component/Lifecycle
  (start [this]
    (log 'ComponentD.start this)
    (assert (started? b))
    (assert (started? my-c))
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentD.stop this)
    (assert (started? b))
    (assert (started? my-c))
    (assoc this ::started? false)))

(defn component-d []
  (map->ComponentD {:state (rand-int Integer/MAX_VALUE)}))

(defrecord ComponentE [state]
  component/Lifecycle
  (start [this]
    (log 'ComponentE.start this)
    (assoc this ::started? true))
  (stop [this]
    (log 'ComponentE.stop this)
    (assoc this ::started? false)))

(defn component-e []
  (map->ComponentE {:state (rand-int Integer/MAX_VALUE)}))

(defrecord System1 [d a e c b]                              ; deliberately scrambled order
  component/Lifecycle
  (start [this]
    (log 'System1.start this)
    (component/start-system this))
  (stop [this]
    (log 'System1.stop this)
    (component/stop-system this)))

(defn system-1 []
  (map->System1 {:a (component-a)
                 :b (component-b)
                 :c (component-c)
                 :d (component/using (component-d)
                                     {:b    :b
                                      :my-c :c})
                 :e (component-e)}))

(defmacro with-log [& body]
  `(binding [*log* []]
     ~@body
     *log*))

(deftest components-start-in-order
  (let [log (with-log (component/start (system-1)))]
    (are [k1 k2] (before? log k1 k2)
                 'ComponentA.start 'ComponentB.start
                 'ComponentA.start 'ComponentC.start
                 'ComponentB.start 'ComponentC.start
                 'ComponentC.start 'ComponentD.start
                 'ComponentB.start 'ComponentD.start)))

(deftest all-components-started
  (let [system (component/start (system-1))]
    (doseq [component (vals system)]
      (is (started? component)))))

(deftest all-components-stopped
  (let [system (component/stop (component/start (system-1)))]
    (doseq [component (vals system)]
      (is (stopped? component)))))

(deftest dependencies-satisfied
  (let [system (component/start (component/start (system-1)))]
    (are [keys] (started? (get-in system keys))
                [:b :a]
                [:c :a]
                [:c :b]
                [:d :my-c])))

(defrecord ErrorStartComponentC [state error a b]
  component/Lifecycle
  (start [_this]
    (throw error))
  (stop [this]
    this))

(defn error-start-c [error]
  (component/using
    (map->ErrorStartComponentC {:error error})
    [:a :b]))

(defn setup-error
  ([]
   (setup-error (ex-info "Boom!" {})))
  ([error]
   (try (component/start
          (assoc (system-1) :c (error-start-c error)))
        (catch Exception e e))))

(str "Yadic throws less structured errors than Component"
     #_(deftest error-thrown-with-partial-system
         (let [ex (setup-error)]
           (is (started? (-> ex ex-data :system :b :a)))))

     #_(deftest error-thrown-with-component-dependencies
         (let [ex (setup-error)]
           (is (started? (-> ex ex-data :component :a)))
           (is (started? (-> ex ex-data :component :b)))))

     #_(deftest error-thrown-with-cause
         (let [error (ex-info "Boom!" {})
               ex    (setup-error error)]
           (is (identical? error (.getCause ^Exception ex)))))

     #_(deftest error-is-from-component
         (let [error (ex-info "Boom!" {})
               ex    (setup-error error)]
           (is (component/ex-component? ex))))

     #_(deftest error-is-not-from-component
         (is (not (component/ex-component? (ex-info "Boom!" {})))))

     #_(deftest remove-components-from-error
         (let [error                 (ex-info (str (rand-int Integer/MAX_VALUE)) {})
               ^Exception ex         (setup-error error)
               ^Exception ex-without (component/ex-without-components ex)]
           (is (contains? (ex-data ex) :component))
           (is (contains? (ex-data ex) :system))
           (is (not (contains? (ex-data ex-without) :component)))
           (is (not (contains? (ex-data ex-without) :system)))
           (is (= (.getMessage ex)
                  (.getMessage ex-without)))
           (is (= (.getCause ex)
                  (.getCause ex-without)))
           (is (java.util.Arrays/equals
                 (.getStackTrace ex)
                 (.getStackTrace ex-without))))))

(defrecord System2b [one]
  component/Lifecycle
  (start [this]
    (assert (started? (get-in one [:b :a])))
    this)
  (stop [this]
    (assert (started? (get-in one [:b :a])))
    this))

(defn system-2 []
  (component/system-map :alpha (system-1)
                        :beta (component/using (->System2b nil)
                                               {:one :alpha})))

(deftest composed-systems
  (let [system (component/start (system-2))]
    (is (started? (get-in system [:beta :one :d :my-c])))))

(str "We don't support update-system. It's not something that fits in yadic's world."
     "Instances in a yadic Container are not expected to be homogenous. They're just objects,"
     "and not expected to all be maps, or all be Lifecyle, or whatever. So performing the"
     "same operation on every instance doesn't make sense."
     #_(defn increment-all-components [system]
         (component/update-system
           system (keys system) update-in [:n] inc))

     #_(defn assert-increments [system]
         (are [n keys] (= n (get-in system keys))
                       11 [:a :n]
                       11 [:b :a :n]
                       11 [:c :a :n]
                       11 [:c :b :a :n]
                       11 [:e :d :b :a :n]
                       21 [:b :n]
                       21 [:c :b :n]
                       21 [:d :b :n]
                       31 [:c :n]
                       41 [:d :n]
                       51 [:e :n]))

     #_(deftest update-with-custom-function-on-maps
         (let [system {:a {:n 10}
                       :b (component/using {:n 20} [:a])
                       :c (component/using {:n 30} [:a :b])
                       :d (component/using {:n 40} [:a :b])
                       :e (component/using {:n 50} [:b :c :d])}]
           (assert-increments (increment-all-components system)))))


(str "I don't propose to implement system-using"
     #_(deftest t-system-using
         (let [dependency-map {:b [:a]
                               :c [:a :b]
                               :d {:a :a :b :b}
                               :e [:b :c :d]}
               system         {:a {:n 10}
                               :b {:n 20}
                               :c {:n 30}
                               :d {:n 40}
                               :e {:n 50}}
               system         (component/system-using system dependency-map)]
           (assert-increments (increment-all-components system)))))

(defrecord ComponentWithoutLifecycle [state])

(deftest component-without-lifecycle
  (let [c (->ComponentWithoutLifecycle nil)]
    (is (= c (component/start c)))
    (is (= c (component/stop c)))))

(defrecord ComponentReturningNil [state]
  component/Lifecycle
  (start [this]
    nil)
  (stop [this]
    nil))

(deftest component-returning-nil
  "This test has been modified from the original to match yadic's exception behaviour"
  (let [a (->ComponentReturningNil nil)
        s (component/system-map :a a)]
    (try (component/start s)
         (is false "Expected exception")
         (catch UnsupportedOperationException e
           (is (= "Activator returned nil for :a" (.getMessage e)))))))

