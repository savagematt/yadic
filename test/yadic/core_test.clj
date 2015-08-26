(ns yadic.core-test
  (:require [midje.sweet :refer :all]
            [yadic.core :refer :all])
  (:import [java.util UUID ArrayList]
           [java.lang AutoCloseable]))

; Basic
; ==================================

(fact "A container calls activators to create instances"
  (let [activators (->activators :a (constantly 3))
        container  (->container activators)]
    (:a container) => 3))

(fact "You cannot add non-activators"
  (->activators :a "not an activator")
  => (throws IllegalArgumentException))

(fact "Activators can request other dependencies"
  (let [activators (->activators
                     :a (constantly 3)
                     :b (fn [container] (* 2 (:a container))))
        container  (->container activators)]
    (:a container) => 3
    (:b container) => 6))

(fact "Containers call activators lazily"
  (let [a-created  (atom false)
        activators (->activators
                     :a (fn [_] (reset! a-created true) "A")
                     :b (fn [_] "B"))
        container  (->container activators)]
    (:b container) => "B"
    @a-created => false))

(fact "Containers cache the results of calling activators"
  (let [activators  (->activators
                      :a (fn [_] (UUID/randomUUID)))
        container   (->container activators)
        first-value (:a container)]

    (:a container) => first-value))

(fact "Containers are maps, so destructuring works"
  (let [activators (->activators
                     :a (constantly 3)
                     :b (fn [{:keys [a]}] (* 5 a)))         ; <--- Destructuring here
        container  (->container activators)]
    (:b container) => 15))


; Activation using functions
; ==================================

(fact "fn->activator turns any function into an activator function"
  (let [activators (->activators
                     :a (constantly 3)
                     :b (constantly 5)
                     :c (fn->activator (fn [a b] (* a b))
                                       [:a :b]))
        container  (->container activators)]
    (:c container) => 15))

(fact "fn->activator throws exception if function arity doesn't match container keys"
  (->activators :a (fn->activator (fn [a b] nil)
                                  [:a]))
  => (throws IllegalArgumentException))

(fact "functions that aren't arity 1 will be rejected"
  (->activators :a (fn [a b])) => (throws IllegalArgumentException))

(fact "There's a handy macro for creating functions activators"
  (-> (->activators :a (act [b] (str "hello " b))
        :b (constantly "world"))
      (->container)
      (:a)) => "hello world")

; Keys
; ==================================

(fact "Calling keys doesn't instantiate anything"
  (let [a-created  (atom false)
        activators (->activators :a (fn [_] (reset! a-created true) "A"))
        container  (->container activators)]
    (keys container) => [:a]
    @a-created => false))

(fact "Calling contains? doesn't instantiate anything"
  (let [a-created  (atom false)
        activators (->activators :a (fn [_] (reset! a-created true) "A"))
        container  (->container activators)]
    (contains? container :a) => true
    @a-created => false))

(fact "Keys include parent container keys"
  (let [parent-container (->container (->activators :a (constantly "a")
                                        :b (constantly "b")))
        child-container  (->container parent-container (->activators :b (constantly "b")))]
    (keys child-container) => (contains #{:a :b} :in-any-order)))


; get
; ==================================

(fact "will return default value from get if key is not found"
  (get (->container empty-activators) :a :some-default-value)
  => :some-default-value

  (get (-> (->container empty-activators)
           (->container empty-activators))
       :a
       :some-default-value)
  => :some-default-value)

; Destructors
; ==================================

(fact "Activators can specify destructors, which are called when the container is closed"
  (let [close-was-called-with (atom nil)
        activators            (->activators :a
                                            (reify
                                              Activator
                                              (activate [this container] "the instance")
                                              (close [this instance]
                                                (reset! close-was-called-with instance))))
        container             (->container activators)]
    (:a container) => "the instance"
    (.close container)
    @close-was-called-with => "the instance"))

(fact "Destructors aren't called if the instance isn't realised"
  (let [close-was-called (atom false)
        activators       (->activators :a
                                       (reify
                                         Activator
                                         (activate [this container] "never activated")
                                         (close [this instance]
                                           (reset! close-was-called true))))
        container        (->container activators)]
    (.close container)
    @close-was-called => false))

(fact "Destructors are called in reverse order of activation"
  (let [close-order (atom [])
        ks          (map (comp keyword str) (range 100))
        activators  (->> ks
                         (map (fn [k]
                                (reify
                                  Activator
                                  (activate [this container] k)
                                  (close [this instance]
                                    (swap! close-order conj k)))))
                         (zipmap ks)
                         (->activators))
        container   (->container activators)]

    (doseq [k ks]
      (get container k))

    (.close container)

    @close-order => (reverse ks)))

(deftype ACloseableType [closed-atom]
  AutoCloseable
  (close [this]
    (reset! closed-atom true)))

(fact "Function activators that produce AutoCloseable instances will close the instance"
  (let [close-was-called (atom false)
        activators       (->activators :a (constantly (ACloseableType. close-was-called)))
        container        (->container activators)]
    (:a container) => truthy
    (.close container)
    @close-was-called => true))

(fact "Container is AutoCloseable, and will be closed when using with-open"
  (let [close-was-called-with (atom nil)
        activators            (->activators :a (reify
                                                 Activator
                                                 (activate [this container] "the instance")
                                                 (close [this instance]
                                                   (reset! close-was-called-with instance))))]

    (with-open [container (->container activators)]
      (:a container) => "the instance")

    @close-was-called-with => "the instance"))

(fact "Container only closes its own instances, not instances in the parent"
  (let [close-was-called  (atom false)
        parent-activators (->activators :a (constantly (ACloseableType. close-was-called)))
        parent-container  (->container parent-activators)
        child-container   (->container parent-container empty-activators)]

    (:a child-container) => truthy

    (.close child-container)

    @close-was-called => false))

; Decoration
; ==================================

(fact "Decoration works"
  (let [activators (-> (->activators :a (constantly "a")
                                     :b (fn [container] (str "b saw " (:a container))))
                       (decorate :a (fn [r] (str "decorated " (:a r))))
                       (decorate :a (fn [r] (str "decorated " (:a r)))))
        container  (->container activators)]
    (:a container) => "decorated decorated a"
    (:b container) => "b saw decorated decorated a"))

(fact "It's possible to decorate a value from a parent container"
  (let [parent-activators (->activators :a (constantly "hello"))
        parent-container  (->container parent-activators)
        child-container   (->container parent-container
                                       (-> empty-activators
                                           (decorate parent-container :a (act [a] (str a " world")))
                                           (decorate :a (act [a] (str a "!!!")))))]

    (:a child-container) => "hello world!!!"))

(future-fact "Destructors still get called on decorated activators")

; Activation using IDeref
; ==================================

(fact "IDerefs can be used as activators"
  (let [a-promise  (promise)
        activators (->activators :atom (atom "atom")
                                 :promise a-promise
                                 :delay (delay "delay")
                                 :future (future "future")
                                 :never-returns (deref->activator (future (Thread/sleep 100000000))
                                                                  1
                                                                  "timed-out"))
        container  (->container activators)]

    (deliver a-promise "promise")

    (:atom container) => "atom"
    (:promise container) => "promise"
    (:delay container) => "delay"
    (:future container) => "future"
    (:never-returns container) => "timed-out"))
