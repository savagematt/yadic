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
