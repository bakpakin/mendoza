###
### mendoza/watch-cache.janet
### Copyright © Calvin Rose 2019
###

# A little cache to allow easy unloading of required
# and imported modules. This is used for mendoza's watch
# functionality.

(def cache @{})

(defn clean-modules
  "Unload all modules that have their value in the cache."
  []
  (loop [[k v] :pairs module/cache
         :when (cache v)]
    (put module/cache k nil))
  (loop [k :keys cache] (put cache k nil)))
