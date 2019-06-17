###
### mendoza/watch-cache.janet
### Copyright © Calvin Rose 2019
###

# A little cache to allow easy unloading of required
# and imported modules. This is used for mendoza's watch
# functionality.

(var cache :private @{})

(defn add
  "Add something to the cache."
  [x]
  (put cache x true)
  x)

(defn clean
  "Unload all modules that have their value in the cache."
  []
  (loop [[k v] :in (pairs module/cache)]
    (if (cache v)
      (put module/cache k nil)))
  (set cache @{}))
