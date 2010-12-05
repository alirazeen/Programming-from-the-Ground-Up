(declare (uses amb))

(display (let (
     (a (amb 1 2 3 4 5))
     (b (amb 4 5 6 7)))
   (amb-assert (> a b))
   (list a b)
 ))
(newline)
