diff

test: (f (g x) (g (g x)) (h (g (g x)) (g x)) ((g x) (g x)))
my: (let* ([g0 (g g1)] [g1 (g x)]) (f g1 g0 (h g0 g1) (g1 g1)))
mayer:(let* ([g0 (g x)] [g1 (g g0)]) (f g0 g1 (h g1 g0) (g0 g0)))
i think this is okay

test: (+ (* (- x y) (* x x)) (* x x) (foo (- x y)) (goo (* (- x y) (* x x))))
my:(let* ([g2 (* g3 g4)] [g4 (* x x)] [g3 (- x y)])
  (+ g2 g4 (foo g3) (goo g2)))
mayer: (let* ([g2 (- x y)] [g3 (* x x)] [g4 (* g2 g3)])
  (+ g4 g3 (foo g2) (goo g4)))
i think this is okay

test: (list (cons (quote a) (quote b)) (cons (quote a) (quote b)) (list (cons (quote a) (quote b)) (cons (quote a) (quote b))) (list (list (cons (quote a) (quote b)) (cons (quote a) (quote b)))))
my:(let* ([g5 (list g6 g6)] [g6 (cons 'a 'b)])
  (list g6 g6 g5 (list g5)))
mayer:(let* ([g5 (cons 'a 'b)] [g6 (list g5 g5)])
  (list g5 g5 g6 (list g6)))
i think this is okay

test: (f (c (a b)) (a b) (c (a b)))
my:   (let* ([g7 (c g8)] [g8 (a b)]) (f g7 g8 g7))
mayer:(let* ([g7 (a b)] [g8 (c g7)]) (f g8 g7 g8))
i think this is okay

test: (f (c (a b)) (a b) (c (a b)) (a b))
my:   (let* ([g9 (c g10)] [g10 (a b)]) (f g9 g10 g9 g10))
mayer:(let* ([g9 (c g10)] [g10 (a b)]) (f g9 g10 g9 g10))
i think this is okay

test:(a (f (+ (g) (h)) 1 (g (+ (g) (h)) (+ (g) (h))) 3 (g (+ (g) (h)) (+ (g) (h))
my:(let* ([g11 (g g12 g12)] [g12 (+ (g) (h))])
  (a (f g12 1 g11 3 g11 g12)))
mayer:(let* ([g11 (+ (g) (h))] [g12 (g g11 g11)])
  (a (f g11 1 g12 3 g12 g11)))
i think this is okay!

test: (* (+ (+ 1 (+ 2 (- 3 (+ 4 5))))) (+ (+ 1 (+ 2 (- 3 (+ 4 5))))) (+ (+ 11 (+ 22 (- 45 (+ 4 5))))) (+ (+ 113 (+ 220 (- 3 (+ 4 5))))))
my:(let* ([g13 (+ (+ 1 (+ 2 g14)))]
       [g14 (- 3 g15)]
       [g15 (+ 4 5)])
  (* g13
     g13
     (+ (+ 11 (+ 22 (- 45 g15))))
     (+ (+ 113 (+ 220 g14)))))
mayer:(let* ([g13 (+ 4 5)]
       [g14 (- 3 g13)]
       [g15 (+ (+ 1 (+ 2 g14)))])
  (* g15
     g15
     (+ (+ 11 (+ 22 (- 45 g13))))
     (+ (+ 113 (+ 220 g14)))))
i think this is okay 

test:((+ 1 (- 2 3)) (+ 1 (- 2 3)) (- 5 (- 2 3)) (- 5 (- 2 3)) 89))
my:(let* ([g16 (- 5 g17)] [g18 (+ 1 g17)] [g17 (- 2 3)])
  (g18 g18 g16 g16 89))
mayer: (let* ([g16 (- 2 3)] [g17 (+ 1 g16)] [g18 (- 5 g16)])
  (g17 g17 g18 g18 89))
i think this is okay

test:((+ 1 (- 2 3) (- 2 3)) (+ 1 (- 2 3) (- 2 3)))
my:   (let* ([g19 (+ 1 g20 g20)] [g20 (- 2 3)]) (g19 g19))
mayer:(let* ([g19 (- 2 3)] [g20 (+ 1 g19 g19)]) (g20 g20))
i think this is ok
