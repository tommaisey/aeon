(let ((f (mul-add (lf-cub kr (mul-add (lf-cub kr 0.2 0) 8 10) 0) 400 800)))
  (mul (lf-cub ar f 0) 0.1))

(mul (lf-cub ar (mul-add (lf-cub kr 0.2 0) 400 800) 0) 0.1)
(mul (lf-cub ar 800 0) 0.1)
(mul (lf-cub ar (x-line kr 100 8000 30 do-nothing) 0) 0.1)

;; compare:

(let ((f (mul-add (lf-par kr (mul-add (lf-par kr 0.2 0) 8 10) 0) 400 800)))
  (mul (lf-par ar f 0) 0.1))

(mul (lf-par ar (mul-add (lf-par kr 0.2 0) 400 800) 0) 0.1)
(mul (lf-par ar 800 0) 0.1)
(mul (lf-par ar (x-line kr 100 8000 30 do-nothing) 0) 0.1)

;; compare:

(let ((f (mul-add (sin-osc kr (mul-add (sin-osc kr 0.2 0) 8 10) 0) 400 800)))
  (mul (sin-osc ar f 0) 0.1))

(mul (sin-osc ar (mul-add (sin-osc kr 0.2 0) 400 800) 0) 0.1)
(mul (sin-osc ar 800 0) 0.1)
(mul (sin-osc ar (x-line kr 100 8000 30 do-nothing) 0) 0.1)

;; compare:

(let ((f (mul-add (lf-tri kr (mul-add (lf-tri kr 0.2 0) 8 10) 0) 400 800)))
  (mul (lf-tri ar f 0) 0.1))

(mul (lf-tri ar (mul-add (lf-tri kr 0.2 0) 400 800) 0) 0.1)
(mul (lf-tri ar 800 0) 0.1)
(mul (lf-tri ar (x-line kr 100 8000 30 do-nothing) 0) 0.1)
