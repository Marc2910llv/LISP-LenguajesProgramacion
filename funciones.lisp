;; Element pertany a  una llista
(defun pertany (x l)
    (cond ((null l) nil)
        ((equal x (car l)) t)
        (t (pertany x (cdr l)))
    )
)

;;Retorna el darrer element d'una llista
(defun darrer (L)
    (cond ((null (cdr L)) (car L))
    (t (darrer (cdr L))))
)

;; Exponent x^y
(defun exp (x y)
    (cond ((= y 0) 1)
        (t (* x (exp x (- y 1))))
    )
)

;; Fibonacci
(defun fib (n)
    (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))
    )    
)

;; Divisio entera
(defun dividir (x y)
    (cond ((< x y) 0)
        (t (+ 1 (dividir (- x y) y)))
    )
)

;; Parell a partir de funcio "dividir"
(defun parell (x)
    (cond ((= (dividir x 2) (dividir (+ x 1) 2)) t)
    )
)

;; Llevar els parells i deixar els senars d'una llista
(defun senars (l)
    (cond ((null l) nil)
        ((parell (car l)) (senars (cdr l)))
        (t (cons (car l) (senars (cdr l))))
    )
)

;; Eliminar la primera aparicio de element dins llista
(defun borra (x l)
    (cond ((null l) nil)
        ((equal x (car l)) (cdr l))
        (t (cons (car l) (borra x (cdr l))))
    )
)

;; Retorna tots els elements d'una llista menys el darrer
(defun rdc (l)
    (cond
        ((null (cdr l)) nil)
        (t (cons (car l)) (rdc (cdr l)))
    )
)

;; Afegir un element al final d'una llista
(defun snoc (x l)
    (cond
        ((null l) (cons x nil))
        (t (cons (car l) (snoc x (cdr l))))
    )
)

;; Multiplicar tots els elements d'una llista per un número
(defun escala (x l)
    (cond
        ((null l) nil)
        (t (cons (* x (car )) (escala x (cdr l))))
    )
)

;; Màxim
(defun maxim (l)
    (cond
        ((null (cdr l)) (car l))
        ((>= (car l) (maxim (cdr l))) (car l))
        (t (maxim (cdr l)))
    )
)

;; Mínim
(defun minim (l)
    (cond
        ((null (cdr l)) (car l))
        ((< (car l) (minim (cdr l))) (car l))
        (t (minim (cdr l)))
    )
)

;; Ordenar una llista de números
(defun ordena (l)
    (cond
        ((null l) nil)
        (t (cons (minim l) (ordena (borra (minim l) l))))
    )
)

;; Invertir ordre d'una llista
(defun invertir (l)
    (cond
        ((null l) nil)
        ((null (cdr l)) l)
        (t (append (invertir (cdr l)) (list (car l))))
    )
)

;; Conjunt correcte
(defun conjunt-correcte (l)
    (cond ((null l) t)
        ((pertany (car l) (cdr l)) nil)
        (t (conjunt-correcte (cdr l)))
    )
)

;; Fer conjunt
(defun fer-conjunt(l)
    (cond ((null l) nil)
        ((pertany (car l) (cdr l)) (fer-conjunt (cdr l)))
        (t (cons (car l) (fer-conjunt (cdr l))))
    )
)

;; Unió de dos conjunts
(defun unio (c1 c2)
    (cond 
        ((null c1) c2)
        ((pertany (car c1) c2) (unio (cdr c1) c2))
        (t (cons (car c1) (unio (cdr c1) c2)))
    )
)

;; Intersecció de dos conjunts
(defun interseccio (c1 c2)
    (cond
        ((null c1) nil)
        ((pertany (car c1) c2) (cons (car c1) (interseccio (cdr c1) c2)))
        (t (intersecccio (cdr c1) c2))
    )
)

;; Diferència entre dos conjunts
(defun diferencia (c1 c2)
    (cond
        ((null c1) nil)
        ((pertany (car c1) c2) (diferencia (cdr 1) c2))
        (t (cons (car c1) diferencia (cdr c1) c2))
    )
)

;; Diferència simètrica
(defun diferencia-simetrica (c1 c2)
    (unio (diferencia c1 c2) (diferencia c2 c1))
)

;; Producte cartesià entre dos conjunts
(defun producte-cartesia (c1 c2)
    (cond
        ((null c1) nil)
        (t (append (prod (car c1) c2) (producte-cartesia (cdr c1) c2)))
    )
)
(defun prod (e l)
    (cond
        ((null l) nil)
        (t (cons (list e (car l)) (prod e (cdr l))))
    )
)

;; Borrar l'n-èsim element d'una llista
(defun borrar (n l)
    (cond ((null l) nil)
        ((= n 1) (cdr l))
        (t (cons (car l) (borrar (- n 1) (cdr l))))
    )
)

;; Contar el nombre de vegades que una expressió apareix en una llista
(defun vegades (x l)
    (cond
        ((null l) 0)
        ((equal x (car l)) (+ 1 (vegades x (cdr l))))
        (t (vegades x (cdr l)))
    )
)

;; Contar el nombre total d'àtoms que hi ha dins una llista
(defun atoms (l)
    (cond
        ((null l) 0)
        ((not (listp (car l))) (+ 1 (atoms (cdr l))))
        (t (atoms (cdr l)))    
    )
)

;; Aplanar una llista
(defun aplanar (l)
    (cond
        ((null l) nil)
        ((listp (car l)) (append (aplanar (car l)) (aplanar (cdr l))))
        (t (append (car l) (aplanar (cdr l))))
    )
)

;; Retornar una llista sense els seus primers n elements
(defun treuprimers (n l)
    (cond
        ((null l) nil)
        ((= n 1) (cdr l))
        (t (treuprimers (- n 1) (cdr l)))
    )
)

;; Retornar els n primers elements d'una llista
(defun tornaprimers (n l)
    (cond
        ((= n 0) nil)
        (t (cons (car l) (tornaprimers (- n 1) (cdr l))))
    )
)

;; Insertar un element dins una llista per l'ESQUERRA
(defun inserta-esquerra (dequi que l)
    (cond 
        ((null l) nil)
        ((equal (car l) dequi) (cons que l))
        (t (cons (car l) (inserta-esquerra dequi que (cdr l))))
    )
)

;; Insertar un element dins una llista per l'DRETA
(defun inserta-dreta (dequi que l)
    (cond 
        ((null l) nil)
        ((equal (car l) dequi) (cons (car l) (cons que (cdr l))))
        (t (cons (car l) (inserta-dreta dequi que (cdr l))))
    )
)

;; Agafar l'element de la posicio indicada
(defun agafar-n (n l)
    (cond
        ((null l) nil)
        ((= n 1) (car l))
        (t (agafar-n (- n 1) (cdr l)))
    )
)