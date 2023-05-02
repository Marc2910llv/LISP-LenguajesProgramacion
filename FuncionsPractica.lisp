;; FUNCIONS

;; Guardar dins les propietats d'un àtom simbòlic "spiro" informació per inicialitzar el nostre spirograph
;;primer hem de fer les llistes de anells grans i petits.
;;cada anell gran tendrà dues propietats : interiors i exteriors.
;;cada anell petit tendrà 3 propietats: dents, forats i diàmetre.

(putprop 'anell1 150 'exteriors)
(putprop 'anell1 105 'interiors)
(putprop 'anell2 144 'exteriors)
(putprop 'anell2 96 'interiors)

(putprop 'cercle1 84 'dents)
(putprop 'cercle1 35 'forats)
(putprop 'cercle1 56 'diametre)

(putprop 'cercle2 80 'dents)
(putprop 'cercle2 33 'forats)
(putprop 'cercle2 53 'diametre)

(putprop 'cercle3 75 'dents)
(putprop 'cercle3 31 'forats)
(putprop 'cercle3 50 'diametre)

(putprop 'cercle4 72 'dents)
(putprop 'cercle4 29 'forats)
(putprop 'cercle4 48 'diametre)

(putprop 'cercle5 63 'dents)
(putprop 'cercle5 25 'forats)
(putprop 'cercle5 42 'diametre)

(putprop 'cercle6 60 'dents)
(putprop 'cercle6 23 'forats)
(putprop 'cercle6 40 'diametre)

(putprop 'cercle7 56 'dents)
(putprop 'cercle7 21 'forats)
(putprop 'cercle7 37 'diametre)

(putprop 'cercle8 52 'dents)
(putprop 'cercle8 19 'forats)
(putprop 'cercle8 35 'diametre)

(putprop 'cercle9 48 'dents)
(putprop 'cercle9 17 'forats)
(putprop 'cercle9 32 'diametre)

(putprop 'cercle10 45 'dents)
(putprop 'cercle10 16 'forats)
(putprop 'cercle10 30 'diametre)

(putprop 'cercle11 42 'dents)
(putprop 'cercle11 14 'forats)
(putprop 'cercle11 28 'diametre)

(putprop 'cercle12 40 'dents)
(putprop 'cercle12 13 'forats)
(putprop 'cercle12 27 'diametre)

(putprop 'cercle13 32 'dents)
(putprop 'cercle13 9 'forats)
(putprop 'cercle13 21 'diametre)

(putprop 'cercle14 30 'dents)
(putprop 'cercle14 8 'forats)
(putprop 'cercle14 20 'diametre)

(putprop 'cercle15 24 'dents)
(putprop 'cercle15 5 'forats)
(putprop 'cercle15 16 'diametre)

;;ara ja tenim les dades guardades en els diferents anells i cercles

(defun guarda-informacio (g p rg rp pu in esc int xco yco pa)
    (putprop 'spiro g 'grans)
    (putprop 'spiro p 'petits)
    (putprop 'spiro rg 'rgran)
    (putprop 'spiro rp 'rpetit)
    (putprop 'spiro pu 'punt)
    (putprop 'spiro in 'inici)
    (putprop 'spiro esc 'escala)
    (putprop 'spiro int 'interior)
    (putprop 'spiro xco 'x)
    (putprop 'spiro yco 'y)
    (putprop 'spiro pa 'pas)
)
;;com cridarem guarda-informació:
(guarda-informacio '(anell1 anell2)
    '(cercle1 cercle2 cercle3 cercle4 cercle5
     cercle6 cercle7 cercle8 cercle9 cercle10
     cercle11 cercle12 cercle13 cercle14 cercle15)
    150 50 3 0 1.2 t 0 0 0.2)

;; Colors de la tinta
(defun vermell ()
    (color 255 0 0)
)

(defun verd ()
    (color 0 255 0)
)

(defun blau ()
    (color 0 0 255)
)

(defun negre ()
    (color 0 0 0)
)

;; Dibuixar cercle en el punt (x,y) de radi r i dividit en n segments
;;movem el llapiç a una posició per pintar
(defun mou (x y)
    (move (realpart (round (+ 320 (* (get 'spiro 'escala) x))))
        (realpart (round (+ 187 (* (get 'spiro 'escala) y ))))
    )
)

;; pintam fins a una posició x i y

(defun pinta(x y)
    (draw (realpart (round (+ 320 (* (get 'spiro 'escala) x))))
        (realpart (round (+ 187 (* (get 'spiro 'escala) y))))
    )
)

;; Convertir graus a radians
(defun radians (g)
    (/ (* g pi) 180)
)

(defun cercle2 (x y radi pas angle)
    (cond ((< angle 360) (pinta (+ x (* radi (cos (radians (+ angle pas)))))  
                                (+ y (* radi (sin (radians (+ angle pas)))))
                                )
                                (cercle2 x y radi pas (+ angle pas)))
            (t t)
    )
)

(defun cercle (x y radi n)
    (mou (+ x radi) y)
    (cercle2 x y radi (/ 360 n) 0)
)

;; Posar r com a nou valor de radi gran i pinta un cercle en la posició per defecte
(defun radigran (r)
    (putprop 'spiro r 'rgran)
    (cercle (get 'spiro 'x) (get 'spiro 'y) r 100)
    
)

;; Posar r com a nou valor de radi petit i pinta un cercle en la posició per defecte
(defun radipetit (r)
    (putprop 'spiro r 'rpetit)
    (cercle (* (- (get 'spiro 'rgran) r) (sin (radians (get 'spiro 'inici)))) (* (- (get 'spiro 'rgran) r) (cos (radians (get 'spiro 'inici)))) r 100)
)

;; Establir valoras per defecte a propietats de "spiro"
(defun punt (p)
    (putprop 'spiro p 'punt)
)

(defun inici (angle)
    (putprop 'spiro angle 'inici)
)

(defun escala (e)
    (putprop 'spiro e 'escala)
)

(defun posicio (x y)
    (putprop 'spiro x 'x)
    (putprop 'spiro y 'y)
)

;; Calcular la fracció reduïda de m i n i retorna una llista amb els dos valors de la nova fracció

;;primer necessitam quatre funcions adicionals, darrer, divisible, conjunt i divisor_comú
;;no fa falta tot això, ja hi ha una funció mcd definida dins lisp
(defun darrer (I) (car (reverse I)))

(defun divisible (x y)
    (cond ((eq (mod x y) 0) t)
        (t nil)))

(defun conjunt (x y)
    (cond ((eq x y) (list x))
        ((divisible x y) (append (list y) (conjunt x (+ y 1))))
        (t (conjunt x (+ y 1)))))

(defun divisor_comu (m n)
    (cond ((null (car m)) nil)
        ((eq (member (car m) n) nil) (divisor_comu (cdr m) n))
        (t (cons (car m) (divisor_comu (cdr m) n)))))

;;funció definitiva

(defun reducir (x y)
    (append (list (/ x (darrer (divisor_comu (conjunt x 1) (conjunt y 1)))))
            (list (/ y (darrer (divisor_comu (conjunt x 1) (conjunt y 1)))))
    )
)

;; Simula el comportament d’un spirograph amb el número de passes p, amb els radis gran i 
;; petit, amb la distancia t, un increment inc a cada passa i amb l’inici del 
;; dibuixat a l’angle donat en graus. 

;;formula 9 i 10 per x mirant la variable interior de spiro
(defun funcio910x (angle d)
   (cond ((eq (get 'spiro 'interior) t) 
   (+ (* (- (get 'spiro 'rgran) (get 'spiro 'rpetit)) (cos (/ (* (get 'spiro 'rpetit) angle) (get 'spiro 'rgran))))
   (* d (cos (* angle (- 1 (/ (get 'spiro 'rpetit) (get 'spiro 'rgran))))))))

    (t (- (* (+ (get 'spiro 'rgran) (get 'spiro 'rpetit)) (cos (/ (* (get 'spiro 'rpetit) angle) (get 'spiro 'rgran))))
            (* d (cos (* angle (+ 1 (/ (get 'spiro 'rpetit) (get 'spiro 'rpetit))))))))
    )
)
;;funció que incrementa l'angle
(defun incrementaangle (angle inc)
    (cond ((>= (+ angle inc) 360) (- (+ angle inc) 360))
        (t (+ angle inc))))

;;formula 9 i 10 per y mirant la variable interior de spiro

(defun funcio910y (angle d)
   (cond ((eq (get 'spiro 'interior) t) 
   (- (* (- (get 'spiro 'rgran) (get 'spiro 'rpetit)) (sin (/ (* (get 'spiro 'rpetit) angle) (get 'spiro 'rgran))))
   (* d (sin (* angle (- 1 (/ (get 'spiro 'rpetit) (get 'spiro 'rgran))))))))

    (t (- (* (+ (get 'spiro 'rgran) (get 'spiro 'rpetit)) (sin (/ (* (get 'spiro 'rpetit) angle) (get 'spiro 'rgran))))
            (* d (sin (* angle (+ 1 (/ (get 'spiro 'rpetit) (get 'spiro 'rpetit))))))))
    )
)

;;dibuixa l'spirograph
(defun spirograph1 (p gran petit d inc inici)
    (cond ((<= p 0) t)
        (t (pinta (+ (* (funcio910x inici d) (cos (get 'spiro 'inici))) (* (funcio910y inici d) (sin (get 'spiro 'inici))))
                (+ (* (* -1 (funcio910x inici d)) (sin (get 'spiro 'inici))) (* (funcio910y inici d) (cos (get 'spiro 'inici))))) 
                (spirograph1 (- p inc) gran petit d inc (incrementaangle inici inc))
                ))
)

(defun spirograph (p gran petit d inc inici)
    (radigran gran)(radipetit petit)(inici inici)(posicio 0 0)(cls)
    (mou (+ (* (funcio910x inici d) (cos (get 'spiro 'inici))) (* (funcio910y inici d) (sin (get 'spiro 'inici))))
                (+ (* (* -1 (funcio910x inici d)) (sin (get 'spiro 'inici))) (* (funcio910y inici d) (cos (get 'spiro 'inici)))))
    (spirograph1 p gran petit d inc inici)
)

;; Simula el comportament d’un spirograph amb el número de voltes necessàries per acabar tot el traçat

(putprop 'cercle_nou 0 'dents)
(putprop 'cercle_nou 0 'forats)
(putprop 'cercle_nou 0 'diametre)


(defun cerca_petit (x y z)
    (cond ((= y 0) nil)
        ((= x (get (car z) 'dents)) (putprop 'cercle_nou (get (car z) 'diametre) 'diametre)
                                            (putprop 'cercle_nou (get (car z) 'dents) 'dents)
                                            (putprop 'cercle_nou (get (car z) 'forats) 'forats) t)
        (t (cerca_petit x (- y 1) (cdr z))))
)

(defun cerca_cercle_petit (x)
    (cerca_petit x 15 (get 'spiro 'petits))
)

(defun spiro (gran petit p inc inici)
    (cerca_cercle_petit petit)
    (spirograph (realpart (round (/ (* (* 2 pi) (car (cdr (reducir gran petit)))) inc)))  
    gran petit (* (/ (+ (get 'cercle_nou 'dents) (+ 1 (- (get 'cercle_nou 'forats) p))) (+ 1 (get 'cercle_nou 'forats))) (car (cdr (reducir gran petit)))) inc inici)
    (print (realpart (round (/ (* (* 2 pi) (car (reducir gran petit))) inc))))
)

;; Simulació completa del spirograph
(defun roda ()

)

;; Simulació completa del spirograph pero n voltes
(defun roda-voltes (n)

)

;; Simulació completa del spirograph pero n voltes i amb els paràmetres indicats
(defun  spiro-voltes (voltes gran petit p in inici)

)

;; Fa totes les simulacions amb els arguments de les llistes contingudes dins la llista l
(defun spiros (l)

)

;; Pintar un joc de proves de 12 figures diferents
(defun dibuix ()

)