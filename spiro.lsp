;; FUNCIONS

;; Guardar dins les propietats d'un àtom simbòlic "spiro" informació per inicialitzar el nostre spirograph
;;primer hem de fer les llistes de anells grans i petits.
;;cada anell gran tendrà dues propietats : interiors i exteriors.
;;cada anell petit tendrà 3 propietats: dents, forats i diàmetre.

(load 'funciones.lisp)

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
    150 50 3 0 1.8 t 0 0 0.2)

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

;;Retorna el darrer element d'una llista
(defun darrer (L)
    (cond ((null (cdr L)) (car L))
    (t (darrer (cdr L))))
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

(defun cerclex (x y radi n)
    (mou (+ x radi) y)
    ;;(cercle2 x y radi (/ 360 n) 0)
)

;; Posar r com a nou valor de radi gran i pinta un cercle en la posició per defecte
(defun radigran (r)
    (putprop 'spiro r 'rgran)
    (cerclex (get 'spiro 'x) (get 'spiro 'y) r 100)
    
)

;; Posar r com a nou valor de radi petit i pinta un cercle en la posició per defecte
(defun radipetit (r)
    (putprop 'spiro r 'rpetit)
    (cerclex (* (- (get 'spiro 'rgran) r) (sin (radians (get 'spiro 'inici)))) (* (- (get 'spiro 'rgran) r) (cos (radians (get 'spiro 'inici)))) r 100)
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

(defun interior (i)
    (putprop 'spiro i 'interior)
)

(defun posicio (x y)
    (putprop 'spiro x 'x)
    (putprop 'spiro y 'y)
)

;; Calcular la fracció reduïda de m i n i retorna una llista amb els dos valors de la nova fracció

;;primer necessitam quatre funcions adicionals, darrer, divisible, conjunt i divisor_comú
;;Aquestes funcionalitats no son necessàries, ja que LISP ja incorpora una funció reducir. Però com que ja 
;;ho teniem fet antes de saber això ho deixarem així.
(defun darrer (I) (car (reverse I)))

(defun divisible (x y)
    (cond ((eq (mod x y) 0) t)
        (t nil)))

;;Funció que li passam un nombre, per exempe 5, i un 1, i treim una llista de tots els nombres divisibles per el 5
(defun conjunt (x y)
    (cond ((eq x y) (list x))
        ((divisible x y) (append (list y) (conjunt x (+ y 1))))
        (t (conjunt x (+ y 1)))))

;;Funció que donats dos conjunts de nombres, treu una llista amb els nombres que es troben a les dues llistes
(defun divisor_comu (m n)
    (cond ((null (car m)) nil)
        ((eq (member (car m) n) nil) (divisor_comu (cdr m) n))
        (t (cons (car m) (divisor_comu (cdr m) n)))))

;;funció definitiva

;;amb la funció divisor comú hem tret la llista de divisors comuns als dos nombres, agafarem el màxim d'aquesta llista
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
;;a pinta li passam una posició x i y. El que feim a x i a y primer es sumar la posició d'inici que
;;es troba a 'spiro 'x i 'spiro 'y respectivament.
;; Aquesta suma es farà sobre la posició calculada per les funcions funcio910x i funcio910y

(defun spirograph1 (p gran petit d inc inici)
    (cond ((<= p 0) t)
        (t (pinta (+ (get 'spiro 'x) (+ (* (funcio910x inici d) (cos (get 'spiro 'inici))) (* (funcio910y inici d) (sin (get 'spiro 'inici)))))
                (+ (get 'spiro 'y)(+ (* (* -1 (funcio910x inici d)) (sin (get 'spiro 'inici))) (* (funcio910y inici d) (cos (get 'spiro 'inici)))))
                ) 
                (spirograph1 (- p inc) gran petit d inc (incrementaangle inici inc))
                ))
)

;;Primer ens movem a la posició d'inici donta l'angle d'inici i la d
;;A l'spiro guardam el radigran, radipetit i l'angle d'inici
(defun spirograph (p gran petit d inc inici)
    (radigran gran)(radipetit petit)(inici inici)
    (mou (+ (get 'spiro 'x) (+ (* (funcio910x inici d) (cos (get 'spiro 'inici))) (* (funcio910y inici d) (sin (get 'spiro 'inici)))))
                (+ (get 'spiro 'y) (+ (* (* -1 (funcio910x inici d)) (sin (get 'spiro 'inici))) (* (funcio910y inici d) (cos (get 'spiro 'inici)))))
                )
    (spirograph1 p gran petit d inc inici)
)

;; Simula el comportament d’un spirograph amb el número de voltes necessàries per acabar tot el traçat

(putprop 'cercle_nou 0 'dents)
(putprop 'cercle_nou 0 'forats)
(putprop 'cercle_nou 0 'diametre)

;;Funció on li passam a x el número de dents de el cercle petit, a y el total de cercles diferents que hi ha (15)
    ;;i a z la llista dels cercles petits
;; El que fa bàsicament és verificar que el número de dents que li hem passat es troba dins la llista de cercles i, per tant,
;; aquest nombre és vàlid
(defun cerca_petit (x y z)
    (cond ((= y 0) nil)
        ((= x (get (car z) 'dents)) (putprop 'cercle_nou (get (car z) 'diametre) 'diametre)
                                            (putprop 'cercle_nou (get (car z) 'dents) 'dents)
                                            (putprop 'cercle_nou (get (car z) 'forats) 'forats) t)
        (t (cerca_petit x (- y 1) (cdr z))))
)

;;Antes de mirar la funció spiro, ens hem de assegurar que els paràmetres gran i petit que es passaràn son correctes
;; cerca_cercle_gran i cerca_cercle_petit s'encarreguen d'això, retornen true si petit i gran son correctes i false d'altra banda
(defun cerca_cercle_petit (x)
    (cerca_petit x 15 (get 'spiro 'petits))
)

(defun cerca_gran_fora (x z)
    (cond ((= x (get (car z) 'exteriors)) t)
        ((= x (get (cadr z) 'exteriors)) t)
        (t nil))
    )


(defun cerca_gran_dins (x z)
    (cond ((= x (get (car z) 'interiors)) t)
        ((= x (get (cadr z) 'interiors)) t)
        (t nil))
    )

(defun cerca_cercle_gran (x)
(cond ((eq (get 'spiro 'interior) t) (cerca_gran_dins x (get 'spiro 'grans)))
    (t (cerca_gran_fora x (get 'spiro 'grans) )))
)

;;Si petit i gran és vàlid és vàlid, si no, retornam false
;; calculam el nombre de passes que haurem de fer perquè el spirograph sigui complet
;; també calculam la t que passam a spirograph amb la p que passam per paràmetre
(defun spiro (gran petit p inc inici)
    (cond ((and (eq (cerca_cercle_gran gran) t) (eq (cerca_cercle_petit petit) t))
    (spirograph (realpart (round (/ (* (* 2 pi) (car (cdr (reducir gran petit)))) inc)))  
    gran petit (/ (* (get 'cercle_nou 'dents) (+ 1 (- (get 'cercle_nou 'forats) p))) (+ 1 (get 'cercle_nou 'forats))) inc inici))
    (t nil)
    )

)

(defun spiro-n (n gran petit p inc inici)
    (cerca_cercle_petit petit)
    (spirograph n 
    gran petit (/ (* (get 'cercle_nou 'dents) (+ 1 (- (get 'cercle_nou 'forats) p))) (+ 1 (get 'cercle_nou 'forats))) inc inici)
)

;; Simulació completa del spirograph
(defun roda ()
    (spiro (get 'spiro 'rgran) (get 'spiro 'rpetit) (get 'spiro 'punt) (get 'spiro 'pas) (get 'spiro 'inici))
)

;; Simulació completa del spirograph pero n voltes
(defun roda-voltes (n)
    (spiro-n n (get 'spiro 'rgran) (get 'spiro 'rpetit) (get 'spiro 'punt) (get 'spiro 'pas) (get 'spiro 'inici))
)

;; Simulació completa del spirograph pero n voltes i amb els paràmetres indicats
(defun  spiro-voltes (voltes gran petit p in inici)
    (spiro-n voltes gran petit p in inici)
)

;; Fa totes les simulacions amb els arguments de les llistes contingudes dins la llista l

(defun spiros (L)
    (cond ((eq (cdr L) nil) (spiro (agafar-n 1 (car L)) (agafar-n 2 (car L)) (agafar-n 3 (car L)) (agafar-n 4 (car L)) (agafar-n 5 (car L))))
    (t (spiro (agafar-n 1 (car L)) (agafar-n 2 (car L)) (agafar-n 3 (car L)) (agafar-n 4 (car L)) (agafar-n 5 (car L))) (spiros (cdr L)))
    )
)

;; Pintar un joc de proves de 12 figures diferents
(defun figura1 ()
    (vermell)
    (spiros '((105 63 1 0.5 0)
              (105 63 3 0.5 0)
              (105 63 5 0.5 0)))
    (verd)
    (spiros '((105 63 7 0.5 0)
              (105 63 9 0.5 0)
              (105 63 11 0.5 0)))
    (blau)
    (spiros '((105 63 13 0.5 0)
              (105 63 15 0.5 0)
              (105 63 17 0.5 0)))
)

(defun figura2 ()
    (verd)
    (spiros '((96 32 1 0.3 40)
              (96 32 7 0.3 40)
              (96 32 13 0.3 40)))
    (vermell)
    (spiros '((96 32 3 0.3 40)
              (96 32 9 0.3 40)
              (96 32 15 0.3 40)))
    (blau)
    (spiros '((96 32 5 0.3 40)
              (96 32 11 0.3 40)
              (96 32 17 0.3 40)))
)

(defun figura3 ()
    (vermell)
    (spiros '((105 84 1 0.1 0)
              (105 84 7 0.1 0)
              (105 84 11 0.1 0)))
    (verd)
    (radigran 144)
    (radipetit 80)
    (inici 0)
    (interior nil)
    (punt 1)(roda-voltes 60)
)

(defun figura4 ()
    (radigran 105)
    (radipetit 60)
    (inici 0)
    (interior t)
    (vermell)
    (punt 3)(roda)
    (radigran 105)
    (radipetit 60)
    (inici 3)
    (interior t)
    (blau)
    (punt 3)(roda)
    (radigran 105)
    (radipetit 60)
    (inici 5.9)
    (interior t)
    (verd)
    (punt 3)(roda)
)

(defun figura5 ()
    (radigran 40)
    (radipetit 15)
    (inici 0)
    (interior nil)
    (blau)
    (punt 1)(roda-voltes 51)
)

(defun figura6 ()
    (vermell)
    (spiro-voltes 60 96 72 5 1 45)
    (blau)
    (spiro-voltes 40 96 32 5 0.5 45)
    (verd)
)

(defun figura7 ()
    (blau)
    (interior nil)
    (spirograph 100 150 84 1 0.3 0)
    (verd)
    (spirograph 50 144 84 17 0.5 0)
    (vermell)
    (interior t)
    (spirograph 100 96 45 15 1 0)
)

(defun figura8 ()
    (vermell)
    (spiro 105 80 5 0.6 0)
    (blau)
    (spiro 96 75 5 0.6 0)
    (verd)
    (spiro 105 72 11 0.7 0)
)

(defun figura9 ()
    (interior nil)
    (verd)
    (spiros '((150 80 1 1 0)
              (150 80 3 1 0)
              (150 80 5 1 0)))
    (vermell)
    (spiros '((150 84 7 1.5 45)
              (150 84 9 1.5 45)
              (150 84 11 1.5 45)))
    (blau)
    (spiros '((144 24 13 0.1 90)
              (144 24 15 0.1 90)
              (144 24 17 0.1 90)))
)

(defun figura10 ()
    (interior nil)
    (vermell)
    (spiros '((150 40 1 0.1 0)
              (150 40 9 0.1 0)
              (150 40 17 0.1 0)))
    (interior t)
    (blau)
    (spiros '((105 40 1 0.5 0)
              (105 40 9 0.5 0)
              (105 40 17 0.5 0)))
    (verd)
    (spiros '((96 84 13 0.5 0)
              (96 84 15 0.5 0)
              (96 84 17 0.5 0)))

)

(defun figura11 ()
    (interior t)
    (verd)
    (spiros '((105 75 1 0.5 0)
              (105 75 3 0.5 0)
              (105 75 5 0.5 0)))
    (verd)
    (spiros '((105 75 7 0.5 30)
              (105 75 9 0.5 30)
              (105 75 11 0.5 30)))
    (blau)
    (spiros '((105 75 13 0.5 15)
              (105 75 15 0.5 15)
              (105 75 17 0.5 15)))
)

(defun figura12 ()
    (interior t)
    (vermell)
    (spiros '((105 52 13 0.8 45)
              (105 52 15 0.8 45)
              (105 52 17 0.8 45)))
    (blau)
    (spiros '((96 84 1 0.5 0)
              (96 84 7 0.5 0)
              (96 84 13 0.5 0)))
    (verd)
    (spiros '((96 24 13 0.2 90)
              (96 24 15 0.2 90)
              (96 24 17 0.2 90)))
    (verd)
    (spiros '((96 24 1 0.2 90)
              (96 24 3 0.2 90)
              (96 24 5 0.2 90)))
)

(defun dibuix ()
    (posicio -300 215) 
    (escala 0.6)
    (figura1)
    (posicio -100 215)
    (escala 0.6)
    (figura2)
    (posicio 450 615)
    (escala 0.2)
    (figura3)
    (posicio 380 215)
    (escala 0.6)
    (figura4)

    (posicio -370 30)
    (escala 0.5)
    (figura5)
    (posicio -170 30)
    (escala 0.3)
    (figura6)
    (posicio 440 30)
    (escala 0.2)
    (figura7)
    (posicio 380 10)
    (escala 0.6)
    (figura8)

    (posicio -950 -525)
    (escala 0.2)
    (figura9)
    (posicio -250 -600)
    (escala 0.2)
    (figura10)
    (posicio 150 -190)
    (escala 0.6)
    (figura11)
    (posicio 380 -190)
    (escala 0.6)
    (figura12)
)