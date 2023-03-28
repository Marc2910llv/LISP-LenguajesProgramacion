;; FUNCIONS

;; Guardar dins les propietats d'un àtom simbòlic "spiro" informació per inicialitzar el nostre spirograph
(defun guarda-informacio ()

)

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
(defun cercle (x y radi n)

)

;; Convertir graus a radians
(defun radians (g)

)

;; Posar r com a nou valor de radi gran i pinta un cercle en la posició per defecte
(defun radigran (r)

)

;; Posar r com a nou valor de radi petit i pinta un cercle en la posició per defecte
(defun radipetit (r)

)

;; Establir valoras per defecte a propietats de "spiro"
(defun punt (p)

)

(defun inici (angle)

)

(defun escala (e)

)

(defun posicio (x y)

)

;; Calcular la fracció reduïda de m i n i retorna una llista amb els dos valors de la nova fracció
(defun reduir (m n)

)

;; Simula el comportament d’un spirograph amb el número de passes p, amb els radis gran i 
;; petit, amb la distancia t, un increment inc a cada passa i amb l’inici del 
;; dibuixat a l’angle donat en graus. 
(defun spirograph (p gran petit d inc inici)

)

;; Simula el comportament d’un spirograph amb el número de voltes necessàries per acabar tot el traçat
(defun spiro (gran petit p inc inici)

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