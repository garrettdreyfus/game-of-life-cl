(defun makeGrid(width height)
 (make-list height :initial-element (make-list width :initial-element 0)))

(defun randomizeGrid(grid)
	(loop for y from 0 below (length grid)
		 collect (loop for x from 0 below (length (nth y grid))
		 	collect(random 2)
		)
	)
)
(defun range (max &key (min 0) (step 1))
(loop for n from min below max by step
       collect n))

(defun generateCheckingRadius(length)
 (setq a '())
 (loop
  for x from (- 0 length) below (+ length 1)
	collect (loop for y from (- 0 length) below (+ length 1) when(or (/= x 0) (/= y 0)) do (push (list x y) a)))
 a
)

(defun countNeighbors(grid x y)
 (setq c (loop
  for el in (generateCheckingRadius 1)
      sum 
      (nth (mod (+ x (nth 0 el)) (length (nth y grid)))
       (nth 
	(mod (+ y (nth 1 el)) (length grid)) grid))
 ))
 c
)

(defun printGrid(grid)
 (shell "clear")
 (setq copy 
 (loop for y in grid
  	collect(loop for x in y
	    collect (cond 
			((= x 0) '░░)
			((= x 1) '██)
		))))
 (format t "~{~{~A~}~%~}" copy)
)

(defun updateGrid(grid rulebook)
	(loop for y from 0 below (length grid)
		collect (loop for x from 0 below (length (nth y grid))
			collect (nth (countNeighbors grid x y) (nth (nth x (nth y grid))  rulebook)))))

(defun runGame(grid rulebook)
	(loop while (= 1 1) do
	 (printGrid grid)
	 (setq grid (updateGrid grid rulebook))
	 (sleep 0.5)
	)
)
(setq rulebook '((0 0 0 1 0 0 0 0 0 0) (0 0 1 1 0 0 0 0 0 0)))
(runGame (randomizeGrid (makeGrid 30 30)) rulebook)
