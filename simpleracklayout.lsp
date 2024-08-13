; define the function
(defun c:SetupRackLayout ()
  (setq column_set (get-column-selection))
  (setq rack_block (get-rack-block))
  (setq rack_area (get-rack-area))
  (setq rack_height (get-rack-height))
  (setq beam_length (get-beam-length))
  (setq building_bounds (get-building-bounds))
  
  (generate-rack-layout column_set rack_block rack_area rack_height beam_length building_bounds)
  (princ)
)

; use selection window to identify the building columns in the drawing
(defun get-column-selection ()
  (princ "\nSelect building columns: ")
  (setq ss (ssget))
  ; assuming no values are selected print error code for debugging sake
  (if (= ss nil)
    (progn
      (princ "\nNo columns selected. Exiting.")
      (exit)
    )
  )
  ss
)

; have user select rack that is being iterated through square footage
(defun get-rack-block ()
  (princ "\nSelect the rack block to be used: ")
  (setq block_name (getstring T))
  ; error handling
  (if (not (tblsearch "BLOCK" block_name))
    (progn
      (princ (strcat "\nBlock '" block_name "' not found. Exiting."))
      (exit)
    )
  )
  block_name
)

; obtains square footage from user
(defun get-rack-area ()
  (princ "\nEnter the square footage for rack placement: ")
  (setq area (getreal))
  ; error handling if racking area not selected
  (if (<= area 0)
    (progn
      (princ "\nInvalid area. Exiting.")
      (exit)
    )
  )
  area
)

; receive max load beam elevation via text entry from user for slap rule
(defun get-rack-height ()
  (princ "\nEnter the maximum rack height: ")
  (setq height (getreal))
  (if (<= height 0)
    (progn
      (princ "\nInvalid height. Exiting.")
      (exit)
    )
  )
  height
)

; obtain beam length for slap rule and simulation distance
(defun get-beam-length ()
  (princ "\nEnter the load beam length: ")
  (setq length (getreal))
  (if (<= length 0)
    (progn
      (princ "\nInvalid length. Exiting.")
      (exit)
    )
  )
  length
)

; obtain building bounds for slap rule and simulation constraints
(defun get-building-bounds ()
  (princ "\nSelect two corners to define the building area: ")
  (setq pt1 (getpoint))
  (setq pt2 (getpoint))
  (list pt1 pt2)
)

(defun calculate-slap-distances (height)
  (list (* height 0.05) (* height 0.02))
)

(defun get-block-info (block_name)
  (setq block_def (entget (tblobjname "BLOCK" block_name)))
  (setq min_point (cdr (assoc 10 block_def)))
  (setq max_point (cdr (assoc 12 block_def)))
  (list (- (car max_point) (car min_point))
        (- (cadr max_point) (cadr min_point)))
)

; commences actual simulation of racking by implementing inputs and calculating minimum distances for aisles
(defun generate-rack-layout (columns rack_block area height beam_length building_bounds)
  (princ "\nGenerating rack layout...")
  
  (setq block_info (get-block-info rack_block))
  (setq block_width (car block_info))
  (setq block_length (cadr block_info))
  
  (setq slap_distances (calculate-slap-distances height))
  (setq down_aisle_distance (car slap_distances))
  (setq cross_aisle_distance (cadr slap_distances))
  
  (princ (strcat "\nDown aisle minimum distance: " (rtos down_aisle_distance 2 2) " units"))
  (princ (strcat "\nCross aisle minimum distance: " (rtos cross_aisle_distance 2 2) " units"))
  
  (place-racks columns building_bounds block_width block_length down_aisle_distance cross_aisle_distance rack_block)
)

; 
(defun place-racks (columns building_bounds block_width block_length down_aisle_dist cross_aisle_dist rack_block)
  (setq x1 (min (car (car building_bounds)) (car (cadr building_bounds))))
  (setq y1 (min (cadr (car building_bounds)) (cadr (cadr building_bounds))))
  (setq x2 (max (car (car building_bounds)) (car (cadr building_bounds))))
  (setq y2 (max (cadr (car building_bounds)) (cadr (cadr building_bounds))))
  
  (setq layout '())
  (setq x (+ x1 cross_aisle_dist))
  (while (<= (+ x block_width) (- x2 cross_aisle_dist))
    (setq y (+ y1 down_aisle_dist))
    (while (<= (+ y block_length) (- y2 down_aisle_dist))
      (if (valid-rack-position x y block_width block_length columns down_aisle_dist cross_aisle_dist)
        (setq layout (cons (list x y) layout))
      )
      (setq y (+ y block_length down_aisle_dist))
    )
    (setq x (+ x block_width cross_aisle_dist))
  )
  
  (foreach pos layout
    (command "._insert" rack_block pos "" "" "")
  )
  
  (princ (strcat "\nPlaced " (itoa (length layout)) " racks."))
)

(defun valid-rack-position (x y width length columns down_dist cross_dist)
  (setq valid T)
  (setq i 0)
  (while (and valid (< i (sslength columns)))
    (setq col_data (entget (ssname columns i)))
    (setq col_point (cdr (assoc 10 col_data)))
    (if (or (< (abs (- (car col_point) x)) (+ cross_dist (/ width 2.0)))
            (< (abs (- (car col_point) (+ x width))) (+ cross_dist (/ width 2.0)))
            (< (abs (- (cadr col_point) y)) (+ down_dist (/ length 2.0)))
            (< (abs (- (cadr col_point) (+ y length))) (+ down_dist (/ length 2.0))))
      (setq valid nil)
    )
    (setq i (1+ i))
  )
  valid
)