(defun C:BOMCALC ()
  (setq block-list '()) ; Initialize block list

  ; Function to add item to the list or increment count
  (defun add-to-list (item-name)
    (setq existing (assoc item-name block-list))
    (if existing
        (setq block-list (subst (cons item-name (1+ (cdr existing))) existing block-list))
        (setq block-list (cons (cons item-name 1) block-list))
    )
  )

  ; Function to process entities within a block reference
  (defun process-entities (block-ref)
    (setq ent (entnext block-ref))
    (while ent
      (setq ent-data (entget ent))
      (if ent-data
          (progn
            (setq ent-type (cdr (assoc 0 ent-data)))
            (cond
              ((= ent-type "TEXT")
               (setq dtext-value (cdr (assoc 1 ent-data)))
               (princ (strcat "\nDTEXT value: " dtext-value)) ; Debug print
               (add-to-list dtext-value)
              )
              ((= ent-type "MTEXT")
               (setq mtext-value (cdr (assoc 1 ent-data)))
               (princ (strcat "\nMTEXT value: " mtext-value)) ; Debug print
               (add-to-list mtext-value)
              )
              ((= ent-type "INSERT")
               (setq nested-block-name (cdr (assoc 2 ent-data)))
               (princ (strcat "\nNested block name: " nested-block-name)) ; Debug print
               (setq nested-block (tblobjname "BLOCK" nested-block-name))
               (if nested-block
                   (vl-catch-all-apply 'process-entities (list nested-block))
               )
              )
            )
          )
      )
      (setq ent (entnext ent))
    )
  )

  ; Function to process block references
  (defun process-block-ref (block-ref)
    (setq block-name (cdr (assoc 2 (entget block-ref))))
    (if block-name
        (progn
          (princ (strcat "\nProcessing block: " block-name)) ; Debug print
          (add-to-list block-name)
          (vl-catch-all-apply 'process-entities (list block-ref))
        )
        (princ "\nBlock name not found.") ; Debug print
    )
  )

  ; Function to get user selection and process blocks
  (defun get-user-selection ()
    (setq ss (ssget '((0 . "INSERT"))))
    (if ss
        (progn
          (setq len (sslength ss))
          (setq i 0)
          (while (< i len)
            (setq block-ref (ssname ss i))
            (if block-ref
                (process-block-ref block-ref)
                (princ "\nInvalid block reference.") ; Debug print
            )
            (setq i (1+ i))
          )
        )
        (princ "\nNo blocks selected.")
    )
  )

  ; Get user selection
  (princ "\nSelect blocks to count: ")
  (get-user-selection)

  ; Prepare CSV data
  (setq csv-data (list (list "Component" "Quantity")))
  
  ; Add block counts to CSV data
  (foreach item block-list
    (setq csv-data (cons (list (car item) (itoa (cdr item))) csv-data))
  )

  ; Reverse the list to get the correct order
  (setq csv-data (reverse csv-data))

  ; Write CSV file
  (setq filename (getfiled "Save CSV File" "" "csv" 1))
  (if filename
      (progn
        (setq file (open filename "w"))
        (foreach row csv-data
          (write-line (strcat (car row) "," (cadr row)) file))
        (close file)
        (princ (strcat "\nBOM calculation complete and exported to " filename)))
      (princ "\nNo file selected."))
  (princ)
)