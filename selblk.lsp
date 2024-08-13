(defun c:selblk ()
  ;; Prompt the user to select a block
  (setq ss (ssget "_:S" '((0 . "INSERT"))))
  (if ss
    (progn
      ;; Get the selected block reference
      (setq blkRef (vlax-ename->vla-object (ssname ss 0)))
      (setq blockName (vla-get-Name blkRef))
      
      ;; Function to collect all instances of the block
      (defun collect-blocks (blockName)
        (setq blockRefs '())
        (vlax-for layout (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-Acad-Object)))
          (vlax-for entity (vla-get-Block layout)
            (if (and (eq (vla-get-ObjectName entity) "AcDbBlockReference")
                     (eq (vla-get-Name entity) blockName))
              (setq blockRefs (cons entity blockRefs)))))
        blockRefs)
      
      ;; Collect all instances of the block
      (setq allBlockRefs (collect-blocks blockName))
      
      ;; Prepare attribute list for the dialog box
      (setq attribs (vlax-invoke blkRef 'GetAttributes))
      (setq attmap '())
      (foreach att attribs
        (setq tag (vla-get-TagString att))
        (setq value (vla-get-TextString att))
        (setq attmap (cons (cons tag value) attmap)))

      ;; Load the DCL file
      (setq dcl_file (findfile "selblk.dcl"))
      (if (not dcl_file)
        (progn
          (alert "DCL file not found!")
          (exit)))

      ;; Initialize the dialog
      (setq dcl_id (load_dialog dcl_file))
      (if (not (new_dialog "selblk" dcl_id))
        (progn
          (alert "Unable to create dialog!")
          (exit)))

      ;; Populate the list box with attributes
      (start_list "attlist")
      (mapcar 'add_list (mapcar 'car attmap))
      (end_list)

      ;; Define actions for dialog controls
      (defun attlist_action ()
        (setq sel_item_index (atoi (get_tile "attlist")))
        (setq sel_item (nth sel_item_index (mapcar 'car attmap)))
        (setq sel_value (cdr (assoc sel_item attmap)))
        (set_tile "attlabel" sel_item)
        (set_tile "currentvalue" sel_value)
        (set_tile "attvalue" sel_value))

      (defun save_attribute ()
        ;; Save the current attribute changes in attmap
        (setq sel_item_index (atoi (get_tile "attlist")))
        (setq sel_item (nth sel_item_index (mapcar 'car attmap)))
        (setq new_value (get_tile "attvalue"))
        (setq attmap (subst (cons sel_item new_value) (assoc sel_item attmap) attmap)))

      (defun apply_action ()
        ;; Apply changes to all instances of the block
        (foreach blkRef allBlockRefs
          (setq attribs (vlax-invoke blkRef 'GetAttributes))
          (foreach att attribs
            (setq tag (vla-get-TagString att))
            (setq new_value (cdr (assoc tag attmap)))
            (if (and (assoc tag attmap) (not (equal new_value "")))
              (vla-put-TextString att new_value))))
        (done_dialog))

      (defun cancel_action ()
        (done_dialog)
        (exit))

      ;; Attach actions to controls
      (action_tile "attlist" "(attlist_action)")
      (action_tile "attvalue" "(save_attribute)")
      (action_tile "apply" "(apply_action)")
      (action_tile "cancel" "(cancel_action)")

      ;; Display the dialog box
      (start_dialog)
      
      ;; Unload the DCL file
      (unload_dialog dcl_id))
    (alert "No block selected!"))
  (princ))

(princ "\nType SELBLK to run the attribute editor.\n")
(princ)


; Aaron Cendejas was here and did all of the above ^^^