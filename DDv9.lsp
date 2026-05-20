;; WARNING! LISP WILL REFER TO "HRK-COTA-ORTH" UNLESS OTHER DIMSTYLE IS SPECIFIED
;; Code Author: Daniel A. Nass - Precision Y-Axis Snapping Correction

(defun c:DDv9 ( / vpEnt vpObj ss i ent entData entType blkName subEnt subData m1 m2 p1 p2 d1 offset count oldDimStyle ang layName parentLay minPt maxPt dx dy )
  (vl-load-com)
  
  ;; 0. Force all layers to be Visible and Thawed before processing
  (command "_.LAYON")
  (command "_.LAYTHW")
  
  ;; 1. Global Setup & System Variables
  (setvar "DIMASSOC" 2)
  
  (setq oldDimStyle (getvar "DIMSTYLE"))
  
  (if (tblsearch "DIMSTYLE" "HRK-COTA-ORTH")
    (progn
      (command "_.DIMSTYLE" "_Restore" "HRK-COTA-ORTH")
      (princ "\nDimension Style set to \"HRK-COTA-ORTH\".")
    )
    (princ "\nWarning: \"HRK-COTA-ORTH\" style not found! Using current style.")
  )

  ;; 2. Select the Viewport border frame
  (setq vpEnt (car (entsel "\nSelect Plant 3D Viewport boundary: ")))
  
  (if vpEnt
    (progn
      (setq vpObj (vlax-ename->vla-object vpEnt))
      (setq offset 12.0) ;; Distance the dimension floats off the line in Paper Space
      (setq count 0)
      
      ;; 3. Activate Model Space to access the nested graphic vectors
      (command "_.MSPACE")
      
      ;; 4. Prompt user to select multiple components
      (princ "\nSelect Plant 3D components and pipes to dimension (Center Lines only): ")
      (setq ss (ssget '((0 . "INSERT,LINE,POLYLINE,LWPOLYLINE,3DSOLID,BODY"))))
      
      (if ss
        (progn
          (princ "\nProcessing selection...")
          
          (setq i 0)
          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            (setq entData (entget ent))
            (setq entType (cdr (assoc 0 entData)))
            
            ;; -----------------------------------------------------------------
            ;; ROUTE A: Standalone Line / Polyline / 3DSolid / Body Primitive
            ;; -----------------------------------------------------------------
            (if (member entType '("LINE" "POLYLINE" "LWPOLYLINE" "3DSOLID" "BODY"))
              (progn
                (setq layName (cdr (assoc 8 entData)))
                
                (if (or (wcmatch (strcase layName) "*CENTRO*")
                        (wcmatch (strcase layName) "*LINHA*"))
                  (progn
                    (cond
                      ((= entType "LINE")
                        (setq m1 (cdr (assoc 10 entData))) 
                        (setq m2 (cdr (assoc 11 entData)))
                      )
                      ((member entType '("POLYLINE" "LWPOLYLINE"))
                        (setq m1 (vlax-curve-getStartPoint ent))
                        (setq m2 (vlax-curve-getEndPoint ent))
                      )
                      ((member entType '("3DSOLID" "BODY"))
                        (vla-getboundingbox (vlax-ename->vla-object ent) 'minPt 'maxPt)
                        (setq m1 (vlax-safearray->list minPt))
                        (setq m2 (vlax-safearray->list maxPt))
                        (if (> (abs (- (car m1) (car m2))) (abs (- (cadr m1) (cadr m2))))
                          (setq m1 (list (car m1) (/ (+ (cadr m1) (cadr m2)) 2.0) (caddr m1))
                                m2 (list (car m2) (/ (+ (cadr m1) (cadr m2)) 2.0) (caddr m2)))
                          (setq m1 (list (/ (+ (car m1) (car m2)) 2.0) (cadr m1) (caddr m1))
                                m2 (list (/ (+ (car m1) (car m2)) 2.0) (cadr m2) (caddr m2)))
                        )
                      )
                    )
                    
                    (command "_.PSPACE")
                    (setq p1 (trans (trans m1 0 2) 2 3))
                    (setq p2 (trans (trans m2 0 2) 2 3))
                    
                    (setq dx (abs (- (car p1) (car p2))))
                    (setq dy (abs (- (cadr p1) (cadr p2))))
                    
                    (if (> dx dy)
                      ;; Horizontal Pipe: Float dimension line UP cleanly
                      (setq d1 (list (/ (+ (car p1) (car p2)) 2.0) (+ (max (cadr p1) (cadr p2)) offset) 0.0))
                      ;; Vertical Pipe: Snap directly to p2's Y plane to enforce perfectly square alignment
                      (setq d1 (list (+ (max (car p1) (car p2)) offset) (cadr p2) 0.0))
                    )
                    
                    (command "_.DIMALIGNED" p1 p2 d1)
                    (command "_.MSPACE")
                    (setq count (1+ count))
                  )
                )
              )
            )
            
            ;; -----------------------------------------------------------------
            ;; ROUTE B: Inline asset / nested block component structure
            ;; -----------------------------------------------------------------
            (if (= entType "INSERT")
              (progn
                (setq blkName (cdr (assoc 2 entData)))
                (setq parentLay (cdr (assoc 8 entData)))
                
                (setq subEnt (tblsearch "BLOCK" blkName))
                (setq subEnt (cdr (assoc -2 subEnt)))
                
                (while subEnt
                  (setq subData (entget subEnt))
                  (setq subType (cdr (assoc 0 subData)))
                  
                  (if (member subType '("LINE" "POLYLINE" "LWPOLYLINE" "3DSOLID" "BODY"))
                    (progn
                      (setq layName (cdr (assoc 8 subData)))
                      
                      (if (= layName "0")
                        (setq layName parentLay)
                      )
                      
                      (if (or (wcmatch (strcase layName) "*CENTRO*")
                              (wcmatch (strcase layName) "*LINHA*"))
                        (progn
                          (cond
                            ((= subType "LINE")
                              (setq m1 (cdr (assoc 10 subData))) 
                              (setq m2 (cdr (assoc 11 subData)))
                            )
                            ((member subType '("POLYLINE" "LWPOLYLINE"))
                              (setq m1 (vlax-curve-getStartPoint subEnt))
                              (setq m2 (vlax-curve-getEndPoint subEnt))
                            )
                            ((member subType '("3DSOLID" "BODY"))
                              (vla-getboundingbox (vlax-ename->vla-object subEnt) 'minPt 'maxPt)
                              (setq m1 (vlax-safearray->list minPt))
                              (setq m2 (vlax-safearray->list maxPt))
                              (if (> (abs (- (car m1) (car m2))) (abs (- (cadr m1) (cadr m2))))
                                (setq m1 (list (car m1) (/ (+ (cadr m1) (cadr m2)) 2.0) (caddr m1))
                                      m2 (list (car m2) (/ (+ (cadr m1) (cadr m2)) 2.0) (caddr m2)))
                                (setq m1 (list (/ (+ (car m1) (car m2)) 2.0) (cadr m1) (caddr m1))
                                      m2 (list (/ (+ (car m1) (car m2)) 2.0) (caddr m2) (caddr m2)))
                              )
                            )
                          )
                          
                          (command "_.PSPACE")
                          (setq p1 (trans (trans m1 0 2) 2 3))
                          (setq p2 (trans (trans m2 0 2) 2 3))
                          
                          (setq dx (abs (- (car p1) (car p2))))
                          (setq dy (abs (- (cadr p1) (cadr p2))))
                          
                          (if (> dx dy)
                            ;; Horizontal Component Asset: Float UP
                            (setq d1 (list (/ (+ (car p1) (car p2)) 2.0) (+ (max (cadr p1) (cadr p2)) offset) 0.0))
                            ;; Vertical Component Asset: Snap directly to p2's Y plane to enforce perfectly square alignment
                            (setq d1 (list (+ (max (car p1) (car p2)) offset) (cadr p2) 0.0))
                          )
                          
                          (command "_.DIMALIGNED" p1 p2 d1)
                          (command "_.MSPACE")
                          (setq count (1+ count))
                        )
                      )
                    )
                  )
                  (setq subEnt (entnext subEnt))
                )
              )
            )
            
            (setq i (1+ i))
          )
          
          (command "_.PSPACE")
          (princ (strcat "\nSuccess! Automatically placed (" (itoa count) ") clean center-line dimensions."))
        )
        (progn
          (command "_.PSPACE")
          (princ "\nNo valid items selected.")
        )
      )
    )
    (princ "\nNo viewport boundary selected.")
  )
  
  ;; 8. Restore original dimension style
  (if (tblsearch "DIMSTYLE" oldDimStyle)
    (command "_.DIMSTYLE" "_Restore" oldDimStyle)
  )
  
  (princ)
)