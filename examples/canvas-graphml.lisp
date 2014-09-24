(in-package :cl-user)

(defvar *node-graphics*)
(defvar *edge-graphics*)

(defvar *node-graphics-query*)
(defvar *edge-graphics-query*)

(defun ensure-font (family size style)
  (let ((name (format nil "~a-~a-~a" family (- size) style)))
    (multiple-value-bind (slant weight)
        (alexandria:eswitch (style :test #'string=)
          ("plain" (values "roman" "normal"))
          ("bold" (values "roman" "bold"))
          ("italic" (values "italic" "normal")))
      (prog1 name
        (cl-tk:tcl "catch"
                   (list "font" "create" name :family family :size size :slant slant :weight weight))))))



(defun canvas-add-label (label &key (origin '(0 0)) id)
  (let ((font-family (xpath:string-value (xpath:evaluate "@fontFamily" label)))
        (font-size (xpath:number-value (xpath:evaluate "@fontSize" label)))
        (font-style (xpath:string-value (xpath:evaluate "@fontStyle" label)))
        (label-w (xpath:number-value (xpath:evaluate "@width" label)))
        (label-x (+ (first origin)
                    (xpath:number-value
                     (xpath:evaluate "@x + @width * 0.5" label))))
        (label-y (+ (second origin)
                    (xpath:number-value
                     (xpath:evaluate "@y + @height * 0.5" label))))
        (label-text (xpath:string-value (xpath:evaluate "text()" label)))
        (alignment (xpath:string-value (xpath:evaluate "@alignment" label))))
    (canvas-create-text label-x label-y label-text
                        :font (ensure-font font-family font-size font-style)
                        :width label-w
                        :justify (or alignment "left")
                        :tags (unless (null id)
                                (list id)))))

(define-gui-function canvas-add-node (node)
  (xpath:with-namespaces (("g" "http://graphml.graphdrawing.org/xmlns")
                          ("y" "http://www.yworks.com/xml/graphml"))
    (let ((id (xpath:string-value (xpath:evaluate "@id" node))))
      (destructuring-bind (graphics)
          (xpath:all-nodes (xpath:evaluate *node-graphics-query* node))
        (let* ((shape (xpath:string-value (xpath:evaluate "y:Shape/@type" graphics)))
               (x1 (xpath:number-value (xpath:evaluate  "y:Geometry/@x" graphics)))
               (y1 (xpath:number-value (xpath:evaluate  "y:Geometry/@y" graphics)))
               (x2 (xpath:number-value (xpath:evaluate  "y:Geometry/@x + y:Geometry/@width" graphics)))
               (y2 (xpath:number-value (xpath:evaluate  "y:Geometry/@y + y:Geometry/@height" graphics)))
               (fill (xpath:string-value (xpath:evaluate "y:Fill/@color" graphics)))
               (args (list :tags (list id))))
          (unless (null fill)
            (push fill args)
            (push :fill args)) 
          (apply (alexandria:switch (shape :test #'string=)
                   ("rectangle" #'canvas-create-rect)
                   ("ellipse" #'canvas-create-oval))
                 x1 y1 x2 y2 args)
          (let ((labels (xpath:evaluate "y:NodeLabel" graphics)))
            (unless (xpath:node-set-empty-p labels)
              (canvas-add-label (xpath:first-node labels)
                                :origin (list x1 y1)
                                :id id))))))))

(define-gui-function canvas-add-edge (edge)
  (xpath:with-namespaces (("g" "http://graphml.graphdrawing.org/xmlns")
                          ("y" "http://www.yworks.com/xml/graphml"))
    (labels ((node (query)
               (destructuring-bind (node)
                   (xpath:all-nodes (xpath:evaluate query edge))
                 node)))
      (let* ((id (xpath:string-value (xpath:evaluate "@id" edge)))
             (graphics (node *edge-graphics-query*))
             (source (node (format nil "../g:node[@id='~a']//y:Geometry" 
                                   (xpath:string-value (node "@source")))))
             (target (node (format nil "../g:node[@id='~a']//y:Geometry" 
                                   (xpath:string-value (node "@target")))))
             (center-x-query "@x + 0.5 * @width")
             (center-y-query "@y + 0.5 * @height")
             (path (xpath:first-node (xpath:evaluate "y:Path" graphics)))
             (source-arrow (not (string= "none"
                                         (xpath:string-value (xpath:evaluate "y:Arrows/@source" graphics)))))
             (target-arrow (not (string= "none"
                                         (xpath:string-value (xpath:evaluate "y:Arrows/@target" graphics)))))
             (sx (+ (xpath:number-value (xpath:evaluate center-x-query source))
                    (xpath:number-value (xpath:evaluate "@sx" path))))
             (sy (+ (xpath:number-value (xpath:evaluate center-y-query source))
                    (xpath:number-value (xpath:evaluate "@sy" path))))
             (ty (+ (xpath:number-value (xpath:evaluate center-y-query target))
                    (xpath:number-value (xpath:evaluate "@ty" path))))
             (tx (+ (xpath:number-value (xpath:evaluate center-x-query target))
                    (xpath:number-value (xpath:evaluate "@tx" path)))))
        (canvas-create-line 
         (let* (result)
           (xpath:do-node-set (point (xpath:evaluate "y:Point" path))
             (push (xpath:number-value (xpath:evaluate "@x" point)) result)
             (push (xpath:number-value (xpath:evaluate "@y" point)) result))
           (list* sx sy (nreverse (list* ty tx result))))
         :tags (list id)
         :arrow (cond ((and source-arrow target-arrow) "both")
		      ((null source-arrow) "last")
		      ((null target-arrow) "first")
		      (t "none")))
        (let ((labels (xpath:evaluate "y:EdgeLabel" graphics)))
          (unless (xpath:node-set-empty-p labels)
            (canvas-add-label (xpath:first-node labels)
                              :origin (list sx sy)
                              :id id)))))))


(define-gui-function canvas-add-graph (graphml)
  (xpath:with-namespaces (("g" "http://graphml.graphdrawing.org/xmlns")
                          ("y" "http://www.yworks.com/xml/graphml"))
    (let* ((*node-graphics* (xpath:string-value
                            (xpath:evaluate "/g:graphml/g:key[@yfiles.type='nodegraphics']/@id" graphml)))
           (*node-graphics-query* (format nil "g:data[@key='~a']/y:ShapeNode" *node-graphics*))
           (*edge-graphics* (xpath:string-value
                            (xpath:evaluate "/g:graphml/g:key[@yfiles.type='edgegraphics']/@id" graphml)))
           (*edge-graphics-query* (format nil "g:data[@key='~a']/y:PolyLineEdge" *edge-graphics*)))
      (xpath:map-node-set #'canvas-add-node
                          (xpath:evaluate "/g:graphml/g:graph/g:node" graphml))
      (xpath:map-node-set #'canvas-add-edge
                          (xpath:evaluate "/g:graphml/g:graph/g:edge" graphml)))))
