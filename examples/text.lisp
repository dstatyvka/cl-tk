(in-package :cl-user)

(define-gui-function setup-text-toplevel (&optional (text-name ".text"))
  (let ((root-children (cl-tk:tcl "winfo" "children" ".")))
    (unless (or (null root-children)
                (and (stringp root-children) (string= "" root-children)))
      (apply #'cl-tk:tcl "destroy" (alexandria:ensure-list root-children))))
  (let ((vsb (cl-tk:tcl "::ttk::scrollbar" ".vsb" :orient "vertical" :command (list text-name "yview")))
        (hsb (cl-tk:tcl "::ttk::scrollbar" ".hsb" :orient "horizontal" :command (list text-name "xview"))))
    (cl-tk:tcl "grid" (cl-tk:tcl "text" text-name
                                 :xscrollcommand (list hsb "set")
                                 :yscrollcommand (list vsb "set"))
               :sticky "news" :row 0 :column 0)
    (cl-tk:tcl "grid" vsb :sticky "ns" :row 0 :column 1)
    (cl-tk:tcl "grid" hsb :sticky "ew" :row 1 :column 0)
    (cl-tk:tcl "grid" "columnconfigure" "." 0 :weight 1)
    (cl-tk:tcl "grid" "rowconfigure" "." 0 :weight 1)))

(defclass text-widget-sink (sax:content-handler)
  ((text :initarg :text  :accessor text :initform (error "Name of the target text widget must be specified"))))

(defvar  *indent-level*)
(defvar *tags*)
(defvar *stack*)

(defstruct (tag (:constructor make-tag (name)))
  name
  (n-children 0)
  (have-gt nil))

(defmacro with-text-tags ((&rest tags) &body body)
  `(let ((*tags* (list* ,@tags *tags*)))
     ,@body))

(defun add-to-text-widget-sink (text sink)
  (cl-tk:tcl (text sink) "insert" "end" text *tags*))

(defmethod sax:start-document ((sink text-widget-sink))
  (with-text-tags ("markup")
    (add-to-text-widget-sink "<?xml version='1.0'?>
" sink)))

(defun text-widget-fresh-line (sink)
  (unless (string= (cl-tk:tcl ".text" "index" "insert")
                   (cl-tk:tcl ".text" "index" "end"))
    (add-to-text-widget-sink (string #\newline) sink)
    (loop :repeat *indent-level*
       :do (add-to-text-widget-sink "    " sink))))

(defun start-indentation-block ()
  (incf *indent-level*))

(defun end-indentation-block ()
  (decf *indent-level*))

(defun maybe-start-indentation (sink)
  (text-widget-fresh-line sink)
  (start-indentation-block))

(defun maybe-close-tag (sink)
  (let ((tag (first *stack*)))
    (when (and (tag-p tag) (not (tag-have-gt tag)))
      (setf (tag-have-gt tag) t)
      (with-text-tags ("markup")
        (add-to-text-widget-sink ">" sink)))))

(defmethod sax:end-document ((sink text-widget-sink))
  )

(defmethod sax:start-element
    ((sink text-widget-sink) namespace-uri local-name qname attributes)
  (maybe-close-tag sink)
  (when *stack*
    (incf (tag-n-children (first *stack*))))
  (push (make-tag qname) *stack*)
  (maybe-start-indentation sink)
  (with-text-tags ("markup")
    (add-to-text-widget-sink "<" sink))
  (with-text-tags ("tagname")
    (add-to-text-widget-sink qname sink))
  (let ((*indent-level* (1+ (1+ *indent-level*))))
    (loop :for a :in attributes :do
       (with-text-tags ("markup")
         (add-to-text-widget-sink " " sink))
       (with-text-tags ("attrname")
         (add-to-text-widget-sink (sax:attribute-qname a) sink))
       (with-text-tags ("markup")
         (add-to-text-widget-sink "=\"" sink))
       (with-text-tags ("attrvalue")
         (loop :for char :across (sax:attribute-value a)
            :do (add-to-text-widget-sink (case char
                                           (#\& "&amp;")
                                           (#\< "&lt;")
                                           (#\> "&gt;")
                                           (#\" "&quot;")
                                           (#\Tab "&#09;")
                                           (#\Newline "&#10;")
                                           (#\Return "&#13;")
                                           (t (string char)))
                                         sink)))
       (with-text-tags ("markup")
         (add-to-text-widget-sink "\"" sink))))
  ;; (maybe-close-tag sink)
  )

(defmethod sax:end-element ((sink text-widget-sink) namespace-uri local-name qname)
  (end-indentation-block)
  (let ((tag (pop *stack*)))
    (unless (zerop (tag-n-children tag))
      (text-widget-fresh-line sink))
    (cond ((tag-have-gt tag)
           (with-text-tags ("markup")
             (add-to-text-widget-sink "</" sink))
           (with-text-tags ("tagname")
             (add-to-text-widget-sink qname sink))
           (with-text-tags ("markup")
             (add-to-text-widget-sink ">" sink)))
          (t (with-text-tags ("markup")
               (add-to-text-widget-sink "/>" sink))))))

(defmethod sax:processing-instruction ((sink text-widget-sink) target data)
  (maybe-close-tag sink)
  (with-text-tags ("markup")
    (add-to-text-widget-sink "<?" sink)
    (add-to-text-widget-sink target sink)
    (add-to-text-widget-sink " " sink)
    (add-to-text-widget-sink data sink)
    (add-to-text-widget-sink "?>" sink)))

(defmethod sax:characters ((sink text-widget-sink) data)
  (maybe-close-tag sink)
  (let ((text (string-trim '(#\Newline #\Return #\Space #\Tab) data)))
    (unless (zerop (length text))
      (add-to-text-widget-sink text sink))))

(defmethod sax:unescaped ((sink text-widget-sink) data)
  (add-to-text-widget-sink data sink))

(defmethod sax:comment ((sink text-widget-sink) data)
  (with-text-tags ("comment")
    (add-to-text-widget-sink "<!--" sink)
    (add-to-text-widget-sink data sink)
    (add-to-text-widget-sink "-->" sink)))

(defun test-show-xml (xml-source)
  (let ((sink (make-instance 'text-widget-sink :text ".text"))
        (doc (cxml:parse xml-source (cxml-dom:make-dom-builder))))
    (cl-tk:with-gui-thread ()
      (time
       (progn
         (cl-tk:tcl ".text" "delete" "1.0" "end")
         (let ((*indent-level* 0) *tags* *stack*)
           (dom:map-document sink doc)))))))

(define-gui-function show-xml-in-text (xml-document &optional (text ".text"))
  (cl-tk:tcl ".text" "delete" "1.0" "end")
  (dom:map-document (make-instance 'xml-text-handler :text text) xml-document))
