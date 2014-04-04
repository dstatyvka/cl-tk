(in-package :cl-user)

(defun execute-statement-to-table (sql &optional (limit 20) (table ".table"))
  (ocilib:with-statement (sql)
    (ocilib:with-result-set () ()
      (let ((cnames (ocilib::column-names)))
        (show-data-table table :titles cnames
                         :data (loop :for row :from 0
                                  :while (and (ocilib:fetch-next)
                                              (or (null limit)
                                                  (< row limit)))
                                  :collect (loop :for i :from 1 :to (length cnames)
                                              :collect (ocilib:get-value i))))))))
