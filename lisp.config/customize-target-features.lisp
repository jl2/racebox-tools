(lambda (list)
  (flet ((enable (x) (pushnew x list))
         (disable (x) (setf list (remove x list))))

    (mapcar #'enable
            '(:sb-show-assem
              :sb-simd-pack
              :sb-thread
              :sb-futex
              :sb-xref-for-internals
              :sb-source-locations
              :sb-core-compression
              :immobile-space
              :compact-instance-header))
    list))
