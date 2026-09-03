;;; ============================================================
;;; МОДУЛЬ XData ДЛЯ УПРАВЛЕНИЯ СЭНДВИЧ-ПАНЕЛЯМИ
;;; ============================================================
;;; Функции: XDATA-SET, XDATA-GET, XDATA-P, XDATA-GET-FIELD
;;; ============================================================

(setq *SANDWICH-APPNAME* "SANDWICH_MGR")

(defun XDATA-SET (ent data / elist xlist app-list newxlist item tag val)

  (if (not (entget ent))
    nil
    (progn
      (setq elist (entget ent))
      (regapp *SANDWICH-APPNAME*)
      (setq elist (sandwich:remove-xdata elist *SANDWICH-APPNAME*))

      (setq xlist (list (cons 1002 "{")))
      (foreach item data
        (setq tag (car item)
              val (cdr item))
        (setq xlist (append xlist
                            (list (cons 1000 tag)
                                  (cons 1000 val))))
      )
      (setq xlist (append xlist (list (cons 1002 "}"))))
      (setq app-list (cons *SANDWICH-APPNAME* xlist))
      (setq newxlist (list -3 app-list))
      (setq elist (append elist (list newxlist)))
      (entmod elist)
      (entupd ent)
      T
    )
  )
)

(defun XDATA-GET (ent / elist xlist appdata result tag val in-group item)

  (if (not (entget ent))
    nil
    (progn
      (setq elist (entget ent '("*")))
      (setq xlist (cdr (assoc -3 elist)))

      (if (not xlist)
        nil
        (progn
          (setq appdata (cdr (assoc *SANDWICH-APPNAME* xlist)))

          (if (not appdata)
            nil
            (progn
              (setq result '()
                    tag nil
                    in-group nil)

              (foreach item appdata
                (cond
                  ((and (= (car item) 1002) (= (cdr item) "{"))
                   (setq in-group T tag nil))
                  ((and (= (car item) 1002) (= (cdr item) "}"))
                   (setq in-group nil))
                  ((and in-group (= (car item) 1000))
                   (if (not tag)
                     (setq tag (cdr item))
                     (progn
                       (setq result (append result (list (cons tag (cdr item)))))
                       (setq tag nil)
                     )
                   )
                  )
                )
              )

              result
            )
          )
        )
      )
    )
  )
)

(defun XDATA-P (ent)
  (if (and ent (XDATA-GET ent))
    T
    nil
  )
)

(defun XDATA-GET-FIELD (ent tag / data pair)
  (setq data (XDATA-GET ent))
  (if data
    (progn
      (setq pair (assoc tag data))
      (if pair
        (cdr pair)
        nil
      )
    )
    nil
  )
)

(defun sandwich:remove-xdata (elist appname / newlist)
  (setq newlist '())
  (foreach item elist
    (if (and (= (car item) -3)
             (listp (cdr item))
             (assoc appname (cdr item)))
      nil
      (setq newlist (append newlist (list item)))
    )
  )
  newlist
)

(defun C:ТЕСТ_ЗАПИСЬ_XDATA ( / ent )
  (setq ent (car (entsel "\nВыберите объект для записи XData: ")))
  (if ent
    (XDATA-SET ent
      (list
        (cons "CONTRACTOR"   "Иванов")
        (cons "STATUS"       "СМОНТИРОВАНО")
        (cons "QUEUE"        "2")
        (cons "DATE_INSTALL" "2026-07-21")
        (cons "TEAM"         "Бригада 3")
        (cons "DEFECT_DESC"  "")
        (cons "DATE_CHANGED" "2026-07-21 16:30")
        (cons "CHANGED_BY"   "Прораб Петров")
      )
    )
  )
  (princ)
)

(defun C:ТЕСТ_ЧТЕНИЕ_XDATA ( / ent data )
  (setq ent (car (entsel "\nВыберите объект для чтения XData: ")))
  (if ent
    (progn
      (setq data (XDATA-GET ent))
      (if data
        (progn
          (princ "\n===== XData объекта =====")
          (foreach pair data
            (princ (strcat "\n" (car pair) ": " (cdr pair)))
          )
          (princ "\n=========================")
        )
      )
    )
  )
  (princ)
)

(princ "\nМодуль XData загружен.")
(princ)