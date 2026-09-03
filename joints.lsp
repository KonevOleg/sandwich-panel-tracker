;;; ============================================================
;;; МОДУЛЬ ПРИМЫКАНИЙ И СТЫКОВ v1.3
;;; Чтение динамического свойства "Длина1"
;;; ============================================================
;;; Зависимости: xdata.lsp
;;; ============================================================

(if (not XDATA-SET)
  (load "xdata.lsp")
)

(setq *JOINT-TOLERANCE* 10.0)

;;; ------------------------------------------------------------
;;;  ЧТЕНИЕ ДИНАМИЧЕСКОГО СВОЙСТВА "Длина1"
;;; ------------------------------------------------------------
(defun get-dynamic-dlina1 (ent / obj props prop val i pname)
  (setq obj (vlax-ename->vla-object ent))
  (setq val 0.0)
  
  (if (= (vla-get-IsDynamicBlock obj) :vlax-true)
    (progn
      (setq props (vlax-invoke obj 'GetDynamicBlockProperties))
      (foreach prop props
        (setq pname (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
        (if (and (not (vl-catch-all-error-p pname))
                 (= (strcase pname) "ДЛИНА1"))
          (progn
            (setq v (vl-catch-all-apply 'vlax-variant-value (list (vla-get-Value prop))))
            (if (not (vl-catch-all-error-p v))
              (setq val v)
            )
          )
        )
      )
    )
  )
  val
)


;;; ------------------------------------------------------------
;;;  ЧТЕНИЕ АТРИБУТОВ
;;; ------------------------------------------------------------
(defun get-panel-length (ent / att-list att att-tag att-val pair)
  (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
    (progn
      (setq att-list nil)
      (if (= (cdr (assoc 66 (entget ent))) 1)
        (progn
          (setq att (entnext ent))
          (while (and att (= (cdr (assoc 0 (entget att))) "ATTRIB"))
            (setq att-tag  (strcase (cdr (assoc 2 (entget att))))
                  att-val  (cdr (assoc 1 (entget att))))
            (setq att-list (append att-list (list (cons att-tag att-val))))
            (setq att (entnext att))
          )
        )
      )
      (setq pair (assoc "DLINA" att-list))
      (if pair (atof (cdr pair)) 0.0)
    )
    0.0
  )
)


(defun get-panel-width (ent / att-list att att-tag att-val pair)
  (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
    (progn
      (setq att-list nil)
      (if (= (cdr (assoc 66 (entget ent))) 1)
        (progn
          (setq att (entnext ent))
          (while (and att (= (cdr (assoc 0 (entget att))) "ATTRIB"))
            (setq att-tag  (strcase (cdr (assoc 2 (entget att))))
                  att-val  (cdr (assoc 1 (entget att))))
            (setq att-list (append att-list (list (cons att-tag att-val))))
            (setq att (entnext att))
          )
        )
      )
      (setq pair (assoc "ШИРИНА" att-list))
      (if pair (atof (cdr pair)) 0.0)
    )
    0.0
  )
)


(defun get-panel-bbox (ent / obj minpt maxpt)
  (setq obj (vlax-ename->vla-object ent))
  (vla-GetBoundingBox obj 'minpt 'maxpt)
  (setq minpt (vlax-safearray->list minpt)
        maxpt (vlax-safearray->list maxpt))
  (list (car minpt) (cadr minpt) (car maxpt) (cadr maxpt))
)


;;; ------------------------------------------------------------
;;;  СБОР И ФИЛЬТРАЦИЯ ПАНЕЛЕЙ
;;; ------------------------------------------------------------
(defun collect-all-panels ( / ss i ent blk-list)
  (setq blk-list '()
        ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (XDATA-P ent)
          (setq blk-list (append blk-list (list ent)))
        )
        (setq i (1+ i))
      )
    )
  )
  blk-list
)


(defun filter-installed-only (blk-list / result ent status)
  (setq result '())
  (foreach ent blk-list
    (setq status (XDATA-GET-FIELD ent "STATUS"))
    (if (= status "СМОНТИРОВАНО")
      (setq result (append result (list ent)))
    )
  )
  result
)


;;; ------------------------------------------------------------
;;;  РАСЧЁТ ВЕРТИКАЛЬНЫХ ГРАНЕЙ
;;; ------------------------------------------------------------
(defun calc-vertical (blk-list / total-length count zero-count ent dlina1)
  (setq total-length 0.0
        count 0
        zero-count 0)

  (foreach ent blk-list
    (setq dlina1 (get-dynamic-dlina1 ent))
    (if (> dlina1 0)
      (progn
        (setq total-length (+ total-length (* dlina1 2.0)))
        (setq count (1+ count))
      )
      (setq zero-count (1+ zero-count))
    )
  )

  (list total-length count zero-count)
)


;;; ------------------------------------------------------------
;;;  РАСЧЁТ ГОРИЗОНТАЛЬНЫХ СТЫКОВ
;;; ------------------------------------------------------------
(defun calc-horizontal (blk-list / i j ent1 ent2
                         x1 y1 w1 h1 top1
                         x2 y2 w2 h2 bottom2 bbox1 bbox2
                         total-joints joint-count
                         overlap-start overlap-end overlap-length)

  (setq total-joints 0.0
        joint-count 0)

  (if (< (length blk-list) 2)
    (list total-joints joint-count)
    (progn
      (setq i 0)
      (repeat (length blk-list)
        (setq ent1 (nth i blk-list))
        (setq bbox1 (get-panel-bbox ent1)
              x1 (nth 0 bbox1)
              y1 (nth 1 bbox1)
              w1 (get-panel-length ent1)
              h1 (get-panel-width ent1)
              top1 (+ y1 h1))

        ;; Проверяем только панели ВЫШЕ текущей (j > i)
        (setq j (1+ i))
        (while (< j (length blk-list))
          (setq ent2 (nth j blk-list))
          (setq bbox2 (get-panel-bbox ent2)
                x2 (nth 0 bbox2)
                y2 (nth 1 bbox2)
                w2 (get-panel-length ent2)
                h2 (get-panel-width ent2)
                bottom2 y2)

          ;; Проверяем: верхняя грань ent1 == нижняя грань ent2
          (if (<= (abs (- top1 bottom2)) *JOINT-TOLERANCE*)
            (progn
              (setq overlap-start (max x1 x2)
                    overlap-end   (min (+ x1 w1) (+ x2 w2)))
              (if (> overlap-end overlap-start)
                (progn
                  (setq overlap-length (- overlap-end overlap-start))
                  (setq total-joints (+ total-joints overlap-length))
                  (setq joint-count (1+ joint-count))
                )
              )
            )
          )

          ;; Проверяем: верхняя грань ent2 == нижняя грань ent1
          (if (<= (abs (- (+ y2 h2) y1)) *JOINT-TOLERANCE*)
            (progn
              (setq overlap-start (max x1 x2)
                    overlap-end   (min (+ x1 w1) (+ x2 w2)))
              (if (> overlap-end overlap-start)
                (progn
                  (setq overlap-length (- overlap-end overlap-start))
                  (setq total-joints (+ total-joints overlap-length))
                  (setq joint-count (1+ joint-count))
                )
              )
            )
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )
      (list total-joints joint-count)
    )
  )
)


;;; ------------------------------------------------------------
;;;  ВЫВОД
;;; ------------------------------------------------------------
(defun print-joint-results (label blk-list vert-result horiz-result)
  (princ "\n==============================================")
  (princ (strcat "\n  " label))
  (princ (strcat "\n  Панелей: " (itoa (length blk-list))))
  (princ "\n----------------------------------------------")
  (princ (strcat "\n  Вертикальные грани (Длина1 × 2):"))
  (princ (strcat "\n    Сумма: " (rtos (/ (car vert-result) 1000.0) 2 2) " м"))
  (princ (strcat "\n    (" (rtos (car vert-result) 2 0) " мм)"))
  (if (> (caddr vert-result) 0)
    (princ (strcat "\n    Панелей без свойства 'Длина1': " (itoa (caddr vert-result))))
  )
  (princ (strcat "\n  Горизонтальные стыки:"))
  (princ (strcat "\n    Количество: " (itoa (cadr horiz-result))))
  (princ (strcat "\n    Суммарная длина: " (rtos (/ (car horiz-result) 1000.0) 2 2) " м"))
  (princ (strcat "\n    (" (rtos (car horiz-result) 2 0) " мм)"))
  (princ "\n==============================================")
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: ПРИМЫКАНИЯ
;;; ------------------------------------------------------------
(defun C:ПРИМЫКАНИЯ ( / all-panels installed-panels
                      all-vert all-horiz inst-vert inst-horiz)

  (princ "\nРасчёт примыканий...")
  (setq all-panels (collect-all-panels))

  (if (not all-panels)
    (princ "\nНет панелей с XData. Сначала выполните ВЗЯТЬ_В_РАБОТУ.")
    (progn
      (setq installed-panels (filter-installed-only all-panels))
      (setq all-vert  (calc-vertical all-panels)
            all-horiz (calc-horizontal all-panels))
      (if installed-panels
        (setq inst-vert  (calc-vertical installed-panels)
              inst-horiz (calc-horizontal installed-panels))
        (setq inst-vert  (list 0.0 0 0)
              inst-horiz (list 0.0 0))
      )
      (print-joint-results "ВСЕ ПАНЕЛИ В РАБОТЕ" all-panels all-vert all-horiz)
      (if installed-panels
        (print-joint-results "ТОЛЬКО СМОНТИРОВАННЫЕ" installed-panels inst-vert inst-horiz)
        (princ "\nСмонтированных панелей нет.")
      )
    )
  )
  (princ)
)

(defun get-dynamic-length (ent / obj props prop val pname)
  (setq obj (vlax-ename->vla-object ent))
  (setq val 0.0)
  
  (if (= (vla-get-IsDynamicBlock obj) :vlax-true)
    (progn
      (setq props (vlax-invoke obj 'GetDynamicBlockProperties))
      (foreach prop props
        (setq pname (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
        (if (and (not (vl-catch-all-error-p pname))
                 (= (strcase pname) "ДЛИНА"))
          (progn
            (setq v (vl-catch-all-apply 'vlax-variant-value (list (vla-get-Value prop))))
            (if (not (vl-catch-all-error-p v))
              (setq val v)
            )
          )
        )
      )
    )
  )
  val
)

;;; ------------------------------------------------------------
;;;  КОМАНДА: ПРИМЫКАНИЯ_ПО_ВЫДЕЛЕНИЮ
;;; ------------------------------------------------------------
(defun C:ПРИМЫКАНИЯ_ПО_ВЫДЕЛЕНИЮ ( / ss i ent blk-list vert-result horiz-result)

  (princ "\nВыберите панели рамкой...")
  (setq ss (ssget '((0 . "INSERT"))))

  (if (not ss)
    (princ "\nНичего не выбрано.")
    (progn
      (setq blk-list '() i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (XDATA-P ent)
          (setq blk-list (append blk-list (list ent)))
        )
        (setq i (1+ i))
      )

      (if (not blk-list)
        (princ "\nСреди выбранного нет панелей с XData.")
        (progn
          (setq vert-result  (calc-vertical blk-list)
                horiz-result (calc-horizontal blk-list))

          (print-joint-results "ПРИМЫКАНИЯ ПО ВЫДЕЛЕННЫМ" blk-list vert-result horiz-result)
        )
      )
    )
  )
  (princ)
)

(princ "\nМодуль примыканий v1.3 загружен. Команды: ПРИМЫКАНИЯ, ПРИМЫКАНИЯ_ПО_ВЫДЕЛЕНИЮ")
(princ)