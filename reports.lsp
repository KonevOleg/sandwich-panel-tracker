;;; ============================================================
;;; МОДУЛЬ ОТЧЁТОВ v2.0
;;; Площадь считается как DLINA × Длина1 (динамическое свойство)
;;; ============================================================
;;; Зависимости: xdata.lsp, joints.lsp
;;; ============================================================

(if (not XDATA-SET)        (load "xdata.lsp"))
(if (not get-dynamic-dlina1) (load "joints.lsp"))


;;; ------------------------------------------------------------
;;;  ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;;; ------------------------------------------------------------

;; Получить площадь блока панели
(defun get-panel-area (ent / dlina dlina1 area att-list att att-tag att-val pair)
  (setq dlina   0.0
        dlina1  0.0)

  (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
    (progn
      ;; DLINA из атрибутов
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
      (if pair (setq dlina (atof (cdr pair))))

      ;; Если DLINA = 0 — пробуем динамическое свойство "Длина"
      (if (= dlina 0.0)
        (setq dlina (get-dynamic-length ent))
      )

      ;; Длина1 всегда 1190 для расчёта
      (setq dlina1 1190.0)

      ;; Площадь = DLINA × 1190 / 1 000 000
      (if (> dlina 0)
        (setq area (/ (* dlina dlina1) 1000000.0))
        (setq area 0.0)
      )
    )
  )

  area
)


;; Получить все блоки с XData
(defun get-all-attributed-blocks ( / ss i ent blk-list)
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


;; Группировка блоков по полю XData
(defun group-by-field (blk-list field-name / result ent val pair group)
  (setq result '())
  (foreach ent blk-list
    (setq val (XDATA-GET-FIELD ent field-name))
    (if (or (not val) (= val ""))
      (if (= field-name "STATUS")
        (setq val "Не смонтировано")
        (setq val "(не указано)")
      )
    )
    (setq pair (assoc val result))
    (if pair
      (progn
        (setq group (cdr pair))
        (setq group (append group (list ent)))
        (setq result (subst (cons val group) pair result))
      )
      (setq result (append result (list (cons val (list ent)))))
    )
  )
  result
)


;; Суммировать площади
(defun sum-areas (blk-list / total ent)
  (setq total 0.0)
  (foreach ent blk-list
    (setq total (+ total (get-panel-area ent)))
  )
  total
)


;; Подсчитать по статусам
(defun count-by-status (blk-list / statuses ent st pair count)
  (setq statuses '())
  (foreach ent blk-list
    (setq st (XDATA-GET-FIELD ent "STATUS"))
    (if (or (not st) (= st ""))
      (setq st "Не смонтировано")
    )
    (setq pair (assoc st statuses))
    (if pair
      (progn
        (setq count (cdr pair))
        (setq statuses (subst (cons st (1+ count)) pair statuses))
      )
      (setq statuses (append statuses (list (cons st 1))))
    )
  )
  statuses
)


;; Проверка на дефект
(defun is-defect-status (status)
  (or (= status "ДЕФЕКТ_ЗАМЕНА") (= status "ДЕФЕКТ_РЕМОНТ"))
)

;; Проверка на смонтировано
(defun is-installed-status (status)
  (= status "СМОНТИРОВАНО")
)


(defun print-separator ( / i)
  (princ "\n")
  (repeat 60 (princ "─"))
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: ВЕДОМОСТЬ_ПО_ПОДРЯДЧИКАМ
;;; ------------------------------------------------------------
(defun C:ВЕДОМОСТЬ_ПО_ПОДРЯДЧИКАМ ( / blk-list groups total-area)

  (princ "\nСбор данных по подрядчикам...")
  (setq blk-list (get-all-attributed-blocks))

  (if (not blk-list)
    (princ "\nВ чертеже нет блоков с XData.")
    (progn
      (setq groups (group-by-field blk-list "CONTRACTOR")
            total-area 0.0)

      (print-separator)
      (princ "\n       ВЕДОМОСТЬ ПО ПОДРЯДЧИКАМ")
      (print-separator)

      (foreach group groups
        (setq contractor (car group)
              ents       (cdr group)
              area       (sum-areas ents)
              count      (length ents)
              statuses   (count-by-status ents)
              total-area (+ total-area area))

        (princ (strcat "\n\nПодрядчик: " contractor))
        (princ (strcat "\n  Всего панелей: " (itoa count)))
        (princ (strcat "\n  Общая площадь: " (rtos area 2 2) " м²"))
        (princ "\n  По статусам:")
        (foreach st-pair statuses
          (princ (strcat "\n    " (car st-pair) ": " (itoa (cdr st-pair)) " шт."))
        )
      )

      (print-separator)
      (princ (strcat "\nИТОГО по всем подрядчикам: " (rtos total-area 2 2) " м²"))
      (princ (strcat "\nВсего блоков: " (itoa (length blk-list))))
      (print-separator)
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: ВЕДОМОСТЬ_ДЕФЕКТОВ
;;; ------------------------------------------------------------
(defun C:ВЕДОМОСТЬ_ДЕФЕКТОВ ( / blk-list defect-list count total-area ent status desc area contractor queue)

  (princ "\nПоиск дефектных панелей...")
  (setq blk-list    (get-all-attributed-blocks)
        defect-list '()
        total-area  0.0)

  (if (not blk-list)
    (princ "\nВ чертеже нет блоков с XData.")
    (progn
      (foreach ent blk-list
        (setq status (XDATA-GET-FIELD ent "STATUS"))
        (if (and status (is-defect-status status))
          (setq defect-list (append defect-list (list ent)))
        )
      )

      (if (not defect-list)
        (princ "\nДефектных панелей не найдено.")
        (progn
          (print-separator)
          (princ "\n            ВЕДОМОСТЬ ДЕФЕКТНЫХ ПАНЕЛЕЙ")
          (print-separator)

          (setq count 1)
          (foreach ent defect-list
            (setq desc       (XDATA-GET-FIELD ent "DEFECT_DESC")
                  area       (get-panel-area ent)
                  contractor (XDATA-GET-FIELD ent "CONTRACTOR")
                  queue      (XDATA-GET-FIELD ent "QUEUE")
                  status     (XDATA-GET-FIELD ent "STATUS")
                  total-area (+ total-area area))

            (if (not desc) (setq desc "(нет описания)"))
            (if (not contractor) (setq contractor "(не указан)"))
            (if (not queue) (setq queue "(не указана)"))

            (if (= status "ДЕФЕКТ_ЗАМЕНА") (setq status "ПОД ЗАМЕНУ"))
            (if (= status "ДЕФЕКТ_РЕМОНТ") (setq status "ВОЗМОЖЕН РЕМОНТ"))

            (princ (strcat "\n\n" (itoa count) ". Площадь: " (rtos area 2 2) " м²"))
            (princ (strcat "\n   Статус: " status))
            (princ (strcat "\n   Подрядчик: " contractor))
            (princ (strcat "\n   Очередь: " queue))
            (princ (strcat "\n   Описание: " desc))

            (setq count (1+ count))
          )

          (print-separator)
          (princ (strcat "\nВСЕГО дефектных: " (itoa (length defect-list)) " шт."))
          (princ (strcat "\nОбщая площадь дефектов: " (rtos total-area 2 2) " м²"))
          (print-separator)
        )
      )
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: ВЕДОМОСТЬ_ПО_ОЧЕРЕДЯМ
;;; ------------------------------------------------------------
(defun C:ВЕДОМОСТЬ_ПО_ОЧЕРЕДЯМ ( / blk-list groups total-area)

  (princ "\nСбор данных по очередям...")
  (setq blk-list (get-all-attributed-blocks))

  (if (not blk-list)
    (princ "\nВ чертеже нет блоков с XData.")
    (progn
      (setq groups (group-by-field blk-list "QUEUE")
            total-area 0.0)

      (print-separator)
      (princ "\n          ВЕДОМОСТЬ ПО ОЧЕРЕДЯМ")
      (print-separator)

      (foreach group groups
        (setq queue  (car group)
              ents   (cdr group)
              area   (sum-areas ents)
              count  (length ents)
              statuses (count-by-status ents)
              total-area (+ total-area area))

        (princ (strcat "\n\nОчередь: " queue))
        (princ (strcat "\n  Всего панелей: " (itoa count)))
        (princ (strcat "\n  Общая площадь: " (rtos area 2 2) " м²"))
        (princ "\n  По статусам:")
        (foreach st-pair statuses
          (princ (strcat "\n    " (car st-pair) ": " (itoa (cdr st-pair)) " шт."))
        )
      )

      (print-separator)
      (princ (strcat "\nИТОГО по всем очередям: " (rtos total-area 2 2) " м²"))
      (princ (strcat "\nВсего блоков: " (itoa (length blk-list))))
      (print-separator)
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: КРАТКАЯ_СВОДКА
;;; ------------------------------------------------------------
(defun C:КРАТКАЯ_СВОДКА ( / blk-list total-area installed-area defect-zamena-area
                           defect-remont-area not-installed-area
                           installed-count defect-zamena-count
                           defect-remont-count not-installed-count
                           ent area status)

  (princ "\nФормирование краткой сводки...")
  (setq blk-list (get-all-attributed-blocks))

  (if (not blk-list)
    (princ "\nВ чертеже нет блоков с XData.")
    (progn
      (setq total-area          0.0
            installed-area      0.0
            defect-zamena-area  0.0
            defect-remont-area  0.0
            not-installed-area  0.0
            installed-count     0
            defect-zamena-count 0
            defect-remont-count 0
            not-installed-count 0)

      (foreach ent blk-list
        (setq area   (get-panel-area ent)
              status (XDATA-GET-FIELD ent "STATUS")
              total-area (+ total-area area))

        (cond
          ((is-installed-status status)
           (setq installed-area (+ installed-area area)
                 installed-count (1+ installed-count)))
          ((= status "ДЕФЕКТ_ЗАМЕНА")
           (setq defect-zamena-area (+ defect-zamena-area area)
                 defect-zamena-count (1+ defect-zamena-count)))
          ((= status "ДЕФЕКТ_РЕМОНТ")
           (setq defect-remont-area (+ defect-remont-area area)
                 defect-remont-count (1+ defect-remont-count)))
          (t
           (setq not-installed-area (+ not-installed-area area)
                 not-installed-count (1+ not-installed-count)))
        )
      )

      (print-separator)
      (princ "\n               КРАТКАЯ СВОДКА ПО ОБЪЕКТУ")
      (print-separator)
      (princ (strcat "\nВсего панелей: " (itoa (length blk-list)) " шт."))
      (princ (strcat "\nОбщая площадь: " (rtos total-area 2 2) " м²"))
      (princ "\n")
      (princ (strcat "\n  Смонтировано:              " (itoa installed-count) " шт. (" (rtos installed-area 2 2) " м²)"))
      (princ (strcat "\n  Дефект под замену:         " (itoa defect-zamena-count) " шт. (" (rtos defect-zamena-area 2 2) " м²)"))
      (princ (strcat "\n  Дефект с возможн. ремонта: " (itoa defect-remont-count) " шт. (" (rtos defect-remont-area 2 2) " м²)"))
      (princ (strcat "\n  Не смонтировано:           " (itoa not-installed-count) " шт. (" (rtos not-installed-area 2 2) " м²)"))
      (print-separator)
    )
  )
  (princ)
)

;;; ------------------------------------------------------------
;;;  КОМАНДА: СВОДКА_ПО_ВЫДЕЛЕНИЮ
;;; ------------------------------------------------------------
(defun C:СВОДКА_ПО_ВЫДЕЛЕНИЮ ( / ss i ent blk-list total-area
                                installed-area defect-zamena-area defect-remont-area not-installed-area
                                installed-count defect-zamena-count defect-remont-count not-installed-count
                                area status)

  (princ "\nВыберите панели рамкой...")
  (setq ss (ssget '((0 . "INSERT"))))

  (if (not ss)
    (princ "\nНичего не выбрано.")
    (progn
      ;; Собираем только панели с XData
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
          (setq total-area          0.0
                installed-area      0.0
                defect-zamena-area  0.0
                defect-remont-area  0.0
                not-installed-area  0.0
                installed-count     0
                defect-zamena-count 0
                defect-remont-count 0
                not-installed-count 0)

          (foreach ent blk-list
            (setq area   (get-panel-area ent)
                  status (XDATA-GET-FIELD ent "STATUS")
                  total-area (+ total-area area))

            (cond
              ((= status "СМОНТИРОВАНО")
               (setq installed-area (+ installed-area area)
                     installed-count (1+ installed-count)))
              ((= status "ДЕФЕКТ_ЗАМЕНА")
               (setq defect-zamena-area (+ defect-zamena-area area)
                     defect-zamena-count (1+ defect-zamena-count)))
              ((= status "ДЕФЕКТ_РЕМОНТ")
               (setq defect-remont-area (+ defect-remont-area area)
                     defect-remont-count (1+ defect-remont-count)))
              (t
               (setq not-installed-area (+ not-installed-area area)
                     not-installed-count (1+ not-installed-count)))
            )
          )

          (print-separator)
          (princ "\n       СВОДКА ПО ВЫДЕЛЕННЫМ ПАНЕЛЯМ")
          (print-separator)
          (princ (strcat "\nВсего выделено: " (itoa (length blk-list)) " шт."))
          (princ (strcat "\nОбщая площадь: " (rtos total-area 2 2) " м²"))
          (princ "\n")
          (princ (strcat "\n  Смонтировано:              " (itoa installed-count) " шт. (" (rtos installed-area 2 2) " м²)"))
          (princ (strcat "\n  Дефект под замену:         " (itoa defect-zamena-count) " шт. (" (rtos defect-zamena-area 2 2) " м²)"))
          (princ (strcat "\n  Дефект с возможн. ремонта: " (itoa defect-remont-count) " шт. (" (rtos defect-remont-area 2 2) " м²)"))
          (princ (strcat "\n  Не смонтировано:           " (itoa not-installed-count) " шт. (" (rtos not-installed-area 2 2) " м²)"))
          (print-separator)
        )
      )
    )
  )
  (princ)
)

(princ "\nМодуль отчётов v2.0 загружен. Команды: ВЕДОМОСТЬ_ПО_ПОДРЯДЧИКАМ, ВЕДОМОСТЬ_ДЕФЕКТОВ, ВЕДОМОСТЬ_ПО_ОЧЕРЕДЯМ, КРАТКАЯ_СВОДКА")
(princ)