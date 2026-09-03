;;; ============================================================
;;; МОДУЛЬ ТАБЛИЦ v2.5
;;; Таблицы AutoCAD с масштабом x100, площадью не смонтированных и учётом резов
;;; ============================================================
;;; Зависимости: xdata.lsp
;;; ============================================================

(if (not XDATA-SET)
  (load "xdata.lsp")
)


;;; ------------------------------------------------------------
;;;  ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;;; ------------------------------------------------------------

(defun create-table (point title headers data / table rows cols row col)
  (setq rows (+ (length data) 2)
        cols (length headers))

  (setq table (vla-AddTable
                (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                (vlax-3d-point point)
                rows cols 1500 8000))

  (vla-SetText table 0 0 title)
  (vla-SetCellTextHeight table 0 0 700.0)
  (vla-SetCellAlignment table 0 0 acMiddleCenter)
  (vla-MergeCells table 0 0 0 (1- cols))

  (setq col 0)
  (foreach h headers
    (vla-SetText table 1 col h)
    (vla-SetCellTextHeight table 1 col 500.0)
    (vla-SetCellAlignment table 1 col acMiddleCenter)
    (setq col (1+ col))
  )

  (setq row 2)
  (foreach data-row data
    (setq col 0)
    (foreach cell data-row
      (vla-SetText table row col (if cell cell ""))
      (vla-SetCellTextHeight table row col 400.0)
      (vla-SetCellAlignment table row col acMiddleLeft)
      (setq col (1+ col))
    )
    (setq row (1+ row))
  )

  table
)


(defun get-insertion-point ( / pt)
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
  (if (not pt) (setq pt '(0 0 0)))
  pt
)


(defun is-defect-status (status)
  (or (= status "ДЕФЕКТ_ЗАМЕНА") (= status "ДЕФЕКТ_РЕМОНТ"))
)


(defun collect-blocks ( / ss i ent blk-list)
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


;;; ------------------------------------------------------------
;;;  ТАБЛИЦА_СВОДКА
;;; ------------------------------------------------------------
(defun C:ТАБЛИЦА_СВОДКА ( / blk-list total-area total-cuts
                           installed-area defect-zamena-area defect-remont-area not-installed-area
                           installed-count defect-zamena-count defect-remont-count not-installed-count
                           pt table ent area status cuts)

  (princ "\nФормирование краткой сводки...")
  (setq blk-list (collect-blocks))

  (if (not blk-list)
    (princ "\nНет панелей с XData. Сначала выполните ВЗЯТЬ_В_РАБОТУ.")

    (progn
      (setq total-area           0.0
            total-cuts           0
            installed-area       0.0
            defect-zamena-area   0.0
            defect-remont-area   0.0
            not-installed-area   0.0
            installed-count      0
            defect-zamena-count  0
            defect-remont-count  0
            not-installed-count  0)

      (foreach ent blk-list
        (setq area   (get-panel-area ent)
              status (XDATA-GET-FIELD ent "STATUS")
              total-area (+ total-area area))

        ;; Считаем резы
        (setq cuts (XDATA-GET-FIELD ent "CUTOUTS"))
        (if cuts
          (setq total-cuts (+ total-cuts (atoi cuts)))
        )

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

      (setq pt (get-insertion-point))

      (setq table (vla-AddTable
                    (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                    (vlax-3d-point pt)
                    8 3 2000 10000))

      (vla-SetText table 0 0 "КРАТКАЯ СВОДКА ПО ОБЪЕКТУ")
      (vla-SetCellTextHeight table 0 0 800.0)
      (vla-SetCellAlignment table 0 0 acMiddleCenter)
      (vla-MergeCells table 0 0 0 2)

      (vla-SetText table 1 0 "Показатель")
      (vla-SetText table 1 1 "Количество, шт.")
      (vla-SetText table 1 2 "Площадь, м²")

      (vla-SetText table 2 0 "Всего панелей")
      (vla-SetText table 2 1 (itoa (length blk-list)))
      (vla-SetText table 2 2 (rtos total-area 2 2))

      (vla-SetText table 3 0 "Смонтировано")
      (vla-SetText table 3 1 (itoa installed-count))
      (vla-SetText table 3 2 (rtos installed-area 2 2))

      (vla-SetText table 4 0 "Дефект под замену")
      (vla-SetText table 4 1 (itoa defect-zamena-count))
      (vla-SetText table 4 2 (rtos defect-zamena-area 2 2))

      (vla-SetText table 5 0 "Дефект с ремонтом")
      (vla-SetText table 5 1 (itoa defect-remont-count))
      (vla-SetText table 5 2 (rtos defect-remont-area 2 2))

      (vla-SetText table 6 0 "Не смонтировано")
      (vla-SetText table 6 1 (itoa not-installed-count))
      (vla-SetText table 6 2 (rtos not-installed-area 2 2))

      (vla-SetText table 7 0 "Количество резов")
      (vla-SetText table 7 1 (itoa total-cuts))
      (vla-SetText table 7 2 "-")

      ;; Высота текста
      (setq row 1)
      (repeat 7
        (setq col 0)
        (repeat 3
          (vla-SetCellTextHeight table row col 400.0)
          (vla-SetCellAlignment table row col acMiddleCenter)
          (setq col (1+ col))
        )
        (setq row (1+ row))
      )

      (princ "\nСводная таблица создана.")
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  ТАБЛИЦА_ПОДРЯДЧИКИ
;;; ------------------------------------------------------------
(defun C:ТАБЛИЦА_ПОДРЯДЧИКИ ( / blk-list groups pt contractor ents
                               area count statuses installed defect-zamena defect-remont not-inst data)

  (princ "\nФормирование таблицы по подрядчикам...")
  (setq blk-list (collect-blocks))

  (if (not blk-list)
    (princ "\nНет панелей с XData.")

    (progn
      (setq groups (group-by-field blk-list "CONTRACTOR"))
      (setq data '())

      (foreach group groups
        (setq contractor (car group)
              ents       (cdr group)
              area       (sum-areas ents)
              count      (length ents)
              statuses   (count-by-status ents))

        (setq installed      (cdr (assoc "СМОНТИРОВАНО" statuses))
              defect-zamena  (cdr (assoc "ДЕФЕКТ_ЗАМЕНА" statuses))
              defect-remont  (cdr (assoc "ДЕФЕКТ_РЕМОНТ" statuses))
              not-inst       (cdr (assoc "НЕ СМОНТИРОВАНО" statuses)))

        (if (not installed) (setq installed 0))
        (if (not defect-zamena) (setq defect-zamena 0))
        (if (not defect-remont) (setq defect-remont 0))
        (if (not not-inst) (setq not-inst 0))

        (setq data (append data
                           (list (list contractor
                                       (itoa count)
                                       (rtos area 2 2)
                                       (itoa installed)
                                       (itoa defect-zamena)
                                       (itoa defect-remont)
                                       (itoa not-inst)))))
      )

      (setq pt (get-insertion-point))
      (create-table pt "ВЕДОМОСТЬ ПО ПОДРЯДЧИКАМ"
                    (list "Подрядчик" "Панелей" "Площадь, м²" "Смонт." "Деф.зам." "Деф.рем." "Не нач.")
                    data)
      (princ "\nТаблица по подрядчикам создана.")
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  ТАБЛИЦА_ДЕФЕКТЫ
;;; ------------------------------------------------------------
(defun C:ТАБЛИЦА_ДЕФЕКТЫ ( / blk-list defect-list pt status contractor queue desc area num data)

  (princ "\nФормирование таблицы дефектов...")
  (setq blk-list (collect-blocks))

  (if (not blk-list)
    (princ "\nНет панелей с XData.")

    (progn
      (setq defect-list '())
      (foreach ent blk-list
        (setq status (XDATA-GET-FIELD ent "STATUS"))
        (if (is-defect-status status)
          (setq defect-list (append defect-list (list ent)))
        )
      )

      (if (not defect-list)
        (princ "\nДефектных панелей не найдено.")

        (progn
          (setq data '()
                num 1)

          (foreach ent defect-list
            (setq area       (get-panel-area ent)
                  status     (XDATA-GET-FIELD ent "STATUS")
                  contractor (XDATA-GET-FIELD ent "CONTRACTOR")
                  queue      (XDATA-GET-FIELD ent "QUEUE")
                  desc       (XDATA-GET-FIELD ent "DEFECT_DESC"))

            (if (= status "ДЕФЕКТ_ЗАМЕНА") (setq status "Под замену"))
            (if (= status "ДЕФЕКТ_РЕМОНТ") (setq status "Ремонт"))
            (if (not contractor) (setq contractor "-"))
            (if (not queue) (setq queue "-"))
            (if (not desc) (setq desc "-"))

            (setq data (append data
                               (list (list (itoa num)
                                           status
                                           contractor
                                           queue
                                           (rtos area 2 2)
                                           desc))))
            (setq num (1+ num))
          )

          (setq pt (get-insertion-point))
          (create-table pt "ВЕДОМОСТЬ ДЕФЕКТНЫХ ПАНЕЛЕЙ"
                        (list "№" "Статус" "Подрядчик" "Очередь" "Площадь, м²" "Описание")
                        data)
          (princ "\nТаблица дефектов создана.")
        )
      )
    )
  )
  (princ)
)



;;; ------------------------------------------------------------
;;;  ТАБЛИЦА_МАРОК
;;; ------------------------------------------------------------

;; Получить строковое значение атрибута
(defun get-attr-str (ent tag / att-list att att-tag att-val pair)
  (setq att-list nil)
  (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
    (if (= (cdr (assoc 66 (entget ent))) 1)
      (progn
        (setq att (entnext ent))
        (while (and att (= (cdr (assoc 0 (entget att))) "ATTRIB"))
          (setq att-tag (strcase (cdr (assoc 2 (entget att))))
                att-val (cdr (assoc 1 (entget att))))
          (setq att-list (append att-list (list (cons att-tag att-val))))
          (setq att (entnext att))
        )
      )
    )
  )
  (setq pair (assoc (strcase tag) att-list))
  (if pair (cdr pair) "")
)

;; Получить числовое значение атрибута
(defun get-attr-value (ent tag / val)
  (setq val (get-attr-str ent tag))
  (if (= val "")
    0.0
    (atof val)
  )
)

;; Группировка блоков по значению атрибута
(defun group-by-attr (blk-list attr-name / result ent val pair group)
  (setq result '())
  (foreach ent blk-list
    (setq val (get-attr-str ent attr-name))
    (if (or (not val) (= val "")) (setq val "(без марки)"))
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
(defun C:ТАБЛИЦА_МАРОК ( / blk-list groups pt data group mark ents
                          dlina dlina1 area-per-panel count total-area)

  (princ "\nФормирование таблицы по маркам...")
  (setq blk-list (collect-blocks))

  (if (not blk-list)
    (princ "\nНет панелей с XData.")
    (progn
      ;; Группируем по марке (атрибут POZ)
      (setq groups (group-by-attr blk-list "POZ"))
      (setq data '())

      (foreach group groups
        (setq mark (car group)
              ents (cdr group)
              count (length ents)
              total-area 0.0
              dlina 0.0
              dlina1 0.0)

        ;; Площадь одной панели (первой в группе)
        (if ents
          (progn
            (setq dlina (get-attr-value (car ents) "DLINA")
                  dlina1 (get-dynamic-dlina1 (car ents)))
            (if (and dlina dlina1 (> dlina 0) (> dlina1 0))
              (setq area-per-panel (/ (* dlina dlina1) 1000000.0))
              (setq area-per-panel 0.0)
            )
          )
        )

        ;; Суммарная площадь всех панелей марки
        (foreach ent ents
          (setq total-area (+ total-area (get-panel-area ent)))
        )

        (setq data (append data
                           (list (list mark
                                       (rtos dlina 2 0)
                                       (rtos dlina1 2 0)
                                       (rtos area-per-panel 2 3)
                                       (itoa count)
                                       (rtos total-area 2 2)))))
      )

      ;; Сортируем по марке
      (setq data (vl-sort data '(lambda (a b) (< (car a) (car b)))))

      ;; Добавляем нумерацию
      (setq idx 1)
      (setq numbered-data '())
      (foreach row data
        (setq numbered-data (append numbered-data
                                    (list (append (list (itoa idx)) row))))
        (setq idx (1+ idx))
      )

      (setq pt (get-insertion-point))
      (create-table pt "ВЕДОМОСТЬ ПО МАРКАМ"
                    (list "№" "Марка" "Длина, мм" "Высота, мм" "Площадь 1 панели, м²" "Кол-во, шт." "Общая площадь, м²")
                    numbered-data)
      (princ "\nТаблица по маркам создана.")
    )
  )
  (princ)
)

(princ "\nМодуль таблиц v2.5 загружен.")
(princ)