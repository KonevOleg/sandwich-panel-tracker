;;; ============================================================
;;; МОДУЛЬ ВЫГРУЗКИ В EXCEL v3.2
;;; Создаёт xlsx-файл с отчётами по проекту
;;; Не сбрасывает переменные assign.lsp
;;; ============================================================
;;; Зависимости: xdata.lsp, reports.lsp, joints.lsp
;;; Требования: установленный Microsoft Excel
;;; ============================================================

(if (not XDATA-SET)      (load "xdata.lsp"))
(if (not get-panel-area) (load "reports.lsp"))
(if (not calc-vertical)  (load "joints.lsp"))


(defun get-dwg-folder ( / dwgname)
  (if (= (getvar "DWGNAME") "")
    (getvar "DWGPREFIX")
    (getvar "DWGPREFIX")
  )
)

(defun get-dwg-basename ( / dwgname)
  (if (= (getvar "DWGNAME") "")
    "Отчёт"
    (vl-filename-base (getvar "DWGNAME"))
  )
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: ВЫГРУЗИТЬ_В_EXCEL
;;; ------------------------------------------------------------
(defun C:ВЫГРУЗИТЬ_В_EXCEL ( / ss idx ent all-pnl inst-pnl
                              xl xl-book xl-sheets xl-s1 xl-s2 xl-s3 cell
                              tt-area tt-cuts in-area dz-area dr-area ni-area
                              in-count dz-count dr-count ni-count
                              ar st cuts
                              groups contractor ents cnt statuses inst dz dr ni
                              av ah iv ih filepath)

  (princ "\nПодготовка данных...")

  ;; ---- Сбор панелей ----
  (setq all-pnl '()
        ss (ssget "_X" '((0 . "INSERT")))
        idx 0)

  (if (not ss)
    (princ "\nНет блоков в чертеже.")
    (progn
      (repeat (sslength ss)
        (setq ent (ssname ss idx))
        (if (XDATA-P ent)
          (setq all-pnl (append all-pnl (list ent)))
        )
        (setq idx (1+ idx))
      )

      (if (not all-pnl)
        (princ "\nНет панелей с XData. Сначала выполните ВЗЯТЬ_В_РАБОТУ.")
        (progn
          (princ (strcat "\nНайдено панелей: " (itoa (length all-pnl))))

          ;; Смонтированные
          (setq inst-pnl '())
          (foreach ent all-pnl
            (if (= (XDATA-GET-FIELD ent "STATUS") "СМОНТИРОВАНО")
              (setq inst-pnl (append inst-pnl (list ent)))
            )
          )

          ;; Счётчики
          (setq tt-area 0.0 tt-cuts 0
                in-area 0.0 dz-area 0.0 dr-area 0.0 ni-area 0.0
                in-count 0 dz-count 0 dr-count 0 ni-count 0)

          (foreach ent all-pnl
            (setq ar  (get-panel-area ent)
                  st  (XDATA-GET-FIELD ent "STATUS")
                  tt-area (+ tt-area ar))
            (setq cuts (XDATA-GET-FIELD ent "CUTOUTS"))
            (if cuts (setq tt-cuts (+ tt-cuts (atoi cuts))))
            (cond
              ((= st "СМОНТИРОВАНО")  (setq in-area (+ in-area ar) in-count (1+ in-count)))
              ((= st "ДЕФЕКТ_ЗАМЕНА") (setq dz-area (+ dz-area ar) dz-count (1+ dz-count)))
              ((= st "ДЕФЕКТ_РЕМОНТ") (setq dr-area (+ dr-area ar) dr-count (1+ dr-count)))
              (t                      (setq ni-area (+ ni-area ar) ni-count (1+ ni-count)))
            )
          )

          ;; Примыкания
          (setq av (calc-vertical all-pnl)  ah (calc-horizontal all-pnl))
          (if inst-pnl
            (setq iv (calc-vertical inst-pnl)  ih (calc-horizontal inst-pnl))
            (setq iv (list 0.0 0 0)  ih (list 0.0 0))
          )

          ;; ---- Excel ----
          (princ "\nСоздание Excel...")
          (setq xl (vl-catch-all-apply 'vlax-get-object (list "Excel.Application")))
          (if (or (vl-catch-all-error-p xl) (not xl))
            (setq xl (vlax-create-object "Excel.Application"))
          )
          (vlax-put-property xl "Visible" :vlax-true)
          (setq xl-book (vlax-invoke (vlax-get-property xl "Workbooks") "Add"))
          (setq xl-sheets (vlax-get-property xl-book "Sheets"))

          ;; ======== ЛИСТ 1: СВОДКА ========
          (setq xl-s1 (vlax-get-property xl-sheets "Item" 1))
          (vlax-put-property xl-s1 "Name" "Сводка")
          (setq cell (vlax-get-property xl-s1 "Cells"))
          (vlax-put-property cell "NumberFormat" "@")

          (vlax-put-property cell "Item" 1 1 "КРАТКАЯ СВОДКА ПО ОБЪЕКТУ")
          (vlax-put-property cell "Item" 2 1 "Показатель")
          (vlax-put-property cell "Item" 2 2 "Количество")
          (vlax-put-property cell "Item" 2 3 "Площадь, м²")
          (vlax-put-property cell "Item" 3 1 "Всего панелей")
          (vlax-put-property cell "Item" 3 2 (itoa (length all-pnl)))
          (vlax-put-property cell "Item" 3 3 (rtos tt-area 2 2))
          (vlax-put-property cell "Item" 4 1 "Смонтировано")
          (vlax-put-property cell "Item" 4 2 (itoa in-count))
          (vlax-put-property cell "Item" 4 3 (rtos in-area 2 2))
          (vlax-put-property cell "Item" 5 1 "Дефект под замену")
          (vlax-put-property cell "Item" 5 2 (itoa dz-count))
          (vlax-put-property cell "Item" 5 3 (rtos dz-area 2 2))
          (vlax-put-property cell "Item" 6 1 "Дефект с ремонтом")
          (vlax-put-property cell "Item" 6 2 (itoa dr-count))
          (vlax-put-property cell "Item" 6 3 (rtos dr-area 2 2))
          (vlax-put-property cell "Item" 7 1 "Не смонтировано")
          (vlax-put-property cell "Item" 7 2 (itoa ni-count))
          (vlax-put-property cell "Item" 7 3 (rtos ni-area 2 2))
          (vlax-put-property cell "Item" 8 1 "Количество резов")
          (vlax-put-property cell "Item" 8 2 (itoa tt-cuts))
          (vlax-put-property cell "Item" 8 3 "-")
          ;; AutoFit убран для скорости
          (princ "\n  Лист 'Сводка' готов.")

          ;; ======== ЛИСТ 2: ПОДРЯДЧИКИ ========
          (setq xl-s2 (vlax-invoke xl-sheets "Add" nil xl-s1))
          (vlax-put-property xl-s2 "Name" "Подрядчики")
          (setq cell (vlax-get-property xl-s2 "Cells"))
          (vlax-put-property cell "NumberFormat" "@")

          (vlax-put-property cell "Item" 1 1 "ВЕДОМОСТЬ ПО ПОДРЯДЧИКАМ")
          (vlax-put-property cell "Item" 2 1 "Подрядчик")
          (vlax-put-property cell "Item" 2 2 "Панелей")
          (vlax-put-property cell "Item" 2 3 "Площадь, м²")
          (vlax-put-property cell "Item" 2 4 "Смонт.")
          (vlax-put-property cell "Item" 2 5 "Деф.зам.")
          (vlax-put-property cell "Item" 2 6 "Деф.рем.")
          (vlax-put-property cell "Item" 2 7 "Не нач.")

          (setq groups (group-by-field all-pnl "CONTRACTOR"))
          (setq idx 3)
          (foreach group groups
            (setq contractor (car group)
                  ents       (cdr group)
                  area       (sum-areas ents)
                  cnt        (length ents)
                  statuses   (count-by-status ents))
            (setq inst (cdr (assoc "СМОНТИРОВАНО" statuses))
                  dz   (cdr (assoc "ДЕФЕКТ_ЗАМЕНА" statuses))
                  dr   (cdr (assoc "ДЕФЕКТ_РЕМОНТ" statuses))
                  ni   (cdr (assoc "НЕ СМОНТИРОВАНО" statuses)))
            (if (not inst) (setq inst 0))
            (if (not dz) (setq dz 0))
            (if (not dr) (setq dr 0))
            (if (not ni) (setq ni 0))
            (vlax-put-property cell "Item" idx 1 contractor)
            (vlax-put-property cell "Item" idx 2 (itoa cnt))
            (vlax-put-property cell "Item" idx 3 (rtos area 2 2))
            (vlax-put-property cell "Item" idx 4 (itoa inst))
            (vlax-put-property cell "Item" idx 5 (itoa dz))
            (vlax-put-property cell "Item" idx 6 (itoa dr))
            (vlax-put-property cell "Item" idx 7 (itoa ni))
            (setq idx (1+ idx))
          )
          ;; AutoFit убран для скорости
          (princ "\n  Лист 'Подрядчики' готов.")

          ;; ======== ЛИСТ 3: ПРИМЫКАНИЯ ========
          (setq xl-s3 (vlax-invoke xl-sheets "Add" nil xl-s2))
          (vlax-put-property xl-s3 "Name" "Примыкания")
          (setq cell (vlax-get-property xl-s3 "Cells"))
          (vlax-put-property cell "NumberFormat" "@")

          (vlax-put-property cell "Item" 1 1 "ПРИМЫКАНИЯ И СТЫКИ")
          (vlax-put-property cell "Item" 2 1 "Показатель")
          (vlax-put-property cell "Item" 2 2 "Все панели")
          (vlax-put-property cell "Item" 2 3 "Смонтированные")
          (vlax-put-property cell "Item" 3 1 "Вертикальные грани, м")
          (vlax-put-property cell "Item" 3 2 (rtos (/ (car av) 1000.0) 2 2))
          (vlax-put-property cell "Item" 3 3 (rtos (/ (car iv) 1000.0) 2 2))
          (vlax-put-property cell "Item" 4 1 "Горизонтальные стыки, м")
          (vlax-put-property cell "Item" 4 2 (rtos (/ (car ah) 1000.0) 2 2))
          (vlax-put-property cell "Item" 4 3 (rtos (/ (car ih) 1000.0) 2 2))
          ;; AutoFit убран для скорости
          (princ "\n  Лист 'Примыкания' готов.")

          ;; ---- Сохранение ----
          (setq filepath (strcat (get-dwg-folder) (get-dwg-basename) "_отчёт.xlsx"))
          (vlax-invoke xl-book "SaveAs" filepath 51)
          (princ (strcat "\nФайл сохранён: " filepath))
          (princ "\nГотово.")
        )
      )
    )
  )
  (princ)
)


(princ "\nМодуль Excel v3.2 загружен. Команда: ВЫГРУЗИТЬ_В_EXCEL")
(princ)