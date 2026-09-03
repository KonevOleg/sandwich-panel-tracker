;;; ============================================================
;;; МОДУЛЬ НАЗНАЧЕНИЯ v2.4
;;; Штриховка по Handle + автоинициализация при открытии файла
;;; ============================================================
;;; Зависимости: xdata.lsp
;;; ============================================================


;;; ------------------------------------------------------------
;;;  НАСТРОЙКИ
;;; ------------------------------------------------------------

(setq *CONTRACTORS*
      (list
        (cons "YD" "Овчинников")
        (cons "RD" "Рамзан")
        (cons "SB" "Васильев")
        (cons "AA" "Зинченко")
        (cons "RZ" "Разметелевские")
        (cons "SH" "Шумбасов")
        (cons "PR" "Привезенов")
        (cons "ND" "Неизвестно")
      )
)

(setq *STATUSES*
      (list
        "НЕ СМОНТИРОВАНО"
        "СМОНТИРОВАНО"
        "ДЕФЕКТ_ЗАМЕНА"
        "ДЕФЕКТ_РЕМОНТ"
      )
)

(setq *STATUS-HATCH*
      (list
        (cons "НЕ СМОНТИРОВАНО"  "ANSI31")
        (cons "СМОНТИРОВАНО"     nil)
        (cons "ДЕФЕКТ_ЗАМЕНА"    "ANSI31")
        (cons "ДЕФЕКТ_РЕМОНТ"    "ANSI31")
      )
)

(setq *STATUS-HATCH-COLOR*
      (list
        (cons "НЕ СМОНТИРОВАНО"  8)
        (cons "СМОНТИРОВАНО"     nil)
        (cons "ДЕФЕКТ_ЗАМЕНА"    1)
        (cons "ДЕФЕКТ_РЕМОНТ"    2)
      )
)

(setq *HATCH-SCALE* 50.0)

(setq *CUTOUT-SCALE* 100.0)

;;; ------------------------------------------------------------
;;;  ПЕРЕМЕННЫЕ СОСТОЯНИЯ
;;; ------------------------------------------------------------

(setq *PANEL-LAYER* nil)
(setq *SANDWICH-INITIALIZED* nil)


;;; ------------------------------------------------------------
;;;  ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;;; ------------------------------------------------------------

(defun get-contractor-name (code)
  (cdr (assoc code *CONTRACTORS*))
)

(defun get-today-string ()
  (menucmd "M=$(edtime,0,YYYY-MO-DD)")
)

(defun get-current-user ()
  (getvar "LOGINNAME")
)

(defun get-handle (ent)
  (cdr (assoc 5 (entget ent)))
)


;;; ------------------------------------------------------------
;;;  АВТОИНИЦИАЛИЗАЦИЯ
;;; ------------------------------------------------------------
(defun auto-init ( / ss i ent layer found)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if (not ss)
    nil
    (progn
      (setq i 0 found nil)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (and (not found) (XDATA-P ent))
          (progn
            (setq layer (cdr (assoc 8 (entget ent))))
            (if layer
              (progn
                (setq *PANEL-LAYER* layer
                      *SANDWICH-INITIALIZED* T
                      found T)
                (princ (strcat "\nПроект автоинициализирован. Слой: \"" layer "\""))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      found
    )
  )
)


;;; ------------------------------------------------------------
;;;  ОПРЕДЕЛИТЬ СЛОЙ ПО ОБРАЗЦУ
;;; ------------------------------------------------------------
(defun detect-panel-layer ( / ent elist layer)
  (princ "\nУкажите ЛЮБУЮ панель как образец...")
  (setq ent (car (entsel)))
  (if (not ent)
    (progn (princ "\nНичего не выбрано.") nil)
    (progn
      (setq elist (entget ent) layer (cdr (assoc 8 elist)))
      (if (not layer)
        (progn (princ "\nОшибка: не удалось определить слой.") nil)
        (progn
          (princ (strcat "\nОпределён слой панелей: \"" layer "\""))
          layer
        )
      )
    )
  )
)


;;; ------------------------------------------------------------
;;;  ИНИЦИАЛИЗАЦИЯ ПРОЕКТА
;;; ------------------------------------------------------------
(defun init-project ( / layer)
  (if *SANDWICH-INITIALIZED*
    T
    (if (auto-init)
      T
      (progn
        (setq layer (detect-panel-layer))
        (if layer
          (progn (setq *PANEL-LAYER* layer *SANDWICH-INITIALIZED* T) T)
          nil
        )
      )
    )
  )
)


;;; ------------------------------------------------------------
;;;  ВЫБРАТЬ ПАНЕЛИ
;;; ------------------------------------------------------------
(defun select-panels ( / choice ss)
  (initget "1 2")
  (setq choice (getkword "\n[1-Все панели / 2-Выделенные] <1>: "))
  (if (not choice) (setq choice "1"))
  (princ (strcat "\nВыбрано: " (if (= choice "1") "Все" "Выделенные")))
  (cond
    ((= choice "1")
     (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 8 *PANEL-LAYER*))))
     (if ss
       (princ (strcat "\nНайдено панелей: " (itoa (sslength ss))))
       (princ "\nПанели не найдены."))
    )
    ((= choice "2")
     (princ (strcat "\nВыберите панели рамкой (слой \"" *PANEL-LAYER* "\"):"))
     (setq ss (ssget (list (cons 0 "INSERT") (cons 8 *PANEL-LAYER*))))
     (if ss
       (princ (strcat "\nВыбрано панелей: " (itoa (sslength ss))))
       (princ "\nНичего не выбрано."))
    )
  )
  ss
)


;;; ------------------------------------------------------------
;;;  НАНЕСТИ ШТРИХОВКУ (ActiveX)
;;; ------------------------------------------------------------
(defun hatch-block (ent hatch-type hatch-color / handle obj inspt dlina shirina
                    att-list att att-tag att-val pair pline-obj hatch-obj
                    points-safearray outer-loop pline-ent hatch-ent)

  (remove-block-hatch ent)

  (if (not hatch-type)
    nil
    (progn
      (setq handle (get-handle ent)
            obj    (vlax-ename->vla-object ent)
            inspt  (vlax-get obj 'InsertionPoint))

      (setq dlina 0.0 shirina 0.0 att-list nil)

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

      (setq pair (assoc "DLINA" att-list))
      (if pair (setq dlina (atof (cdr pair))))
      (setq pair (assoc "ШИРИНА" att-list))
      (if pair (setq shirina (atof (cdr pair))))

      (if (or (= dlina 0.0) (= shirina 0.0))
        (progn
          (vla-GetBoundingBox obj 'minpt 'maxpt)
          (setq minpt (vlax-safearray->list minpt)
                maxpt (vlax-safearray->list maxpt)
                dlina  (- (car maxpt) (car minpt))
                shirina (- (cadr maxpt) (cadr minpt))
                inspt  (list (car minpt) (cadr minpt) 0.0))
        )
      )

      (setq points-safearray (vlax-make-safearray vlax-vbDouble (cons 0 7)))
      (vlax-safearray-fill points-safearray
        (list
          (car inspt) (cadr inspt)
          (+ (car inspt) dlina) (cadr inspt)
          (+ (car inspt) dlina) (+ (cadr inspt) shirina)
          (car inspt) (+ (cadr inspt) shirina)))

      (setq pline-obj (vla-AddLightWeightPolyline
                        (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                        points-safearray))
      (vla-put-Closed pline-obj :vlax-true)
      (setq pline-ent (vlax-vla-object->ename pline-obj))

      (setq hatch-obj (vla-AddHatch
                        (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                        acHatchPatternTypePredefined
                        hatch-type
                        :vlax-true
                        acHatchObject))
      (vla-put-PatternScale hatch-obj *HATCH-SCALE*)
      (vla-put-PatternAngle hatch-obj 0)

      (setq outer-loop (vlax-make-safearray vlax-vbObject (cons 0 0)))
      (vlax-safearray-put-element outer-loop 0 pline-obj)
      (vla-AppendOuterLoop hatch-obj outer-loop)
      (vla-Evaluate hatch-obj)

      (setq hatch-ent (vlax-vla-object->ename hatch-obj))

      (if hatch-color
        (vla-put-Color hatch-obj hatch-color)
      )

      (XDATA-SET pline-ent
        (list (cons "HATCH_OWNER" handle) (cons "HATCH_TYPE" "PLINE")))
      (XDATA-SET hatch-ent
        (list (cons "HATCH_OWNER" handle) (cons "HATCH_TYPE" "HATCH")))
    )
  )
)


;;; ------------------------------------------------------------
;;;  УДАЛИТЬ ШТРИХОВКУ И ПОЛИЛИНИЮ
;;; ------------------------------------------------------------
(defun remove-block-hatch (ent / handle ss i he)
  (setq handle (get-handle ent))
  (setq ss (ssget "_X" (list (cons -4 "<OR") (cons 0 "HATCH") (cons 0 "LWPOLYLINE") (cons -4 "OR>"))))
  (if ss
    (progn
      (setq i (1- (sslength ss)))
      (while (>= i 0)
        (setq he (ssname ss i))
        (if (= (XDATA-GET-FIELD he "HATCH_OWNER") handle)
          (entdel he)
        )
        (setq i (1- i))
      )
    )
  )
)


;;; ------------------------------------------------------------
;;;  ОЧИСТИТЬ СТАРЫЕ ШТРИХОВКИ
;;; ------------------------------------------------------------
(defun C:ОЧИСТИТЬ_ШТРИХОВКИ ( / ss i e count)
  (princ "\nУдаление старых штриховок...")
  (setq ss (ssget "_X" (list (cons -4 "<OR") (cons 0 "HATCH") (cons 0 "LWPOLYLINE") (cons -4 "OR>"))))
  (setq i 0 count 0)
  (if ss
    (repeat (sslength ss)
      (setq e (ssname ss i))
      (if (or (XDATA-GET-FIELD e "HATCH_OWNER") (XDATA-GET-FIELD e "HATCH_TYPE"))
        (progn (entdel e) (setq count (1+ count)))
      )
      (setq i (1+ i))
    )
  )
  (princ (strcat "\nУдалено: " (itoa count) " объектов."))
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: ВЗЯТЬ_В_РАБОТУ (со штриховкой)
;;; ------------------------------------------------------------
(defun C:ВЗЯТЬ_В_РАБОТУ ( / ss i ent total hatch-type hatch-color today user count old-regen old-cmdecho)

  (if (not (init-project))
    (princ "\nИнициализация не выполнена.")
    (progn
      (setq ss (select-panels))
      (if (not ss)
        (princ "\nОперация отменена — панели не выбраны.")
        (progn
          (setq total (sslength ss))
          (princ (strcat "\nБудет обработано: " (itoa total) " панелей."))

          (setq hatch-type  (cdr (assoc "НЕ СМОНТИРОВАНО" *STATUS-HATCH*))
                hatch-color (cdr (assoc "НЕ СМОНТИРОВАНО" *STATUS-HATCH-COLOR*))
                today       (get-today-string)
                user        (get-current-user)
                count       0)

          (setq old-regen   (getvar "REGENMODE")
                old-cmdecho (getvar "CMDECHO"))
          (setvar "CMDECHO" 0)
          (setvar "REGENMODE" 0)

          (setq i 0)
          (repeat total
            (setq ent (ssname ss i))

            (XDATA-SET ent
              (list
                (cons "CONTRACTOR"   "")
                (cons "STATUS"       "НЕ СМОНТИРОВАНО")
                (cons "QUEUE"        "")
                (cons "DATE_INSTALL" "")
                (cons "TEAM"         "")
                (cons "DEFECT_DESC"  "")
                (cons "DATE_CHANGED" today)
                (cons "CHANGED_BY"   user)
                (cons "CUTOUTS"      "0")
              )
            )

            (hatch-block ent hatch-type hatch-color)

            (setq i (1+ i) count (1+ count))

            (if (= (rem count 100) 0)
              (princ (strcat "\r  Прогресс: " (itoa count) " из " (itoa total)
                             " (" (rtos (* 100.0 (/ (float count) total)) 2 0) "%)"))
            )
          )

          (setvar "CMDECHO" old-cmdecho)
          (setvar "REGENMODE" old-regen)

          (princ (strcat "\n\nВзято в работу: " (itoa count) " панелей."))
          (princ "\nСтатус: НЕ СМОНТИРОВАНО (серая штриховка).")
        )
      )
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: НАЗНАЧИТЬ_ПОДРЯДЧИКА
;;; ------------------------------------------------------------
(defun C:НАЗНАЧИТЬ_ПОДРЯДЧИКА ( / ss i ent code name today user count xd newdata pair)

  (if (not (init-project))
    (princ "\nИнициализация не выполнена.")
    (progn
      (princ "\nВыберите блоки панелей...")
      (setq ss (ssget (list (cons 0 "INSERT") (cons 8 *PANEL-LAYER*))))
      (if (not ss)
        (princ "\nНичего не выбрано.")
        (progn
          (princ "\n\n===== ПОДРЯДЧИКИ =====")
          (setq i 1)
          (foreach pair *CONTRACTORS*
            (princ (strcat "\n  " (itoa i) ". " (cdr pair)))
            (setq i (1+ i))
          )
          (princ "\n========================")
          (setq code nil)
          (while (not code)
            (initget 1)
            (setq choice (getint "\nВведите номер подрядчика: "))
            (if (and choice (>= choice 1) (<= choice (length *CONTRACTORS*)))
              (setq code (car (nth (1- choice) *CONTRACTORS*)))
              (princ (strcat "\nВведите число от 1 до " (itoa (length *CONTRACTORS*))))
            )
          )
          (setq name (get-contractor-name code)
                today (get-today-string)
                user (get-current-user)
                count 0)
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq xd (XDATA-GET ent))
            (if xd
              (progn
                (setq newdata '())
                (foreach pair xd
                  (cond
                    ((= (car pair) "CONTRACTOR")   (setq newdata (append newdata (list (cons "CONTRACTOR" name)))))
                    ((= (car pair) "DATE_CHANGED") (setq newdata (append newdata (list (cons "DATE_CHANGED" today)))))
                    ((= (car pair) "CHANGED_BY")   (setq newdata (append newdata (list (cons "CHANGED_BY" user)))))
                    (t                              (setq newdata (append newdata (list pair))))
                  )
                )
                (XDATA-SET ent newdata)
              )
              (XDATA-SET ent
                (list
                  (cons "CONTRACTOR"   name)
                  (cons "STATUS"       "НЕ СМОНТИРОВАНО")
                  (cons "QUEUE"        "")
                  (cons "DATE_INSTALL" "")
                  (cons "TEAM"         "")
                  (cons "DEFECT_DESC"  "")
                  (cons "DATE_CHANGED" today)
                  (cons "CHANGED_BY"   user)
                  (cons "CUTOUTS"      "0")
                )
              )
            )
            (setq i (1+ i) count (1+ count))
          )
          (princ (strcat "\n\nНазначено: " (itoa count) " блоков подрядчику \"" name "\""))
        )
      )
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: СМЕНИТЬ_СТАТУС
;;; ------------------------------------------------------------
(defun C:СМЕНИТЬ_СТАТУС ( / ss i ent status hatch-type hatch-color today user count xd newdata pair)

  (if (not (init-project))
    (princ "\nИнициализация не выполнена.")
    (progn
      (princ "\nВыберите блоки панелей...")
      (setq ss (ssget (list (cons 0 "INSERT") (cons 8 *PANEL-LAYER*))))
      (if (not ss)
        (princ "\nНичего не выбрано.")
        (progn
          (princ "\n\n===== СТАТУСЫ =====")
          (princ "\n  1. НЕ СМОНТИРОВАНО — серая штриховка")
          (princ "\n  2. СМОНТИРОВАНО — без штриховки")
          (princ "\n  3. ДЕФЕКТ ПОД ЗАМЕНУ — красная штриховка")
          (princ "\n  4. ДЕФЕКТ С ВОЗМОЖНОСТЬЮ РЕМОНТА — жёлтая штриховка")
          (princ "\n====================")
          (setq status nil)
          (while (not status)
            (initget 1)
            (setq choice (getint "\nВведите номер статуса: "))
            (if (and choice (>= choice 1) (<= choice (length *STATUSES*)))
              (setq status (nth (1- choice) *STATUSES*))
              (princ (strcat "\nВведите число от 1 до " (itoa (length *STATUSES*))))
            )
          )
          (setq hatch-type  (cdr (assoc status *STATUS-HATCH*))
                hatch-color (cdr (assoc status *STATUS-HATCH-COLOR*))
                today       (get-today-string)
                user        (get-current-user)
                count       0)
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq xd (XDATA-GET ent))
            (if xd
              (progn
                (setq newdata '())
                (foreach pair xd
                  (cond
                    ((= (car pair) "STATUS")       (setq newdata (append newdata (list (cons "STATUS" status)))))
                    ((= (car pair) "DATE_CHANGED") (setq newdata (append newdata (list (cons "DATE_CHANGED" today)))))
                    ((= (car pair) "CHANGED_BY")   (setq newdata (append newdata (list (cons "CHANGED_BY" user)))))
                    (t                              (setq newdata (append newdata (list pair))))
                  )
                )
                (XDATA-SET ent newdata)
              )
              (XDATA-SET ent
                (list
                  (cons "CONTRACTOR"   "")
                  (cons "STATUS"       status)
                  (cons "QUEUE"        "")
                  (cons "DATE_INSTALL" "")
                  (cons "TEAM"         "")
                  (cons "DEFECT_DESC"  "")
                  (cons "DATE_CHANGED" today)
                  (cons "CHANGED_BY"   user)
                  (cons "CUTOUTS"      "0")
                )
              )
            )
            (hatch-block ent hatch-type hatch-color)
            (setq i (1+ i) count (1+ count))
          )
          (princ (strcat "\n\nСтатус изменён у " (itoa count) " блоков на \"" status "\""))
        )
      )
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: СМЕНИТЬ_ОЧЕРЕДЬ
;;; ------------------------------------------------------------
(defun C:СМЕНИТЬ_ОЧЕРЕДЬ ( / ss i ent queue user count xd newdata pair)

  (if (not (init-project))
    (princ "\nИнициализация не выполнена.")
    (progn
      (princ "\nВыберите блоки панелей...")
      (setq ss (ssget (list (cons 0 "INSERT") (cons 8 *PANEL-LAYER*))))
      (if (not ss)
        (princ "\nНичего не выбрано.")
        (progn
          (initget 1)
          (setq queue (getstring "\nВведите номер очереди (1, 2, 3...): "))
          (setq user (get-current-user) count 0)
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq xd (XDATA-GET ent))
            (if xd
              (progn
                (setq newdata '())
                (foreach pair xd
                  (cond
                    ((= (car pair) "QUEUE")       (setq newdata (append newdata (list (cons "QUEUE" queue)))))
                    ((= (car pair) "DATE_CHANGED") (setq newdata (append newdata (list (cons "DATE_CHANGED" (get-today-string))))))
                    ((= (car pair) "CHANGED_BY")   (setq newdata (append newdata (list (cons "CHANGED_BY" user)))))
                    (t                              (setq newdata (append newdata (list pair))))
                  )
                )
                (XDATA-SET ent newdata)
              )
              (XDATA-SET ent
                (list
                  (cons "CONTRACTOR"   "")
                  (cons "STATUS"       "НЕ СМОНТИРОВАНО")
                  (cons "QUEUE"        queue)
                  (cons "DATE_INSTALL" "")
                  (cons "TEAM"         "")
                  (cons "DEFECT_DESC"  "")
                  (cons "DATE_CHANGED" (get-today-string))
                  (cons "CHANGED_BY"   user)
                  (cons "CUTOUTS"      "0")
                )
              )
            )
            (setq i (1+ i) count (1+ count))
          )
          (princ (strcat "\nОчередь \"" queue "\" назначена для " (itoa count) " блоков."))
        )
      )
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: ДОБАВИТЬ_РЕЗ
;;; ------------------------------------------------------------
(defun C:ДОБАВИТЬ_РЕЗ ( / ss i ent xd newdata pair cuts today user count)

  (if (not (init-project))
    (princ "\nИнициализация не выполнена.")
    (progn
      (princ "\nВыберите панели с резом...")
      (setq ss (ssget (list (cons 0 "INSERT") (cons 8 *PANEL-LAYER*))))
      (if (not ss)
        (princ "\nНичего не выбрано.")
        (progn
          (setq today (get-today-string) user (get-current-user) count 0)
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq xd (XDATA-GET ent))
            (if xd
              (progn
                (setq cuts (XDATA-GET-FIELD ent "CUTOUTS"))
                (if (not cuts) (setq cuts 0))
                (if (= (type cuts) 'STR) (setq cuts (atoi cuts)))
                (setq cuts (1+ cuts))
                (setq newdata '())
                (foreach pair xd
                  (cond
                    ((= (car pair) "CUTOUTS")     (setq newdata (append newdata (list (cons "CUTOUTS" (itoa cuts))))))
                    ((= (car pair) "DATE_CHANGED") (setq newdata (append newdata (list (cons "DATE_CHANGED" today)))))
                    ((= (car pair) "CHANGED_BY")   (setq newdata (append newdata (list (cons "CHANGED_BY" user)))))
                    (t                              (setq newdata (append newdata (list pair))))
                  )
                )
                (XDATA-SET ent newdata)
                (princ (strcat "\n  Панель: добавлен рез (всего: " (itoa cuts) ")"))
                (setq count (1+ count))
              )
            )
            (setq i (1+ i))
          )
          (princ (strcat "\n\nДобавлены резы для " (itoa count) " панелей."))
        )
      )
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: НАЗНАЧИТЬ_ВСЁ
;;; ------------------------------------------------------------
(defun C:НАЗНАЧИТЬ_ВСЁ ( / ss i ent code name status hatch-type hatch-color today user count xd newdata pair)

  (if (not (init-project))
    (princ "\nИнициализация не выполнена.")
    (progn
      (princ "\nВыберите панели...")
      (setq ss (ssget (list (cons 0 "INSERT") (cons 8 *PANEL-LAYER*))))
      (if (not ss)
        (princ "\nНичего не выбрано.")
        (progn
          (princ "\n\n===== ПОДРЯДЧИК (Enter = не менять) =====")
          (setq i 1)
          (foreach pair *CONTRACTORS*
            (princ (strcat "\n  " (itoa i) ". " (cdr pair)))
            (setq i (1+ i))
          )
          (princ "\n===========================================")
          (setq code nil)
          (setq choice (getint "\nНомер подрядчика или Enter: "))
          (if (and choice (>= choice 1) (<= choice (length *CONTRACTORS*)))
            (setq code (car (nth (1- choice) *CONTRACTORS*)))
            (if choice (princ "\nНеверный номер подрядчика."))
          )
          (princ "\n\n===== СТАТУС (Enter = не менять) =====")
          (princ "\n  1. НЕ СМОНТИРОВАНО — серая штриховка")
          (princ "\n  2. СМОНТИРОВАНО — без штриховки")
          (princ "\n  3. ДЕФЕКТ ПОД ЗАМЕНУ — красная штриховка")
          (princ "\n  4. ДЕФЕКТ С ВОЗМОЖНОСТЬЮ РЕМОНТА — жёлтая штриховка")
          (princ "\n=========================================")
          (setq status nil)
          (setq status-choice (getint "\nНомер статуса или Enter: "))
          (if (and status-choice (>= status-choice 1) (<= status-choice (length *STATUSES*)))
            (setq status (nth (1- status-choice) *STATUSES*))
            (if status-choice (princ "\nНеверный номер статуса."))
          )
          (if (and (not code) (not status))
            (princ "\nНичего не выбрано для изменения.")
            (progn
              (setq name (if code (get-contractor-name code) nil)
                    hatch-type  (if status (cdr (assoc status *STATUS-HATCH*)) nil)
                    hatch-color (if status (cdr (assoc status *STATUS-HATCH-COLOR*)) nil)
                    today (get-today-string)
                    user  (get-current-user)
                    count 0)
              (setq i 0)
              (repeat (sslength ss)
                (setq ent (ssname ss i))
                (setq xd (XDATA-GET ent))
                (if xd
                  (progn
                    (setq newdata '())
                    (foreach pair xd
                      (cond
                        ((and code (= (car pair) "CONTRACTOR"))
                         (setq newdata (append newdata (list (cons "CONTRACTOR" name)))))
                        ((and status (= (car pair) "STATUS"))
                         (setq newdata (append newdata (list (cons "STATUS" status)))))
                        ((= (car pair) "DATE_CHANGED")
                         (setq newdata (append newdata (list (cons "DATE_CHANGED" today)))))
                        ((= (car pair) "CHANGED_BY")
                         (setq newdata (append newdata (list (cons "CHANGED_BY" user)))))
                        (t
                         (setq newdata (append newdata (list pair))))
                      )
                    )
                    (XDATA-SET ent newdata)
                  )
                )
                (if status
                  (hatch-block ent hatch-type hatch-color)
                )
                (setq i (1+ i) count (1+ count))
              )
              (princ (strcat "\n\nОбработано панелей: " (itoa count)))
              (if name (princ (strcat "\nПодрядчик: " name)))
              (if status (princ (strcat "\nСтатус: " status)))
            )
          )
        )
      )
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: ВЫДЕЛИТЬ_ПО
;;; ------------------------------------------------------------
(defun C:ВЫДЕЛИТЬ_ПО ( / filter-type filter-value filter-field ss i ent xd val match-list choice)

  (if (not (init-project))
    (princ "\nИнициализация не выполнена.")
    (progn
      (princ "\n\n===== ВЫДЕЛИТЬ ПО =====")
      (princ "\n  1. Статусу")
      (princ "\n  2. Подрядчику")
      (princ "\n  3. Очереди")
      (princ "\n========================")
      (initget "1 2 3")
      (setq filter-type (getkword "\nВыберите критерий [1/2/3]: "))
      (if (not filter-type)
        (princ "\nОтменено.")
        (progn
          (cond
            ((= filter-type "1")
             (princ "\n\n===== СТАТУСЫ =====")
             (princ "\n  1. НЕ СМОНТИРОВАНО")
             (princ "\n  2. СМОНТИРОВАНО")
             (princ "\n  3. ДЕФЕКТ ПОД ЗАМЕНУ")
             (princ "\n  4. ДЕФЕКТ С ВОЗМОЖНОСТЬЮ РЕМОНТА")
             (princ "\n====================")
             (initget "1 2 3 4")
             (setq choice (getkword "\nНомер статуса: "))
             (if choice (setq filter-value (nth (1- (atoi choice)) *STATUSES*)))
             (setq filter-field "STATUS"))
            ((= filter-type "2")
             (princ "\n\n===== ПОДРЯДЧИКИ =====")
             (setq i 1)
             (foreach pair *CONTRACTORS*
               (princ (strcat "\n  " (itoa i) ". " (cdr pair)))
               (setq i (1+ i)))
             (princ "\n========================")
             (setq choice (getint "\nНомер подрядчика: "))
             (if (and choice (>= choice 1) (<= choice (length *CONTRACTORS*)))
               (setq filter-value (cdr (nth (1- choice) *CONTRACTORS*)))
               (if choice (princ "\nНеверный номер."))
             )
             (setq filter-field "CONTRACTOR"))
            ((= filter-type "3")
             (setq filter-value (getstring "\nНомер очереди: "))
             (setq filter-field "QUEUE")))
          (if (or (not filter-value) (= filter-value ""))
            (princ "\nЗначение не задано.")
            (progn
              (princ (strcat "\nПоиск панелей с " filter-field " = \"" filter-value "\"..."))
              (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 8 *PANEL-LAYER*))))
              (setq match-list '())
              (if ss
                (progn
                  (setq i 0)
                  (repeat (sslength ss)
                    (setq ent (ssname ss i))
                    (setq val (XDATA-GET-FIELD ent filter-field))
                    (if (and val (= val filter-value))
                      (setq match-list (append match-list (list ent))))
                    (setq i (1+ i)))))
              (if (not match-list)
                (princ "\nНе найдено ни одной панели.")
                (progn
                  (sssetfirst nil (list->ss match-list))
                  (princ (strcat "\nВыделено панелей: " (itoa (length match-list))))
                  (princ "\nПримените СМЕНИТЬ_СТАТУС, НАЗНАЧИТЬ_ПОДРЯДЧИКА или НАЗНАЧИТЬ_ВСЁ.")))))))))
  (princ)
)


;;; ------------------------------------------------------------
;;;  ВСПОМОГАТЕЛЬНАЯ: список -> ss
;;; ------------------------------------------------------------
(defun list->ss (lst / ss i)
  (setq ss (ssadd) i 0)
  (repeat (length lst)
    (ssadd (nth i lst) ss)
    (setq i (1+ i)))
  ss
)

;;; ------------------------------------------------------------
;;;  КОМАНДА: ВЫРЕЗ
;;;  Рисует полилинию выреза на слое ВЫРЕЗЫ, работает пока не нажат Enter
;;; ------------------------------------------------------------
(defun C:ВЫРЕЗ ( / old-layer)

  ;; Создаём слой ВЫРЕЗЫ, если его нет
  (if (not (tblsearch "LAYER" "ВЫРЕЗЫ"))
    (command "_.LAYER" "_N" "ВЫРЕЗЫ" "_C" "1" "ВЫРЕЗЫ" "")
  )

  ;; Сохраняем текущий слой
  (setq old-layer (getvar "CLAYER"))

  ;; Переключаемся на слой ВЫРЕЗЫ
  (setvar "CLAYER" "ВЫРЕЗЫ")

  (princ "\nОбводите вырезы (Enter — завершить):")

  ;; Цикл: рисуем полилинию, пока пользователь не нажмёт Enter на пустом месте
  (setq done nil)
  (while (not done)
    (setq pt (getpoint "\nНачальная точка выреза (Enter — выход): "))
    (if (not pt)
      (setq done T)  ;; Enter = выход
      (progn
        ;; Запускаем полилинию
        (command "_.PLINE" pt)

        ;; Ждём завершения команды
        (while (= (getvar "CMDACTIVE") 1)
          (command pause)
        )

        ;; Перемещаем созданную полилинию на слой ВЫРЕЗЫ
        (command "_.CHPROP" "_L" "" "_LA" "ВЫРЕЗЫ" "")

        (princ "\nСледующий вырез (Enter — выход): ")
      )
    )
  )

  ;; Возвращаем слой
  (setvar "CLAYER" old-layer)

  (princ "\nВырезы обведены. СУММА_ВЫРЕЗОВ — показать сумму.")
  (princ)
)

;;; ------------------------------------------------------------
;;;  ВСПОМОГАТЕЛЬНАЯ: группировка одинаковых площадей (через строки)
;;; ------------------------------------------------------------
(defun group-similar-areas (area-list / groups area found new-groups pair str-area str-car)
  (setq groups '())
  (foreach area area-list
    (setq found nil
          new-groups '()
          str-area (rtos area 2 3))
    (foreach pair groups
      (setq str-car (rtos (car pair) 2 3))
      (if (= str-car str-area)
        (progn
          (setq new-groups (append new-groups (list (cons (car pair) (1+ (cdr pair))))))
          (setq found T)
        )
        (setq new-groups (append new-groups (list pair)))
      )
    )
    (if (not found)
      (setq new-groups (append new-groups (list (cons area 1))))
    )
    (setq groups new-groups)
  )
  (vl-sort groups '(lambda (a b) (> (car a) (car b))))
)

;;; ------------------------------------------------------------
;;;  КОМАНДА: СУММА_ВЫРЕЗОВ
;;; ------------------------------------------------------------
(defun C:СУММА_ВЫРЕЗОВ ( / ss i total-area total-perim count ent area perim area-list perim-list area-groups perim-groups)

  (princ "\nПодсчёт вырезов...")

  (setq ss (ssget "_X" (list (cons 0 "LWPOLYLINE") (cons 8 "ВЫРЕЗЫ"))))

  (if (not ss)
    (princ "\nВырезы не найдены. Обведите их командой ВЫРЕЗ.")
    (progn
      (setq total-area 0.0 total-perim 0.0 count 0 i 0 area-list '() perim-list '())
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq area (/ (vla-get-Area (vlax-ename->vla-object ent)) 1000000.0))
        (setq perim (/ (vla-get-Length (vlax-ename->vla-object ent)) 1000.0))
        (setq area-list (append area-list (list area)))
        (setq perim-list (append perim-list (list perim)))
        (setq total-area (+ total-area area))
        (setq total-perim (+ total-perim perim))
        (setq count (1+ count))
        (setq i (1+ i))
      )

      (setq area-groups (group-similar-areas area-list))
      (setq perim-groups (group-similar-areas perim-list))

      (princ "\n==============================================")
      (princ "\n  СУММА ВЫРЕЗОВ")
      (princ (strcat "\n  Количество: " (itoa count) " шт."))
      (princ "\n==============================================")
      
      ;; Площади
      (princ "\n  ПЛОЩАДИ (сгруппированы):")
      (princ "\n    ")
      (setq first T)
      (foreach g area-groups
        (if first (setq first nil) (princ " + "))
        (if (= (cdr g) 1)
          (princ (rtos (car g) 2 4))
          (princ (strcat (itoa (cdr g)) "×" (rtos (car g) 2 4)))
        )
      )
      (princ (strcat " = " (rtos total-area 2 4) " м²"))

      ;; Периметры
      (princ "\n\n  ПЕРИМЕТРЫ (сгруппированы):")
      (princ "\n    ")
      (setq first T)
      (foreach g perim-groups
        (if first (setq first nil) (princ " + "))
        (if (= (cdr g) 1)
          (princ (rtos (car g) 2 3))
          (princ (strcat (itoa (cdr g)) "×" (rtos (car g) 2 3)))
        )
      )
      (princ (strcat " = " (rtos total-perim 2 3) " м"))

      (princ "\n==============================================")
    )
  )
  (princ)
)


;;; ------------------------------------------------------------
;;;  КОМАНДА: СУММА_ВЫРЕЗОВ_ПО_ВЫДЕЛЕНИЮ
;;; ------------------------------------------------------------
(defun C:СУММА_ВЫРЕЗОВ_ПО_ВЫДЕЛЕНИЮ ( / ss i total-area total-perim count ent area perim area-list perim-list area-groups perim-groups)

  (princ "\nВыберите полилинии вырезов рамкой...")
  (setq ss (ssget (list (cons 0 "LWPOLYLINE") (cons 8 "ВЫРЕЗЫ"))))

  (if (not ss)
    (princ "\nНичего не выбрано или выбранные полилинии не на слое ВЫРЕЗЫ.")
    (progn
      (setq total-area 0.0 total-perim 0.0 count 0 i 0 area-list '() perim-list '())
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq area (/ (vla-get-Area (vlax-ename->vla-object ent)) 1000000.0))
        (setq perim (/ (vla-get-Length (vlax-ename->vla-object ent)) 1000.0))
        (setq area-list (append area-list (list area)))
        (setq perim-list (append perim-list (list perim)))
        (setq total-area (+ total-area area))
        (setq total-perim (+ total-perim perim))
        (setq count (1+ count))
        (setq i (1+ i))
      )

      (setq area-groups (group-similar-areas area-list))
      (setq perim-groups (group-similar-areas perim-list))

      (princ "\n==============================================")
      (princ "\n  СУММА ВЫРЕЗОВ — выделенные")
      (princ (strcat "\n  Количество: " (itoa count) " шт."))
      (princ "\n==============================================")
      
      (princ "\n  ПЛОЩАДИ (сгруппированы):")
      (princ "\n    ")
      (setq first T)
      (foreach g area-groups
        (if first (setq first nil) (princ " + "))
        (if (= (cdr g) 1)
          (princ (rtos (car g) 2 4))
          (princ (strcat (itoa (cdr g)) "×" (rtos (car g) 2 4)))
        )
      )
      (princ (strcat " = " (rtos total-area 2 4) " м²"))

      (princ "\n\n  ПЕРИМЕТРЫ (сгруппированы):")
      (princ "\n    ")
      (setq first T)
      (foreach g perim-groups
        (if first (setq first nil) (princ " + "))
        (if (= (cdr g) 1)
          (princ (rtos (car g) 2 3))
          (princ (strcat (itoa (cdr g)) "×" (rtos (car g) 2 3)))
        )
      )
      (princ (strcat " = " (rtos total-perim 2 3) " м"))

      (princ "\n==============================================")
    )
  )
  (princ)
)

(princ "\nМодуль назначения v2.4 загружен. Команды: ВЗЯТЬ_В_РАБОТУ, НАЗНАЧИТЬ_ПОДРЯДЧИКА, СМЕНИТЬ_СТАТУС, СМЕНИТЬ_ОЧЕРЕДЬ, ДОБАВИТЬ_РЕЗ, ОЧИСТИТЬ_ШТРИХОВКИ, НАЗНАЧИТЬ_ВСЁ, ВЫДЕЛИТЬ_ПО")
(princ)