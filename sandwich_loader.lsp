;;; ============================================================
;;; ЕДИНЫЙ ЗАГРУЗЧИК ПЛАГИНА СЭНДВИЧ-ПАНЕЛИ
;;; ============================================================

(defun load-module (filename / result)
  (setq result (findfile filename))
  (if result
    (progn
      (load result)
      (princ (strcat "\n[OK] " filename))
      T
    )
    (progn
      (princ (strcat "\n[НЕ НАЙДЕН] " filename))
      nil
    )
  )
)

(load-module "xdata.lsp")
(load-module "assign.lsp")
(load-module "reports.lsp")
(load-module "table.lsp")
(load-module "joints.lsp")
(load-module "excel.lsp")

(princ "\n========================================")
(princ "\nПлагин СЭНДВИЧ-ПАНЕЛИ загружен.")
(princ "\nПервая команда: ВЗЯТЬ_В_РАБОТУ")
(princ "\n========================================")
(princ)