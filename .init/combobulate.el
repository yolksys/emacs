(use-package combobulate
   :custom
   ;; You can customize Combobulate's key prefix here.
   ;; Note that you may have to restart Emacs for this to take effect!
   (combobulate-key-prefix "C-c o")
   :hook (
	  (go-ts-mode . combobulate-mode)
	  (typescript-ts-mode . combobulate-mode)
	  (html-mode . combobulate-mode)
	 )
   :load-path ("~/.emacs.d/packages/combobulate")
)
