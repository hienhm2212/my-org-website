;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)

;; Load the publishing system
(require 'ox-publish)
(require 'htmlize)

(use-package esxml
  :ensure t)

;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")
;; Define functions
(defun lf/header ()
  "Return custom HTML header using XML"
  (sxml-to-xml
   '(header
     (h1 "Welcome to Little Fox")
     (nav
      (a (@ (href "/")) "Home") " "
      (a (@ (href "/blogs")) "Blogs") " "
      (a (@ (href "/contact")) "Contact")))))

(defun lf/footer ()
  "Return custom HTML footer using XML"
  (sxml-to-xml
   '(footer
     (p "© 2025 Little Fox. All rights reserved."))))

(defun lf/org-html-template(contents info)
  "Add header and footer to contents with info."
  (concat (lf/header) contents (lf/footer)))

(setq org-html-preamble (lambda (_)
                          (lf/header))
      org-html-postamble (lambda (_)
                           (lf/footer)))

;; Define the publishing project
(defun lf/blogs-sitemap (title files)
  (format "#+title: %s\n\n%s"
          title
          (mapconcat (lambda (file)
                       (format "- %s\n" file))
                     (cadr files)
                     "\n")))

(setq org-publish-project-alist
      (list '("littlefox:main"
              :base-directory "./content"
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)
            '("littlefox:assets"
              :base-directory "./assets"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf"
              :publishing-directory "./public"
              :recursive t
              :publishing-function org-publish-attachment)
            '("littlefox:blogs"
              :base-directory "./content/blogs"
              :base-extension "org"
              :publishing-directory "./public/blogs"
              :publishing-function org-html-publish-to-html
              :auto-sitemap t
              :sitemap-title "Little Fox Blogs"
              :sitemap-filename "../blogs.org"
              :sitemap-function lf/blogs-sitemap)
            ))
;; Generate the site output
(org-publish-all t)

(message "Build complete!")

(provide 'publish)
;;; publish.el ends here
