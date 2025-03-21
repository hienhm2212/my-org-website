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
;; TODO for github pages
(defvar lf/base-url "https://hienhm2212.github.io/my-org-website" "Base URL for GitHub Pages.")

(defun lf/header ()
  "Return custom HTML header using XML"
  (sxml-to-xml
   '(header
     (h1 "Welcome to Little Fox")
     (nav (@ (class "nav"))
          (a (@ (class "nav-link") (href "/my-org-website/")) "Home") " "
          (a (@ (class "nav-link") (href "/my-org-website/blogs")) "Blogs") " "
          (a (@ (class "nav-link") (href "/my-org-website/contact")) "Contact")
          ))))

(defun lf/footer ()
  "Return custom HTML footer using XML"
  (sxml-to-xml
   '(footer
     (p "© 2025 Little Fox. All rights reserved."))))

(cl-defun lf/generate-page (title
                            content
                            info
                            &key
                            (publish-date)
                            (head-extra)
                            (pre-content)
                            (exclude-header)
                            (exclude-footer))
  "Generate a complete HTML page with optional header, footer and metadata."
  (concat
   "<!-- Generated on " (format-time-string "%Y-%m-%d @ %H:%M") "with Emacs Org Mode -->\n"
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
           ;; Head section
           (head
            (meta (@ (charset "utf-8")))
            (meta (@ (author "HienHM")))
            (meta (@ (name "viewport")
                     (content "width=device-width, initial-scale=1")))
            (link (@ (rel "icon") (type "image/png") (href "/images/fox.png")))
            (title ,(concat title " - Little Fox")))
           ;; Body section
           (body
            ,@(unless exclude-header
                `(,(lf/header)))
            (div (@ (class "container"))
                 (div (@ (class "site-post"))
                      (h1 (@ (class "site-post-title")) ,title)
                      ,(when publish-date
                         `(p (@ (class "site-post-meta")) ,publish-date))
                      ,(when pre-content pre-content)
                      (div (@ (id "content")) ,content)))
            ,@(unless exclude-footer
                `(,(lf/footer))))))))

(defun lf/org-html-template (contents backend info)
  "Use custom HTML generation with contents, backend, and info."
  (ignore backend) ;; Backend is not used in this function
  (lf/generate-page (org-export-data (plist-get info :title) info)
                    contents
                    info
                    :publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)))

(setq org-html-preamble nil
      org-html-postamble nil
      org-export-filter-final-output-functions '(lf/org-html-template))

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
