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
(package-install 'esxml)

;; Load the publishing system
(require 'ox-publish)

(use-package htmlize
  :ensure t)
(use-package esxml
  :ensure t)


;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      ;; org-html-head-include-default-stype t   ;; current use emacs theme
      org-html-doctype "html5"
      org-html-html5-fancy t)

;; Define functions
(defvar lf/base-url "" "Base URL for GitHub Pages.")
;; (defvar lf/base-url "https://hienhm2212.github.io/my-org-website" "Base URL for GitHub Pages.")

(defun lf/header ()
  "Return custom HTML header using XML"
  (sxml-to-xml
   `(header
     (h1 "Welcome to Little Fox")
     (nav (@ (class "nav"))
          (a (@ (class "nav-link") (href ,(concat lf/base-url "/"))) "Home") " "
          (a (@ (class "nav-link") (href ,(concat lf/base-url "/blogs"))) "Blogs") " "
          (a (@ (class "nav-link") (href ,(concat lf/base-url "/contact"))) "Contact")))))

(defun lf/footer ()
  "Return custom HTML footer using XML"
  (sxml-to-xml
   '(footer
     (p "© 2025 Little Fox. All rights reserved."))))

;; Define a proper HTML template function
(defun lf/html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.
INFO is a plist holding export options."
  (let* ((title (org-export-data (plist-get info :title) info))
         (publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)))
    (concat
     "<!DOCTYPE html>"
     (sxml-to-xml
      `(html (@ (lang "en"))
             (head
              (meta (@ (charset "utf-8")))
              (meta (@ (name "viewport")
                       (content "width=device-width, initial-scale=1")))
              (meta (@ (author "HienHM")))
              ;; (link (@ (rel "stylesheet")
              ;;          (href "https://cdn.simplecss.org/simple.min.css")))
              (link (@ (rel "stylesheet") (href ,(concat lf/base-url "/css/code.css"))))
              (link (@ (rel "stylesheet") (href ,(concat lf/base-url "/css/site.css"))))
              (link (@ (rel "icon") (type "image/png")
                       (href ,(concat lf/base-url "/images/fox.png"))))
              (title ,(concat title " - Little Fox")))
             (body
              ,(lf/header)
              (main (@ (class "container"))
                    (article (@ (class "site-post"))
                             (h1 (@ (class "site-post-title")) ,title)
                             ,(when publish-date
                                `(p (@ (class "site-post-meta")) ,publish-date))
                             (div (@ (id "content")) ,contents)))
              ,(lf/footer)))))))

;; Override the default HTML template
(setq org-html-template-function 'lf/html-template)

;; Very simple sitemap function that works reliably
(defun lf/sitemap-format-entry (entry style project)
  "Format ENTRY for the sitemap.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (format "* [[file:%s][%s]]\nPublished: %s\n"
          (file-name-nondirectory entry)
          (org-publish-find-title entry project)
          (format-time-string "%Y-%m-%d"
                              (org-publish-find-date entry project))))

(setq org-publish-project-alist
      (list '("littlefox:main"
              :base-directory "./content"
              :base-extension "org"
              :exclude "blogs.org\\|blogs/.*\\.org"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-toc nil           ;; Disable TOC
              :section-numbers nil    ;; Disable section numbers
              :with-author nil
              :with-creator nil
              :time-stamp-file nil)

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
              :sitemap-filename "index.org"
              :sitemap-sort-files anti-chronologically
              :sitemap-format-entry lf/sitemap-format-entry
              :with-toc nil           ;; Disable TOC
              :section-numbers nil    ;; Disable section numbers
              :with-author nil
              :with-creator nil
              :time-stamp-file nil)

            '("littlefox"
              :components ("littlefox:main" "littlefox:assets" "littlefox:blogs"))))

;; Generate the site output
(org-publish-all t)

(message "Build complete!")

(provide 'publish)
;;; publish.el ends here
