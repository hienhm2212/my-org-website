;;; Code:

;; Initialize package sources
(require 'package)

;; Set package installation directory so that packages are't stored in the
;; ~/.emacs.d/elpa path.
(setq package-user-dir (expand-file-name "./.packages"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Require built-in dependencies
(require 'vc-git)
(require 'ox-publish)
(require 'subr-x)
(require 'cl-lib)

;; Install other packages
(use-package esxml
  :pin "melpa-stable"
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package webfeeder
  :ensure t)

(setq user-full-name "Hien Huynh-Minh")
(setq user-mail-address "blackcat22121996@gmail.com")


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
              :publishing-function org-publish-attachment
              )
            '("littlefox:blogs"
              :base-directory "./content/blogs"
              :base-extension "org"
              :publishing-directory "./public/blogs"
              :publishing-function org-html-publish-to-html
              :auto-sitemap t
              :sitemap-filename "../blogs.org"
              :sitemap-title "Little Fox Blogs"
              :sitemap-format-entry lf/format-blogs-entry
              :sitemap-style list
              :sitemap-function lf/blogs-sitemap
              :sitemap-sort-files anti-chronologically
              :with-tile nil
              )

            ))


;; Functions
(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 ;; The 404 page is a special case, it must be name "404.html"
                 (concat article-path
                         (if (string= (file-name-nondirectory filename) "404.org") "404" "index")
                         extension))))
      (org-publish-org-to 'site-html
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))


(defun get-article-output-path (org-file pub-dir)
  "Generate the output directory path for given org file."
  (let ((article-dir (concat pub-dir
                             (downcase
                              (file-name-as-directory
                               (file-name-sans-extension
                                (file-name-nondirectory org-file)))))))

    (if (string-match "\\/index.org\\|\\/404.org$" org-file)
        pub-dir
      (progn
        (unless (file-directory-p article-dir)
          (make-directory article-dir t))
        article-dir))))

;;; publish.el ends here
