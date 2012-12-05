;;; configure-package.el --- Pretty package configuration for Emacs

;; Copyright (C) 2012 Randy Morris <randy.morris@archlinux.us>

;; Author: Randy Morris <randy.morris@archlinux.us>
;; Version: 0.0.1
;; Keywords: configuration, packages, extensions
;; Package-Requires: 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; # configure-package.el
;;
;; Comments here

;;; Code:

(defun cp--possible-paths (package)
  "Returns a list of absolute paths built from `load-path',
`load-suffixes', and the string PACKAGE."
  (let (all
	(suffixes (append load-suffixes '(""))))
    (dolist (path load-path)
      (dolist (suffix suffixes)
	(let* ((filename (concat package suffix))
	       (full-path (expand-file-name filename path)))
	  (setq all (append all `(,full-path))))))
    all))

(defun cp--package-installed-p (feature)
  "Return t if FEATURE can be required without triggering a file-error.

Follows the same logic as `load' to determine whether or not
FEATURE is available."
  (let ((files (cp--possible-paths (symbol-name feature))))
    (member t (mapcar '(lambda (file) (file-exists-p file)) files))))

(defun cp--get-missing-requirements (requirements)
  "Return a list of requirements that are not yet installed."
  (delq nil (mapcar #'(lambda (req)
			(if (not (cp--package-installed-p req))
			    req)) requirements)))

(defun cp--log (msg &rest args)
  (message "configure-package: %s" (apply 'format msg args)))

(defvar configured-packages nil
  "A list of packages that have been configured with
  `configure-package'.")

(defmacro configure-package (package &rest args)
  "Dirty macro to make configuration a little prettier.

PACKAGE is the name of a package to configure.
ARGS is a plist which optionally contains:

  DOCSTRING  The first element of the plist is an optional docstring.
             This docstring is completely ignored by this macro.

  :init      A form to eval at the time `configure-package' is run.

  :after     A form to eval after PACKAGE is eventually loaded.

  :bind      A list of conses in the form (\"KEYS\" . symbol) which
             will result in `autoload's being created for the
             symbols and global mappings being created to these
             autoloaded functions.

  :requires  A list of packages that this package depends on.  If
             a package is in this list and the package appears as
             though it is not installed, `configure-package' will
             only add PACKAGE to the `configured-packages'
             variable.  No forms will be evaluated.

This is in a similar vein as github.com/jweigley/use-package but
extremely less featureful and written only to suit my needs."
  (declare (indent defun))
  (when (eql 'string (type-of (car args)))
    (pop args))
  (let* ((installed (cp--package-installed-p package))
	 (init-form (plist-get args :init))
	 (after-form (plist-get args :after))
	 (autoloads (plist-get args :autoload))
	 (binds (plist-get args :bind))
	 (requires (plist-get args :require))
	 (missing-reqs (cp--get-missing-requirements requires)))
    (add-to-list 'configured-packages  package)
    (if missing-reqs
	(progn 
	  (mapc #'(lambda (req)
		    (cp--log "Missing requirement `%s' for `%s'" req package))
		missing-reqs)
	  nil)
      (if installed
	`(progn
	   ,(when init-form init-form)
	   ,(when after-form `(eval-after-load (quote ,package) (quote ,after-form)))
	   ,@(when autoloads
	       (mapcar #'(lambda (fn) 
			   `(autoload (quote ,fn) ,(symbol-name package) "" t))
		       autoloads))
	   ,@(when binds
	       (mapcar #'(lambda (keys) 
			   `(progn
			      (autoload (quote ,(cdr keys)) ,(symbol-name package) "" t)
			      (global-set-key (kbd ,(car keys)) (quote ,(cdr keys)))))
		       binds)))
	(cp--log "Package `%s' not installed" package)))))

(defun install-configured-packages ()
  "Instal all packages that have been configured with `configure-package'."
  (interactive)
  (package-refresh-contents)
  (dolist (package configured-packages)
    (condition-case nil
	;; package-install throws an error if a package isn't available.
	;; I don't care if it's not available right now.
	(package-install package)
      ((error) nil))))

(provide 'configure-package)
;;; configure-package.el ends here
