(define-module (local nerdfont)
	       #:use-module (guix utils)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix download)
	       #:use-module (guix build-system font))

(define-public font-fira-code-nerdfont
	       (package
		 (name "font-fira-code-nerdfont")
		 (version "2.1.0")
		 (source
		   (origin
		     (method url-fetch/zipbomb)
		     (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
					 version "/FiraCode.zip"))
		     (sha256
		       (base32 "1rx7zrkq0584k6hrh6dx30xrnz5nq379xyw73pfd4gxaxnh9mpi1"))))
		 (arguments
		   '(#:phases
		     (modify-phases %standard-phases
				    (add-after 'unpack 'delete-unused-fonts
					       (lambda _ 
						 (for-each delete-file (find-files (getcwd) "(Fura|Windows)"))
						 #t))
				    (add-after 'delete-unused-fonts 'rename-fonts
					       (lambda _ 
						 (for-each 
						   (lambda name 
						     (rename-file (car name) (string-concatenate 
									 (string-split (car name) #\ ))))
						   (find-files "."))
						 #t)))))
		 (build-system font-build-system)
		 (home-page "https://mozilla.github.io/Fira/")
		 (synopsis "Monospaced font with programming ligatures")
		 (description
		   "Fira Code is an extension of the Fira Mono font containing a set of ligatures
		   for common programming multi-character combinations.  This is just a font rendering
		   feature: underlying code remains ASCII-compatible.  This helps to read and understand
		   code faster.  For some frequent sequences like .. or //, ligatures allow us to
		   correct spacing.")
		   (license license:silofl1.1)))
