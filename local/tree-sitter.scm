(define-module (local tree-sitter)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix utils)
	       #:use-module (guix git-download)
	       #:use-module (guix build-system gnu)
	       #:use-module (gnu packages node))

(define-public tree-sitter
	       (package
		 (name "tree-sitter")
		 (version "0.19.5")
		 (source
		   (origin
		     (method git-fetch)
		     (uri (git-reference
			    (url "https://github.com/tree-sitter/tree-sitter")
			    (commit (string-append "v" version))))
		     (file-name (git-file-name name version))
		     (sha256
		       (base32 "1qmb0sva28zv6r3c3j7xs9pc8bpwwhkb9vxxndw2zbdn9wkvmbmn"))))
		 (build-system gnu-build-system)
		 (arguments
		   `(#:make-flags (list (string-append "CC=" ,(cc-for-target)))
		     #:phases (modify-phases %standard-phases
					     (delete 'configure)
					     (delete 'check)
					     (add-before 'build 'set-env
							 (lambda* (#:key inputs outputs #:allow-other-keys)
							   (setenv "PREFIX" (assoc-ref outputs "out"))
							   #t)))))
		 (native-inputs
		   `(("node" ,node)))
		 (synopsis "TODO")
		 (description
		   "TODO")
		 (home-page "https://tree-sitter.github.io")
		 (license license:expat)))
