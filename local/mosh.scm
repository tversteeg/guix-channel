(define-module (local mosh)
	       #:use-module (gnu packages)
	       #:use-module (gnu packages autotools)
	       #:use-module (gnu packages base)
	       #:use-module (gnu packages boost)
	       #:use-module (gnu packages compression)
	       #:use-module (gnu packages crypto)
	       #:use-module (gnu packages elf)
	       #:use-module (gnu packages gnupg)
	       #:use-module (gnu packages gperf)
	       #:use-module (gnu packages groff)
	       #:use-module (gnu packages guile)
	       #:use-module (gnu packages hurd)
	       #:use-module (gnu packages libedit)
	       #:use-module (gnu packages linux)
	       #:use-module (gnu packages logging)
	       #:use-module (gnu packages m4)
	       #:use-module (gnu packages multiprecision)
	       #:use-module (gnu packages ncurses)
	       #:use-module (gnu packages nettle)
	       #:use-module (gnu packages kerberos)
	       #:use-module (gnu packages perl)
	       #:use-module (gnu packages pkg-config)
	       #:use-module (gnu packages popt)
	       #:use-module (gnu packages protobuf)
	       #:use-module (gnu packages python)
	       #:use-module (gnu packages python-crypto)
	       #:use-module (gnu packages python-web)
	       #:use-module (gnu packages python-xyz)
	       #:use-module (gnu packages readline)
	       #:use-module (gnu packages texinfo)
	       #:use-module (gnu packages tls)
	       #:use-module (gnu packages xorg)
	       #:use-module (guix build-system cmake)
	       #:use-module (guix build-system gnu)
	       #:use-module (guix build-system python)
	       #:use-module (guix download)
	       #:use-module (guix git-download)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix utils)
	       #:use-module (srfi srfi-1))

(define-public mosh
	       (let ((commit "03087e7a761df300c2d8cd6e072890f8e1059dfa"))
		 (package
		   (name "mosh")
		   (version (git-version "1.3.2" "0" commit))
		   (source (origin
			     (method git-fetch)
			     (uri (git-reference
				    (url "https://github.com/mobile-shell/mosh.git")
				    (commit commit)))
			     (file-name (git-file-name name version))
			     (sha256
			       (base32 "170m3q9sxw6nh8fvrf1l0hbx0rjjz5f5lzhd41143kd1rps3liw8"))))
		   (build-system gnu-build-system)
		   (arguments
		     '(#:phases
		       (modify-phases %standard-phases
				      ;      (add-after 'unpack 'patch-FHS-file-names
				      ;		 (lambda _
				      ;		   (substitute* "scripts/mosh.pl"
				      ;				(("/bin/sh")
				      ;				 (which "sh")))
				      ;		   #t))
				      (add-after 'install 'wrap
						 (lambda* (#:key outputs #:allow-other-keys)
							  ;; Make sure 'mosh' can find 'mosh-client' and
							  ;; 'mosh-server'.
							  (let* ((out (assoc-ref outputs "out"))
								 (bin (string-append out "/bin")))
							    (wrap-program (string-append bin "/mosh")
									  `("PATH" ":" prefix (,bin)))))))))
		   (native-inputs
		     `(("pkg-config" ,pkg-config)
		       ("autoconf" ,autoconf)
		       ("automake" ,automake)))
		   (inputs
		     `(("openssl" ,openssl)
		       ("perl" ,perl)
		       ("perl-io-tty" ,perl-io-tty)
		       ("zlib" ,zlib)
		       ("ncurses" ,ncurses)
		       ("protobuf" ,protobuf)
		       ("boost-headers" ,boost)))
		   (home-page "https://mosh.org/")
		   (synopsis "Remote shell tolerant to intermittent connectivity")
		   (description
		     "Mosh is a remote terminal application that allows client roaming, supports
		     intermittent connectivity, and provides intelligent local echo and line editing
		     of user keystrokes.  It's a replacement for SSH that's more robust and
		     responsive, especially over Wi-Fi, cellular, and long-distance links.")
		     (license license:gpl3+))))
