(define-module (gnu packages vim)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix utils)
	       #:use-module (guix download)
	       #:use-module (guix git-download)
	       #:use-module (guix build-system copy))

;; There are no release tarballs.
(define-public vim-rust
	       (let ((commit "87c745d8d506fc1eecc1d81df15d5bde1658a2fc")
		     (revision "0"))
		 (package
		   (name "vim-rust")
		   (version (string-append "0.0.0-" revision "." (string-take commit 7)))
		   (source
		     (origin
		       (method git-fetch)
		       (uri (git-reference
			      (url "https://github.com/rust-lang/rust.vim")
			      (commit commit)))
		       (file-name (string-append name "-" version "-checkout"))
		       (sha256
			 (base32 "0v0ip731lclh9aqrmlqwnnz4skgawaq3invghh1c7lh0zdq22lzb"))))
		   (build-system copy-build-system)
		   (arguments
		     '(#:install-plan
		       '(("after" "share/vim/vimfiles/")
			 ("autoload" "share/vim/vimfiles/")
			 ("compiler" "share/vim/vimfiles/")
			 ("ctags" "share/vim/vimfiles/")
			 ("doc" "share/vim/vimfiles/")
			 ("ftdetect" "share/vim/vimfiles/")
			 ("ftplugin" "share/vim/vimfiles/")
			 ("indent" "share/vim/vimfiles/")
			 ("plugin" "share/vim/vimfiles/")
			 ("syntax" "share/vim/vimfiles/")
			 ("syntax_checkers" "share/vim/vimfiles/"))))
		   (synopsis "Vim configuration for Rust")
		   (description
		     "Rust.vim is a Vim plugin that provides Rust file detection, syntax highlighting, formatting, Syntastic integration, and more.")
		   (home-page "https://github.com/rust-lang/rust.vim")
		   (license license:asl2.0))))

(define-public neovim-rust
	       (package
		 (inherit vim-rust)
		 (name "neovim-rust")
		 (arguments
		   '(#:install-plan
		     '(("after" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("autoload" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("compiler" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("ctags" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("doc" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("ftdetect" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("ftplugin" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("indent" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("plugin" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("syntax" "share/nvim/site/pack/guix/start/rust.vim/")
		       ("syntax_checkers" "share/nvim/site/pack/guix/start/rust.vim/"))))
		 (synopsis "Neovim configuration for Rust")
		 (description
		   "Rust.vim is a Neovim plugin that provides Rust file detection, syntax highlighting, formatting, Syntastic integration, and more.")))

(define-public vim-numbertoggle
	       (package
		 (name "vim-numbertoggle")
		 (version "2.1.2")
		 (source
		   (origin
		     (method git-fetch)
		     (uri (git-reference
			    (url "https://github.com/jeffkreeftmeijer/vim-numbertoggle")
			    (commit version)))
		     (file-name (git-file-name name version))
		     (sha256
		       (base32 "02zgzkwv2fk4zyg6agvski054nwkrm1m9dw0jpva57ksbf8rvqrg"))))
		 (build-system copy-build-system)
		 (arguments
		   '(#:install-plan
		     '(("doc" "share/vim/vimfiles/")
		       ("plugin" "share/vim/vimfiles/"))))
		 (synopsis "Toggles between hybrid and absolute line numbers automatically in Vim")
		 (description
		   "Numbertoggle is a Vim plugin that in a buffer with 'hybrid' line numbers (:set number relativenumber), numbertoggle switches to absolute line numbers (:set number norelativenumber) automatically when relative numbers don't make sense.")
		 (home-page "https://github.com/rust-lang/rust.vim")
		 (license license:expat)))

(define-public neovim-numbertoggle
	       (package
		 (inherit vim-numbertoggle)
		 (name "neovim-numbertoggle")
		 (arguments
		   '(#:install-plan
		     '(("doc" "share/nvim/site/pack/guix/start/numbertoggle/")
		       ("plugin" "share/nvim/site/pack/guix/start/numbertoggle/"))))
		 (synopsis "Toggles between hybrid and absolute line numbers automatically in Neovim")
		 (description
		   "Numbertoggle is a Neovim plugin that in a buffer with 'hybrid' line numbers (:set number relativenumber), numbertoggle switches to absolute line numbers (:set number norelativenumber) automatically when relative numbers don't make sense.")))
