(define-module (local neovim)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix packages)
	       #:use-module (guix utils)
	       #:use-module (guix download)
	       #:use-module (guix git-download)
	       #:use-module (guix build-system cmake)
	       #:use-module (guix build-system copy)
	       #:use-module (guix build-system gnu)
	       #:use-module (gnu packages base)
	       #:use-module (gnu packages gettext)
	       #:use-module (gnu packages gperf)
	       #:use-module (gnu packages jemalloc)
	       #:use-module (gnu packages pkg-config)
	       #:use-module (gnu packages node)
	       #:use-module (gnu packages libevent)
	       #:use-module (gnu packages lua)
	       #:use-module (gnu packages serialization)
	       #:use-module (gnu packages terminals))

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

(define-public neovim-nightly
	       (let ((commit "6a77def1ee05d4e4eceddb559bc779cd9b805614")
		     (revision "0"))
		 (package
		   (name "neovim-nightly")
		   (version (string-append "0.5.0-" revision "." (string-take commit 7)))
		   (source
		     (origin
		       (method git-fetch)
		       (uri (git-reference
			      (url "https://github.com/neovim/neovim")
			      (commit commit)))
		       (file-name (string-append name "-" version "-checkout"))
		       (sha256
			 (base32 "13029a60bip8nmlny4p6hrvvmhmw8d4cdl9waacdbbh9cmxlchyn"))))
		   (build-system cmake-build-system)
		   (arguments
		     `(#:modules ((srfi srfi-26)
				  (guix build cmake-build-system)
				  (guix build utils))
		       #:configure-flags '("-DPREFER_LUA:BOOL=YES")
		       #:phases
		       (modify-phases %standard-phases
				      (add-after 'unpack 'set-lua-paths
						 (lambda* (#:key inputs #:allow-other-keys)
							  (let* ((lua-version "5.1")
								 (lua-cpath-spec
								   (lambda (prefix)
								     (let ((path (string-append prefix "/lib/lua/" lua-version)))
								       (string-append path "/?.so;" path "/?/?.so"))))
								 (lua-path-spec
								   (lambda (prefix)
								     (let ((path (string-append prefix "/share/lua/" lua-version)))
								       (string-append path "/?.lua;" path "/?/?.lua"))))
								 (lua-inputs (map (cute assoc-ref %build-inputs <>)
										  '("lua"
										    "lua-luv"
										    "lua-lpeg"
										    "lua-bitop"
										    "lua-libmpack"))))
							    (setenv "LUA_PATH"
								    (string-join (map lua-path-spec lua-inputs) ";"))
							    (setenv "LUA_CPATH"
								    (string-join (map lua-cpath-spec lua-inputs) ";"))
							    #t)))
				      (add-after 'unpack 'prevent-embedding-gcc-store-path
						 (lambda _
						   ;; nvim remembers its build options, including the compiler with
						   ;; its complete path.  This adds gcc to the closure of nvim, which
						   ;; doubles its size.  We remove the refirence here.
						   (substitute* "cmake/GetCompileFlags.cmake"
								(("\\$\\{CMAKE_C_COMPILER\\}") "/gnu/store/.../bin/gcc"))
						   #t)))))
		   (inputs
		     `(("libuv" ,libuv)
		       ("msgpack" ,msgpack)
		       ("libtermkey" ,libtermkey)
		       ("libvterm" ,libvterm)
		       ("unibilium" ,unibilium)
		       ("jemalloc" ,jemalloc)
		       ("libiconv" ,libiconv)
		       ("tree-sitter" ,tree-sitter)
		       ("lua" ,lua-5.1)
		       ("lua-luv" ,lua5.1-luv)
		       ("lua-lpeg" ,lua5.1-lpeg)
		       ("lua-bitop" ,lua5.1-bitop)
		       ("lua-libmpack" ,lua5.1-libmpack)))
		   (native-inputs
		     `(("pkg-config" ,pkg-config)
		       ("gettext" ,gettext-minimal)
		       ("gperf" ,gperf)))
		   (home-page "https://neovim.io")
		   (synopsis "Fork of vim focused on extensibility and agility")
		   (description "Neovim is a project that seeks to aggressively
				refactor Vim in order to:
				@itemize
				@item Simplify maintenance and encourage contributions
				@item Split the work between multiple developers
				@item Enable advanced external UIs without modifications to the core
				@item Improve extensibility with a new plugin architecture
				@end itemize\n")
				;; Neovim is licensed under the terms of the Apache 2.0 license,
				;; except for parts that were contributed under the Vim license.
				(license (list license:asl2.0 license:vim)))))

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
		   '(#:phases (modify-phases %standard-phases
					     (delete 'configure))))
		 (native-inputs
		   `(("pkg-config" ,pkg-config)
		     ("node" ,node)))
		 (synopsis "TODO")
		 (description
		   "TODO")
		 (home-page "https://tree-sitter.github.io")
		 (license license:expat)))

tree-sitter
