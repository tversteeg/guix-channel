(define-module (gnu packages vim)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin) ; For GNU hostname
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public neovim-nightly
  (package
    (name "neovim")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neovim/neovim")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11zyj6jvkwas3n6w1ckj3pk6jf81z1g7ngg4smmwm7c27y2a6f2m"))))
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
    (license (list license:asl2.0 license:vim))))
