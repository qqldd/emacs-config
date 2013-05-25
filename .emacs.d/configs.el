
(load "configs/package")
(load "configs/global")
(load "configs/defuns")
(load "configs/server")

(load "configs/c-config")
(load "configs/cedet")
(load "configs/yasnippet")
(load "configs/auctex")

(if window-system
    (load "configs/window")
  (load "configs/terminal"))
