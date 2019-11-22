How i need to set up my stuff on new systems.

Assuming checkout in `~/dotfiles`:

```bash
~$ git checkout git@github.com:yoricksijsling/dotfiles.git
```


# Emacs

Shared config:

```bash
mkdir ~/.emacs.d
ln -s ~/dotfiles/emacs.d/Cask ~/.emacs.d
ln -s ~/dotfiles/emacs.d/yorick.el ~/.emacs.d
ln -s ~/dotfiles/emacs.d/.mc-lists.el ~/.emacs.d
```

[Install Cask](http://cask.readthedocs.io/en/latest/guide/installation.html). You'll need Python and
GNU Emacs. Make sure that `emacs` points to the right version. Install packages via Cask:

```bash
cd ~/.emacs.d
cask install
```

We clone some packages manually. The `haskell-mode` and `emacs-purpose` packages have custom
additions.

```
cd ~/opensource
git clone git@github.com:yoricksijsling/haskell-mode.git
git clone git@github.com:yoricksijsling/emacs-purpose.git
```

I'm not committing `~/.emacs.d/init.el` because i put location-dependent and potentially
confidential stuff in there. Start out with something like this:

```elisp
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; We use cask, so prevent package.el from adding the following line:
;; (package-initialize)

;; Packages from other sources
(add-to-list 'load-path "~/opensource/emacs-purpose")
(add-to-list 'load-path "~/opensource/haskell-mode")

(load "~/.emacs.d/yorick.el")

(setq-default fill-column 100)

;; Use system ssh agent. Provided by keychain-environment package
(keychain-refresh-environment)

;; Postgres connections
(add-to-list 'sql-connection-alist
             '(my-database (sql-product 'postgres)
               (sql-user "admin") (sql-password "admin")
               (sql-server "localhost") (sql-database "my-database") (sql-port 5432)))
(configure-sql-connections)

```

