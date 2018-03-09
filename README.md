How i need to set up my stuff on new systems.

Assuming checkout in `~/dotfiles`:

```bash
~$ git checkout git@github.com:yoricksijsling/dotfiles.git
```


# Emacs

Shared config:

```bash
ln -s ~/dotfiles/emacs.d/yorick.el ~/.emacs.d/yorick.el
ln -s ~/dotfiles/emacs.d/.mc-lists.el ~/.emacs.d/.mc-lists.el
```

Not committing `~/.emacs.d/init.el` because i put location-dependent and potentially confidential
stuff in there. Start out with something like this:

```elisp
(package-initialize)
(load "~/.emacs.d/yorick.el")

(setq-default fill-column 100)

;; Use system ssh agent. Provided by keychain-environment package
(keychain-refresh-environment)

(custom-set-variables
 '(package-selected-packages
   (quote
    (hydra ivy-purpose orglink keychain-environment web-mode ensime scala-mode yaml-mode elpy counsel-projectile wgrep github-browse-file virtualenvwrapper smart-mode-line-powerline-theme markdown-mode intero phi-search multiple-cursors haskell-mode framemove buffer-move auto-highlight-symbol))))
```

