Emacs config for self use
===========================================

Personal Emacs configuration, only tested on macOS with Emacs 29 or higher.

Main features:
* Use Borg to manage Emacs packages as Git submodules.
* Use use-package to organize config.
* Use builtin eglot to provide LSP support.
* Optimized for dart and flutter programing.

# Installation

Run the following script to install Emacs and this configuration:

```bash
cd ~
mv .emacs.d .emacs.d.bak
git clone https://github.com/50ways2sayhard/.emacs.d.git --branch borg
cd .emacs.d
cp init-custom-example.el init-custom.el
make bootstrap-borg
make bootstrap
```


# Updating

You can update this config with `git pull`. For Updating third-party packages, run:

```bash
cd ~/.emacs.d
git submodule update --init ./lib/<package-name> # Update specific package
git submodule foreach git pull # Update each submodule to latest commit

make clean # remove all byte-code and native files
make build # byte-compile all drones and init files

make help # show brief help
```

# Why borg?

I used to manage third-party package with `straight.el`, but recently I meet some problem:
* `eglot` becomes builtin package now, but `straight.el` keep pulling the `eglot` repository when some packages depend on it. `use-package` has the same problem.

# Reference
