# ushin-shapes.el

`ushin-shapes.el` replaces ushin tags with [ushin
shapes](https://ushin.org/#shapes) in org documents.

### Dependencies

Nicolas Rougier's
[svg-tag-mode.el](https://github.com/rougier/svg-tag-mode) and
[svg-lib.el](https://github.com/rougier/svg-lib) make it easy to
replace tags with SVGs. Both packages can be installed from GNU ELPA
with `package-install` command.

## Installation

Clone this repository:

```
git clone https://git.sr.ht/~breatheoutbreathein/ushin-shapes.el/ ~/.local/src/ushin-shapes.el/
```

Add the following lines to your init.el file:

```
(add-to-list 'load-path "~/.local/src/ushin-shapes.el/")
(require 'ushin-shapes)
```

## Usage

Use `ushin-shapes-mode` in all org buffers:

```
(global-ushin-shapes-mode +1)
```
