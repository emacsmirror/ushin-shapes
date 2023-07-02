# ushin-shapes.el

`ushin-shapes.el` replaces ushin tags with [ushin
shapes](https://ushin.org/#shapes) in Org documents.

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

In an Org buffer, tag some headings with USHIN shapes like so:

```
* I'm excited to use Org files for community deliberation  :feelings:
** The USHIN shapes are helpful for clear communication    :thoughts:
```

Run `M-x ushin-shapes-mode` to see `:feelings:` replaced with the
heart shape:

![demo-light.png](./img/demo-light.png)

The SVG foreground color is customizable as
`ushin-shapes-foreground-color`, so you can set it to match your
theme:

![demo-dark.png](./img/demo-dark.png)

Or add the following line to your `init.el` file to use
`ushin-shapes-mode` in all Org buffers:

```
(global-ushin-shapes-mode)
```
