# ushin-shapes.el

`ushin-shapes.el` replaces ushin tags with [ushin
shapes](https://ushin.org/#shapes) in Org documents.

In an Org buffer, tag some headings with USHIN shapes like so:

```
* I'm excited to use Org files for community deliberation  :feelings:
** The USHIN shapes are helpful for clear communication    :thoughts:
```

Run `M-x ushin-shapes-mode` to see `:feelings:` replaced with the
heart shape and `:thoughts:` replaced with a circle.

![demo-light.png](./img/demo-light.png)

The SVG foreground color is customizable as
`ushin-shapes-foreground-color`, so you can set it to match your
theme:

![demo-dark.png](./img/demo-dark.png)

## Installation

`ushin-shapes.el` requires
[Emacs](https://www.gnu.org/software/emacs/) version 27.1 or later.

`ushin-shapes.el` is available on
[MELPA](https://melpa.org/#/getting-started). Once you've set up MELPA,
run `M-x package-install` then enter `ushin-shapes`.

## Usage

Run `M-x ushin-shapes-mode` in an Org buffer or add the following line
to your `init.el` file to use `ushin-shapes-mode` in all Org buffers:

```
(global-ushin-shapes-mode)
```

## Bugs and Patches

Bugs can be submitted to the [ushin issue tracker](https://todo.sr.ht/~ushin/ushin). Patches, comments or
questions can be submitted to the [ushin public inbox](https://lists.sr.ht/~ushin/ushin).

## Chat Room

You're welcome to join our public XMPP chat
room!

- `xmpp:discuss@conference.ushin.org` ([Join anonymously from your browser](https://anonymous.cheogram.com/discuss@conference.ushin.org))
- `#xmpp_discuss_conference.ushin.org:matrix.org` (Matrix bridge)
