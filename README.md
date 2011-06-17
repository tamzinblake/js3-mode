## js-mode ##

A chimeric fork of js-mode (included with emacs 24) and [js2-mode](http://code.google.com/p/js2-mode/) that supports comma-first style and other quirks.

The goal of this project was to get a javascript mode working that supports [npm style](https://github.com/isaacs/npm/blob/master/doc/coding-style.md), but it turns out this mode is compatible with other styles as well.

Since js2-mode already used indentation code copied from js-mode (then called espresso-mode) it was straightforward to update the indentation code using the official js-mode, and then make relevant changes.

Notably, this js2-mode does not support bounce-indent, though it does support several popular indentation styles.

## Credits ##

Created by [Thom Blake](https://github.com/thomblake).

Forked from js-mode (included with emacs 24) and [js2-mode](http://code.google.com/p/js2-mode/).

There are also several changes rolled in from [mooz and dgutov's fork](https://github.com/mooz/js2-mode/) on a piecemeal basis.

Inspired by [A better coding convention for lists and object literals in Javascript](https://gist.github.com/357981) and [npm style](https://github.com/isaacs/npm/blob/master/doc/coding-style.md).

Special thanks to:

 * [Cheeso on StackOverflow](http://stackoverflow.com/questions/6144930/emacs-js-mode-for-npm-style)
 * [Mihai Bazon](http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode)

## Installation ##

js2.el should be placed in your emacs include path. You'll need to byte-compile js2-mode before using it - in emacs, `M-x byte-compile-file RET <path-to-js2.el> RET`.  If you want, js2-mode can be configured using `M-x customize-group RET js2-mode RET`.  See [here](http://code.google.com/p/js2-mode/wiki/InstallationInstructions) for detailed installation instructions on js2-mode.

## Notes ##

If your JS is in error, the indentation might look wrong.  I tend to regard this as a feature.

Remember - if you start a line with `(`, `[`, `+`, or `-`, strongly consider preceding it with a semicolon (`;`).

I expect that there are still some bugs; if you see any, **please report them**. Feel free to **file issue reports on github** for things like "it indented like [code block] but I want it to be [code block]".

## License ##

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see http://www.gnu.org/licenses/.
