## js-mode ##

An improved chimeric fork of js-mode and js2-mode that supports comma-first style and other quirks.

The goal of this project was to get a javascript mode working that supports [npm style](https://github.com/isaacs/npm/blob/master/doc/coding-style.md), but it turns out this mode is compatible with other styles as well. Most of the credit for the indentation goes to js-mode, with some handy special cases put in by yours truly.

## Credits ##

Created by [Thom Blake](https://github.com/thomblake).

Forked from js-mode and js2-mode.

Inspired by [A better coding convention for lists and object literals in Javascript](https://gist.github.com/357981) and [npm style](https://github.com/isaacs/npm/blob/master/doc/coding-style.md).

With help from [Cheeso on StackOverflow](http://stackoverflow.com/questions/6144930/emacs-js-mode-for-npm-style) and [Mihai Bazon](http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode)

The js2-mode included here is basically Steve Yegge's js2-mode version 20090723 findable [here](http://code.google.com/p/js2-mode/) with some reasonable defaults set.

The js-mode included here is the one included with emacs version 24, with some modifications to the way it indents in certain cases and added backwards-compatibility for emacs version 23.2.

## Installation ##

Both js.el and js2.el should be placed in your emacs include path. You'll need to byte-compile js2-mode before using it - in emacs, `M-x byte-compile-file RET <path-to-js2.el> RET`.  If you want, js2-mode can be configured using `M-x customize-group RET js2-mode RET`.  See [here](http://code.google.com/p/js2-mode/wiki/InstallationInstructions) for detailed installation instructions on js2-mode.

The .emacs file included contains what you need to stick the 2 modes together.

I know this is compatible with emacs 23.2 on freebsd and 23.4 on ubuntu, but I welcome feedback on compatibility/incompatibility with your version of emacs.

## Notes ##

Using this will entail having 2 separate JS parsers running, so sometimes it takes a while to 'catch up' - if the indentation on a line looks off, try pressing TAB again.  Right now it looks like this only happens with non-comma-first continued var statements.  A future version will be a single mode based on JS2-mode (which has a better JS parser) which should solve some of these problems.

If your JS is in error, the indentation might look wrong.  I tend to regard this as a feature.

I expect that there are still some bugs; if you see any, please report them.  I have a sneaking suspicion that I'm not handling lines with comments very well. Feel free to file issue reports on github for things like "this doesn't indent [code block] how I want it to".

Remember - if you start a line with `(`, `[`, `+`, or `-`, strongly consider preceding it with a semicolon (`;`).

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

(Several programs included and referenced here are GPL, so this is too
why not.)
