# Emacs Dev Kit

## About

The Emacs Dev Kit is a set of customizations and extensions for the latest GNU Emacs
(currently 23.2) designed to improve the development experience on Emacs and make it
more competitive to the modern IDEs. Sure, I know that Emacs is the greatest, but many people
don't and I hope this powerful setup will make believers out of them as well.

I have borrowed the idea from technomancy's Emacs Starter Kit, but pushed it to the next level.

## Enhanced support

EDK features enhanced support for the following programming languages:
    * Ruby
    * Common Lisp/Scheme/Clojure
    * Perl
    * Python
    * Java
    * Scala
    * Prolog

I'll try add more stuff along the way

## Installation

`git clone git://github.com/bbatsov/emacs-dev-kit.git path/to/local/repo`

`ln -s path/to/local/repo ~/.emacs.d`

## Running

Nothing fancy here. Just start Emacs as usual. Personally I run Emacs
in daemon mode:

`emacs --daemon`

Afterwards I connect to the server with either a terminal or a GUI
client like this:

`emacsclient -t`

`emacsclient -c` 

## Known issues

The proper LaTeX support depends on Auctex being installed. You won't
get any errors without auctex, though. Just keep in mind you have to
install it manually if you're planning of doing some serious LaTeX editing.

## Bugs & Improvements
Bug reports and suggestions for improvements are always welcome. github pull request are even better! ;-)

Bozhidar
