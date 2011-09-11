# Emacs Dev Kit

## About

The Emacs Dev Kit is a set of customizations and extensions for the latest GNU Emacs
(currently 23.2) designed to improve the development experience on Emacs and make it
more competitive to the modern IDEs. Sure, I know that Emacs is the greatest, but many people
don't and I hope this powerful setup will make believers out of them as well.

I have borrowed the idea from technomancy's Emacs Starter Kit, but pushed it to the next level.

## Enhanced language support

What do I mean by "enhanced support"? Basically that Emacs features
built-in support for that particular programming language, but I've
augmented it with additional settings and helper modes or
functions. On some occasions I've replaced the bundled support for the
language all together (Prolog, LaTeX, etc)...

The Emacs Dev Kit features enhanced support for the following programming languages:

* C/C++
* Ruby
* Common Lisp
* Emacs Lisp
* Scheme
* Perl
* Python
* Java
* Prolog
* LaTeX
* XML

I'll be adding more stuff along the way.

## Additional programming languages support

* Clojure
* Scala
* Haskell
* CoffeeScript

## Additional markup languages support

* Markdown
* Sass
* Haml
* Yaml

## Misc

The Emacs Dev Kit uses by default the Zenburn color theme (a personal
preference of me and many other hackers), but you can easily disable
(replace) it.

## Emacs 24 compatibility

I've recently started to tweak the config to use some of the exciting
new features from Emacs 24 (the global electric modes, the new
deftheme infrastructure, etc).

If you're already using Emacs 24 the EDK will a good fit for
you. Emacs 23.2 will remain fully supported of course (at least until
the official release of Emacs 24).

## Installation

    git clone git://github.com/bbatsov/emacs-dev-kit.git path/to/local/repo
    ln -s path/to/local/repo ~/.emacs.d

## Running

Nothing fancy here. Just start Emacs as usual. Personally I run Emacs
in daemon mode:

`emacs --daemon`

Afterwards I connect to the server with either a terminal or a GUI
client like this:

    emacsclient -t
    emacsclient -c

## Known issues

I've opted against bundling SLIME with the EDK since it's a rapidly
moving target. I heartily recommend you to install it from
Quicklisp. If you do so with the default Quicklisp settings EDK will
pick up the SLIME installation automatically.

The proper LaTeX support depends on Auctex being installed. You won't
get any errors without auctex, though. Just keep in mind you have to
install it manually if you're planning of doing some serious LaTeX editing.

## Bugs & Improvements
Bug reports and suggestions for improvements are always welcome. github pull request are even better! ;-)

Bozhidar
