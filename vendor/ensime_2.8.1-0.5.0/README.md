# ENSIME
the ENhanced Scala Interaction Mode for Emacs

# Links
- [ Downloads ](https://github.com/aemoncannon/ensime/downloads)
- [ Manual ](http://aemon.com/file_dump/ensime_manual.html)
- [ Discussion Group ](http://groups.google.com/group/ensime?hl=en)


## Features

- Highlight errors and warnings in your code buffers.
- Inspect the type of any expression.
- Browse packages
- Completion for variables, methods, constructors, etc.
- Incrementally search through classpath symbols
- Find references to a symbol
- Jump to symbol definitions.
- Automated Refactorings (rename, organize imports, extract method)
- Source Formatting
- AST-based selection
- Supports sbt,Maven,Ivy projects
- Embedded sbt shell
- REPL
- Debugger


## Demo Videos

- [Overview (a bit out of date)](http://www.youtube.com/watch?v=A2Lai8IjLoY)
- [Searching](http://www.youtube.com/watch?v=fcgnAJz98QE)
- [Debugger Support](http://www.youtube.com/watch?v=v7-G6vD42z8)



## System Requirements

- Emacs 22 or later.
- Unix-like OS or Windows.
- Java Runtime
- A Scala 2.8.1 compatible project. 


## Documentation
- [The ENSIME User Manual](http://aemon.com/file_dump/ensime_manual.html)


## Quick Start

__1) Install scala-mode__

ENSIME is designed to compliment scala-mode (or any other scala language mode). scala-mode can be found in the Scala distribution under ./misc/scala-tool-support/emacs/. The rest of the steps assume your scala-mode is installed and working correctly.

__2) Install ensime-mode__

Download the ENSIME distribution from the github [downloads page](http://github.com/aemoncannon/ensime/downloads). Unpack the ENSIME distribution into a directory of your choosing. 

Add the following lines to your .emacs file:

    ;; Load the ensime lisp code...
    (add-to-list 'load-path "ENSIME_ROOT/elisp/")
    (require 'ensime)

    ;; This step causes the ensime-mode to be started whenever
    ;; scala-mode is started for a buffer. You may have to customize this step
    ;; if you're not using the standard scala mode.
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

    ;; MINI HOWTO: 
    ;; Open .scala file. M-x ensime (once per project)


__3) Verify Permissions__

Verify that the startup script (usually bin/server.sh) has executable permissions.


__4) Create Project__

In Emacs, execute M-x ensime-config-gen. Follow directions in the mini-buffer to create a .ensime file for your project.. 


__5) Start ENSIME__

Execute M-x ensime
You only need to do this once per project.


## Developer Quick Start
Note: This section is for people who want to hack on ENSIME itself.

After cloning, and before you can run ENSIME, you must create the distribution directory structure. The sbt task 'stage' will create the directory 'dist' underneath the root clone directory. Then, follow the install instructions in section 2.2 above, substituting CLONE_DIR/dist as the root of your ENSIME distribution.


The work-flow I use when hacking ENSIME:

- Edit source files
- 'sbt update'
- 'sbt dist'
- Stop existing ENSIME server by killing *inferior-ensime-server* buffer
- Restart ENSIME with M-x ensime
