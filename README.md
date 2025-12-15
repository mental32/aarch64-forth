I'm having fun indulging myself in writing a low-level Forth targeting
aarch64.  This includes a metacompiler that takes forth, acts as a
host Forth interpreter, and produces assembler that is then linked
against a small kernel written in assembly (aarch64); the goal is
to try and combine putting together a small operating system running
a live, dynamic, Forth environment with some utilities like a text
editor, a simple shell, and a basic file system.
