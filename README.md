I'm having fun indulging myself in writing a low-level Forth targeting
qemu-system-aarch64.  This includes a meta/compiler that takes
forth, acts as a host Forth interpreter, and emits assembler that
is then linked against a small kernel; the goal is to try and combine
putting together a small operating system running a live, dynamic,
Forth environment with some utilities like a text editor, a simple
shell, and a basic file system.
