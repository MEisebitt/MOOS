# MOOS
A small project toying arround with an x86 operating system
## Requirements
* NASM Compiler
* QEMU Emulator
* Make 
## Compiling & Running
* Run `make` to compile the project
* Run `qemu-system-x86_64 -drive file=boot.bin,format=raw -m 512M -monitor stdio -s -S` or `qemu-system-x86_64.exe -drive [...]`
* Emulation might need to be manually unpaused after starting, with either `c` in the terminal or over the interface
