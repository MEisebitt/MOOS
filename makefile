boot.bin: src/boot.asm
	nasm -f bin src/boot.asm -o boot.bin