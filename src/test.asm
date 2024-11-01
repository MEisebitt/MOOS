BITS 16
org 0x7c00 ; Where the bootsector is loaded in memory

%include "uefi.inc"

istruc int64
    at int64.data, dq "ABCDEFGH"
iend
db '_'
db '_'
istruc int64
    at int64.data, dq "IJKLMNOP"
iend
align 8

jmp $

strMemMap:
    db 10, 13, "Memory Map:", 10, 13, 3

strUnsupported:
    db "Unsupported function ", 3

strUnsuccess:
    db "Unsuccessfull call ", 3

hexchars:
    db "0123456789ABCDEF"

memmaplengths:
    db 7, 7, 3

times 510-($-$$) db 0
dw 0xaa55 ; Or "db 0x55, 0xaa" which is the final word of the bootsector