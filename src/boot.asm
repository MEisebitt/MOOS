BITS 16
org 0x7c00 ; Where the bootsector is loaded in memory

mov si, artWelcomeMsg
call printString
jmp $

artWelcomeMsg:
    db 219, 223, 220, 223, 219, " ", 219, 223, 219, " ", 219, 223, 219, " ", 219, 223, 223, 10, 13         ; █▀▄▀█ █▀█ █▀█ █▀▀
    db 219, " ", 223, " ", 219, " ", 219, 220, 219, " ", 219, 220, 219, " ", 220, 220, 219, 10, 13, 3      ; █ ▀ █ █▄█ █▄█ ▄▄█

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

printChar: ; Prints a char that has to be put in al
    mov ah, 0x0e ; Activate teletype mode
    int 0x10 ; BIOS interrupt to print
    ret

printString: ; Print string that has adress in si until EOT character (3) ist found
    mov ah, 0x0e ; Activate teletype mode
    xor bx, bx ; Set counter to 0
    .beginPrintLoop:
        mov al, [si + bx] ; Move char into register
        cmp al, 3 ; Check if char is ETX
        je .postPrintLoop ; If ETX break free

        int 0x10 ; BIOS interrupt to print
        inc bx ; The contents of BX, CX, DX, SS, CS and DS segment registers are not affected
        jmp .beginPrintLoop

    .postPrintLoop:
        ret

printByteAsHex: ; Print bytes in si as hex until number in dx is reached
    xor cx, cx ; Set count register to zero
    xor bx, bx ; Set offset to zero
    .beginPrintLoop:
        cmp dx, cx ; Check if end of loop is reached
        je .postPrintLoop

        mov bx, cx

        xor ax, ax
        mov al, [si + bx]
        shr al, 4
        xor bx, bx
        mov bl, al
        mov ah, 0x0e
        mov al, [hexchars + bx]
        int 0x10 ; BIOS interrupt to print

        mov bx, cx

        xor ax, ax
        mov al, [si + bx]
        and al, 0b00001111
        xor bx, bx
        mov bl, al
        mov ah, 0x0e
        mov al, [hexchars + bx]
        int 0x10 ; BIOS interrupt to print

        inc cx
        jmp .beginPrintLoop

    .postPrintLoop:
        ret

; getMemMap:
;     mov dx, 0x0500
;     mov es, dx ; Set es to 0x0050 (d16*x50=x500)
;     mov di, 0x0000 ; Set di to offset where the data will go
;     xor ax, ax
;     mov [es:di]

; printMemMapEntry:
;     xor ax, ax ; clear register
;     mov [0x0500], ax ; setup outer loop counter
;     xor cx, cx
    
;     .printLoop:
;         mov bx, [es:di + 24] ; get index for outer loop
;         mov si, memmaplengths ; get adress for loop lengths
;         mov ax, [si + bx] ; get current loop length
;         add ax, 48
;         call printChar
;         sub ax, 48
;         cmp cx, ax
;         jge .interLoop

;         mov si, hexchars ; Get adress of char list
;         xor edx, edx ; Clear dx as we want to use dl
;         mov bx, cx ; Move count to bx as only the bx register can index
;         mov dl, [es:di + bx] ; Write one byte of the number to dl

;         shr dl, 4
;         mov bx, dx
;         mov al, [si + bx]
;         ; mov al, 0x58
;         call printChar

;         xor edx, edx ; Clear dx as we want to use dl
;         mov bx, cx ; Move count to bx as only the bx register can index
;         mov dl, [es:di + bx] ; Write one byte of the number to dl

;         and dl, 0b00001111
;         mov bx, dx
;         mov al, [si + bx]
;         call printChar

;         inc cx
;         jmp .printLoop

;     .interLoop:
;         mov al, 0x7C ; Move '|' into register
;         call printChar
;         mov ax, [es:di + 24] ; load outer loop counter
;         cmp ax, 2 ; chekc if limit is reached
;         jge .postLoop

;         inc ax
;         mov [es:di + 24], ax ; save updated outer loop counter
;         xor cx, cx
;         jmp .printLoop

;     .postLoop:
;         mov al, 0x0A
;         call printChar
;         mov al, 0x0D
;         call printChar
;         ret


; getMemMapEntry:
;     mov dx, 0x0500
;     mov es, dx ; Set es to 0x0050 (d16*x50=x500)
;     mov di, 0x0002 ; Set di to 0x0002 -> Where the data will go
; 	xor ebx, ebx ; ebx (continuation value) must be 0 to start
; 	xor bp, bp ; keep an entry count in bp
; 	mov edx, 0x0534D4150 ; Place "SMAP" into edx
; 	mov eax, 0xe820
; 	mov [es:di + 20], dword 1 ; force a valid ACPI 3.X entry
; 	mov ecx, 24 ; ask for 24 bytes
; 	int 0x15
;     jc failed1 ; carry set on first call means "unsupported function"

;     mov edx, 0x0534D4150 ; Some BIOSes apparently trash this register?
; 	cmp eax, edx ; on success, eax must have been reset to "SMAP"
;     jne failed2

;     test ebx, ebx ; ebx = 0 implies list is only 1 entry long (worthless)
;     je failed2

;     success:
;         mov si, msg1
;         call printString
;         call printMemMapEntry
;         ret
;         ; xor cx, cx
;         ; .beginPrintLoop:
;         ;     cmp cx, 20
;         ;     je .endOfLoop

;         ;     mov si, hexchars ; Get adress of char list
;         ;     xor edx, edx ; Clear dx as we want to use dl
;         ;     mov bx, cx ; Move count to bx as only the bx register can index
;         ;     mov dl, [es:di + bx] ; Write one byte of the number to dl

;         ;     shr dl, 4
;         ;     mov bx, dx
;         ;     mov al, [si + bx]
;         ;     call printChar

;         ;     xor edx, edx ; Clear dx as we want to use dl
;         ;     mov bx, cx ; Move count to bx as only the bx register can index
;         ;     mov dl, [es:di + bx] ; Write one byte of the number to dl

;         ;     and dl, 0b00001111
;         ;     mov bx, dx
;         ;     mov al, [si + bx]
;         ;     call printChar

;         ;     inc cx
;         ;     jmp .beginPrintLoop

;         ; .endOfLoop:
;         ;     ret

;     failed1:
;         mov si, msg2
;         call printString
;         ret

;     failed2:
;         mov si, msg3
;         call printString
;         ret

times 510-($-$$) db 0
dw 0xaa55 ; Or "db 0x55, 0xaa" which is the final word of the bootsector