section .text

; This is just for Mac OS compilation woes
extern _start
global _main
_main:
    call _start
    ret

extern _malloc

; Allocates eax bytes of memory. Pointer to allocated memory returned in eax.
    global __malloc
__malloc:
    push eax     ; number of bytes to allocate
    sub esp, 4   ; align the stack
    call _malloc
    add esp, 8   ; reset the stack
    cmp eax, 0   ; on error, exit with code 22
    jne ok
    mov eax, 22
    call __debexit
ok:
    ret

; Debugging exit: ends the process, returning the value of
; eax as the exit code.
    global __debexit
__debexit:
    push eax     ; exit code is stored in eax
    sub esp, 4   ; align the stack
    mov eax, 1   ; sys_exit system call
    int 0x80

; Exceptional exit: ends the process with exit code 13.
; Call this in cases where the Joos code would throw an exception.
    global __exception
__exception:
    mov al, 13
    call __debexit

; Implementation of java.io.OutputStream.nativeWrite method.
; Outputs the low-order byte of eax to standard output.
    global NATIVEjava.io.OutputStream.nativeWrite
NATIVEjava.io.OutputStream.nativeWrite:
    mov [char], al ; save the low order byte in memory

    push 1         ; number of bytes to write
    push char      ; address of characters to start printing
    push 1         ; stdout
    sub esp, 4     ; align stack to 16 byte boundary

    mov eax, 4     ; sys_write system call
    int 0x80
    add esp, 16    ; reset stack
    mov eax, 0     ; return 0
    ret

section .data

char:
    dd 0