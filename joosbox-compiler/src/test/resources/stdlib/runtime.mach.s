USE32

section .text

extern _malloc

; This is just for Mac OS compilation woes
extern _start
global _main
_main:
    call _start
    ret

; Allocates eax bytes of memory. Pointer to allocated memory returned in eax.
    global __malloc
__malloc:
    push ebp
    mov ebp, esp       ; save the stack

    ;   Ensure that we use a 16-bit aligned stack
    and esp, 0xFFFFFFF0

    ;   Push a bunch of stuff onto the stack...
    push dword eax ; number of bytes to allocate
    push dword 0   ; nothing!
    push dword 0   ; nothing, this is just padding
    push dword 0   ; absolutely nothing

    call _malloc   ; memory, please

    cmp eax, 0     ; if error, exit with code 22
    jne ok
    mov eax, 22
    call __debexit
ok:
    mov esp, ebp
    pop ebp
    ret

; Debugging exit: ends the process, returning the value of
; eax as the exit code.
    global __debexit
__debexit:
    ;   Ensure that we use a 16-bit aligned stack
    and esp, 0xFFFFFFF0

    push dword eax ; exit code
    push dword 0   ; pad the stack
    
    mov eax, 0x1   ; exit
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
    push ebp
    mov ebp, esp   ; save the stack pointer
    
    ;   Ensure that we use a 16-bit aligned stack
    and esp, 0xFFFFFFF0

    mov [char], al ; save the low order byte in memory

    push 1         ; number of bytes to write
    push char      ; address of characters to start printing
    push 1         ; stdout
    push 0         ; align args to 16 byte boundary
    
    mov eax, 4     ; sys_write system call
    int 0x80
    mov eax, 0     ; return 0
    
    mov esp, ebp   ; reset the stack pointer
    pop ebp
    ret

section .data

char:
    dd 0