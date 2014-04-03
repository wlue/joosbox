section .text

extern _malloc

; This is just for Mac OS compilation woes
extern _start
global _main
_main:
    call _start
    ret

section .text

kernel_entry:
    int 0x80
    ret

; Allocates eax bytes of memory. Pointer to allocated memory returned in eax.
    global __malloc
__malloc:
    push eax                          ; back-up the size to be allocated
    mov eax, [brk_region]             ; load the current brk region
    test eax, eax                     ; if not initialized yet, then
    je  initialize_brk                ; initialize it
brk_initialized:                      ; eax now contains the region
    pop ebx                           ; pop size to be allocated to ebx
    push eax                          ; back it up
    add eax, ebx                      ; adjust the pointer
    mov [brk_region], eax             ; store the adjusted pointer
    mov eax, [brk_region_size]        ; get the remaining region size
    cmp eax, ebx                      ; if it less than the size we want to allocate
    jb __exception                    ; goto exception
    sub eax, ebx                      ; otherwise, update the remaining size
    mov [brk_region_size], eax        ; store it
    pop eax                           ; return the backed-up pointer to the region
    ret

; Initialize the brk by allocating a chunk of memory
initialize_brk:
    push 0                            ; offset = 0
    push -1                           ; fd = -1
    push 0x1000                       ; flag = MAP_ANONYMOUS
    push 0x03                         ; prot = PROT_READ | PROT_WRITE
    mov eax, [brk_region_size]        ; get the size of the region (predefined)
    push eax
    push 0                            ; addr = 0
    mov eax, 197
    call kernel_entry                 ; call mmap with brk_region_size to allocate
    add esp, 24                       ; fix the stack pointer
    cmp eax, 0xFFFFFFFF               ; if it fails then
    je __exception                    ; goto exception
    mov [brk_region], eax             ; set the pointer
    jmp brk_initialized

; Debugging exit: ends the process, returning the value of
; eax as the exit code.
    global __debexit
__debexit:
    push eax                          ; status
    mov eax, 1
    call kernel_entry                 ; call exit

; Exceptional exit: ends the process with exit code 13.
; Call this in cases where the Joos code would throw an exception.
    global __exception
__exception:
    push 13                           ; status = 13
    mov eax, 1
    call kernel_entry                 ; call exit

; Implementation of java.io.OutputStream.nativeWrite method.
; Outputs the low-order byte of eax to standard output.
    global NATIVEjava.io.OutputStream.nativeWrite
NATIVEjava.io.OutputStream.nativeWrite:
    mov [char], al ; save the low order byte in memory
    push 1         ; nbyte = 1
    push char      ; buf = char
    push 1         ; fd = stdout
    mov eax, 4
    call kernel_entry ; call write
    add esp, 12    ; fix the stack pointer
    mov eax, 0     ; return 0
    ret

section .data
    
brk_region_size:
    dd 41943040
brk_region:
    dd 0
char:
    dd 0