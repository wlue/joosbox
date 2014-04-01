extern __malloc
extern __debexit
extern __exception

%define VTableClassTagOffset 0
%define VTableInstanceOfPointerOffset 4

%define ObjectVTableOffset 0

; cast the object %1 from supertype %2 to subtype %3.
; before calling, put object vtable pointer into register %1
; after calling (if no exception), register %1 will contain new vtable pointer
%macro downcast_object 3
  ; lookup the instanceof pointer to see if this
  ; cast operation is valid. If not, error out.
  push esi

  mov esi, [%1 + VTableInstanceOfPointerOffset]
  ; esi now contains the address of the instanceof table

%%_loop_find_instance_match:
  
  cmp dword[esi], 0
  je %%_no_match

  cmp dword[esi], %3_class_tag
  je %%_found_instance_match

  add esi, 4
  jmp %%_loop_find_instance_match

%%_no_match:
  call __exception

%%_found_instance_match:
  add %1, (vtable_class_%2_%3 - vtable_class_%2)
  pop esi

%endmacro

; cast the object %1 from subtype %2 to supertype %3.
; before calling, put object vtable pointer into register %1
; after calling (if no exception), register %1 will contain new vtable pointer
%macro upcast_object 3
  ; lookup the instanceof pointer to see if this
  ; cast operation is valid. If not, error out.
  push esi

  mov esi, [%1 + VTableInstanceOfPointerOffset]
  ; esi now contains the address of the instanceof table

%%_loop_find_instance_match:
  
  cmp dword[esi], 0
  je %%_no_match

  cmp dword[esi], %3_class_tag
  je %%_found_instance_match

  add esi, 4
  jmp %%_loop_find_instance_match

%%_no_match:
  mov eax, esi
  call __debexit

%%_found_instance_match:
  sub %1, (vtable_class_%3_%2 - vtable_class_%3)
  pop esi

%endmacro