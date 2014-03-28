extern __malloc
extern __debexit
extern __exception


%define VTableBase(x) (vtable_class_ %+ x)
%define VMethodLabel(class, method) vtable_%+class%+ _method__%+ method
;   This call takes a pointer to a vtable in reg and calls the appropriate method.
;   We should usually keep the "this" pointer in EBX for simplicity.
%define VMethodCall(reg, class, method) call [reg + (VMethodLabel(class, method) - VTableBase(class))]

%define VTableClassRef(class) vtable_class_%+class
%define VTableNestedClassRef(class, super) vtable_class_%+class%+_%+super

%define VTableInstanceOfRef(class) VTableClassRef(class)%+_instanceof: dd instanceof_class_%+class
%define VTableNestedInstanceOfRef(class, super) VTableNestedClassRef(class, super)%+_instanceof: dd instanceof_class_%+class

%define ClassTagForClass(class)  class %+ _class_tag

%define VTableClassHeader(class) VTableClassRef(class): dd ClassTagForClass(class)
%define VTableNestedClassHeader(class, super) VTableNestedClassRef(class, super): dd ClassTagForClass(class)

%define VTableMethodDef(class, method, impl) VMethodLabel(class, method): dd impl
%define VTableNestedMethodDef(class, super, method, impl) VMethodLabel(class, method)%+_%+super: dd impl

%define InstanceOfHeader(class) instanceof_class_%+class:
%define InstanceOfEntry(otherclass) dd otherclass%+_class_tag
%define InstanceOfEnd dd 0x0

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

  cmp dword[esi], ClassTagForClass(%3)
  je %%_found_instance_match

  add esi, 4
  jmp %%_loop_find_instance_match

%%_no_match:
  call __exception

%%_found_instance_match:
  add %1, (VTableNestedClassRef(%2, %3) - VTableClassRef(%2))
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

  cmp dword[esi], ClassTagForClass(%3)
  je %%_found_instance_match

  add esi, 4
  jmp %%_loop_find_instance_match

%%_no_match:
  mov eax, esi
  call __debexit

%%_found_instance_match:
  sub %1, (VTableNestedClassRef(%3, %2) - VTableClassRef(%3))
  pop esi

%endmacro