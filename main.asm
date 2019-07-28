section .text
global _start
	
_start:
	
	; Move the contents of variables
	; mov   ecx, 11
	; mov   ebx, 13

	push    11
	push    100

	pop     r10
	pop     r11
	add     r10, r11

	cmp     rax, rdi                                ; 05DB _ 48: 39. F8
        sete    al                                      ; 05DE _ 0F 94. C0
        movzx   rax, al                                 ; 05E1 _ 48: 0F B6. C0

_exit:
	mov   rax, 1
	mov   rbx, r10
	int   80h
