section .text
global _start
	
_start:
	
	; Move the contents of variables
	; mov   ecx, 11
	; mov   ebx, 13

	push 11
	push 100

	pop r10
	pop r11
	add r10, r11
	
_exit:
	mov   rax, 1
	mov   rbx, r10
	int   80h
