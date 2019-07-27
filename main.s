.intel_syntax noprefix
.global main

main:
	mov rax,2
	push rax
	mov rax,3
	push rax
	
	pop rdi
	pop rax
	add rax, rdi
	push rax
	pop rax
	
	ret
