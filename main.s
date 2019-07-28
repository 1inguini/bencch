.intel_syntax noprefix
.global main

main:
	mov rax,2
	push rax
	mov rax,3
	push rax

	pop rdi
	pop rax
	cmp rax, rdi
	sete al
	movzb rax, al	
	ret
