.intel_syntax noprefix
.data
.text
.global example
example:
	push rbp
	mov rbp, rsp
	sub rsp, 16

	;; x;
	lea rax, [rbp-16]
	push rax

	pop rax
	mov rax, [rax]
	push rax

	add rsp, 8

	lea rax, [rbp-8]
	push rax
	pop rax
	mov rax, [rax]
	push rax
	add rsp, 8

	lea rax, [rbp-16]
	push rax
	add rsp, 8

	lea rax, [rbp-8]
	push rax
	lea rax, [rbp-16]
	push rax
	pop rdi
	pop rax
	mov [rax], rdi
	push rdi
	add rsp, 8

	lea rax, [rbp-8]
	push rax
	pop rax
	mov rax, [rax]
	push rax
	pop rax
	mov rax, [rax]
	push rax
	add rsp, 8
	lea rax, [rbp-8]
	push rax
	pop rax
	mov rax, [rax]
	push rax
	push 3
	pop rdi
	pop rax
	mov [rax], rdi
	push rdi
	add rsp, 8

	lea rax, [rbp-16]
	push rax
	pop rax
	mov rax, [rax]
	push rax
	pop rax
	jmp .Lreturn.example

.Lreturn.example:
	mov rsp, rbp
	pop rbp
	ret
