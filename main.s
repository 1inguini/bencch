.intel_syntax noprefix
.data
.text
.global example
example:
	push rbp
	mov rbp, rsp
	sub rsp, 16

	;; y = &x;
	push [rbp-16]
	pop [rbp-8]
	push rdi

	add rsp, 8

	;; y;
	lea rax, [rbp-8]
	push rax

	pop rax
	mov rax, [rax]
	push rax

	add rsp, 8

	;; *y;
	lea rax, [rbp-8]
	push rax

	pop rax
	mov rax, [rax]
	push rax

	pop rax
	mov rax, [rax]
	push rax

	add rsp, 8

	;; *y = 3
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

	;; return x;
	lea rax, [rbp-16]
	push rax

	pop rax
	mov rax, [rax]
	push rax

	pop rax
	jmp .Lreturn.exampl
.Lreturn.example:
	mov rsp, rbp
	pop rbp
	ret
