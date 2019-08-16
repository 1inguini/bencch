.intel_syntax noprefix
.data
.text
.global example
example:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  lea rax, [rbp-16]
  push rax
  push 1
  push 2
  push 4
  push 8
  pop r8
  pop rcx
  pop rdx
  pop rsi
  pop rdi
  mov rax, rsp
  and rax, 15
  jnz .Lcall1
  mov rax, 0
  call alloc4
  jmp .Lend1
.Lcall1:
  sub rsp, 8
  mov rax, 0
  call alloc4
  add rsp, 8
.Lend1:
  push rax
  pop rax
  mov eax, eax
  push rax
  add rsp, 8
  lea rax, [rbp-8]
  push rax
  lea rax, [rbp-16]
  push rax
  pop rax
  mov rax, [rax]
  push rax
  push 2
  pop rdi
  pop rax
  imul rdi, 8
  add rax, rdi
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
  lea rax, [rbp-16]
  push rax
  pop rax
  mov rax, [rax]
  push rax
  push 3
  pop rdi
  pop rax
  imul rdi, 8
  add rax, rdi
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
  pop rax
  jmp .Lreturn.example
.Lreturn.example:
  mov rsp, rbp
  pop rbp
  ret
