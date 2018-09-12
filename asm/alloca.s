	.file	"alloca.c"
	.text
	.type	a, @function
a:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp

	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax

	movl	$16, %eax
	subq	$1, %rax
	addq	$115, %rax

	movl	$16, %esi
	movl	$0, %edx
	divq	%rsi

	imulq	$16, %rax, %rax
	subq	%rax, %rsp
	movq	%rsp, %rax
	addq	$15, %rax
	shrq	$4, %rax
	salq	$4, %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	$20, (%rax)

	movl	$16, %eax
	subq	$1, %rax
	addq	$215, %rax

	movl	$16, %edi
	movl	$0, %edx
	divq	%rdi
	imulq	$16, %rax, %rax
	subq	%rax, %rsp
	movq	%rsp, %rax
	addq	$15, %rax
	shrq	$4, %rax
	salq	$4, %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	$30, (%rax)

	movl	$16, %eax
	subq	$1, %rax
	addq	$315, %rax
	movl	$16, %ecx
	movl	$0, %edx
	divq	%rcx
	imulq	$16, %rax, %rax
	subq	%rax, %rsp
	movq	%rsp, %rax
	addq	$15, %rax
	shrq	$4, %rax
	salq	$4, %rax
	movq	%rax, -16(%rbp)

	movq	-32(%rbp), %rax
	movq	(%rax), %rdx
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	addq	%rax, %rdx

	movq	-16(%rbp), %rax
	movq	%rdx, (%rax)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	-8(%rbp), %rcx
	xorq	%fs:40, %rcx
	je	.L3
	call	__stack_chk_fail@PLT
.L3:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	a, .-a
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, %eax
	call	a
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 7.3.0-16ubuntu3) 7.3.0"
	.section	.note.GNU-stack,"",@progbits
