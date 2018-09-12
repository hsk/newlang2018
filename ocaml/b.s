	.file	"b.c"
	.text
	.globl	a
	.data
	.align 4
	.type	a, @object
	.size	a, 4
a:
	.long	8000
	.globl	c
	.align 4
	.type	c, @object
	.size	c, 4
c:
	.long	2
	.text
	.globl	b
	.type	b, @function
b:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	a(%rip), %eax
	movl	c(%rip), %ecx
	cltd
	idivl	%ecx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	b, .-b
	.globl	d
	.type	d, @function
d:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	a(%rip), %eax
	movl	c(%rip), %ecx
	cltd
	idivl	%ecx
	movl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	d, .-d
	.globl	a1
	.type	a1, @function
a1:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	a(%rip), %eax
	testl	%eax, %eax
	jne	.L6
	movl	c(%rip), %eax
	testl	%eax, %eax
	je	.L7
.L6:
	movl	$1, %eax
	jmp	.L9
.L7:
	movl	$0, %eax
.L9:
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	a1, .-a1
	.globl	a300
	.type	a300, @function
a300:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	a(%rip), %edx
	movl	c(%rip), %eax
	cmpl	%eax, %edx
	setg	%al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	a300, .-a300
	.section	.rodata
.LC0:
	.string	"%d %d\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB4:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$8, %rsp
	.cfi_offset 3, -24
	movl	$0, %eax
	call	a1
	movl	%eax, %ebx
	movl	$0, %eax
	call	b
	movl	%ebx, %edx
	movl	%eax, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	movl	$0, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 7.3.0-16ubuntu3) 7.3.0"
	.section	.note.GNU-stack,"",@progbits
