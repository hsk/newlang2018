	.file	"a.c"

	.text
	.globl	add
	.type	add, @function
add:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %edx
	movl	-8(%rbp), %eax
	addl	%edx, %eax
	popq	%rbp
	ret

	.section	.rodata
.LC0:
	.string	"%d\n"

	.text
	.globl	print_int
	.type	print_int, @function
print_int:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	nop
	leave
	ret

	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp

	movl	$2, %esi
	movl	$1, %edi
	call	add

	movl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %edi
	call	print_int
	movl	$0, %eax
	leave
	ret
