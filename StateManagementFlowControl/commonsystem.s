	#Scheme-specific
	.globl NULL_LIST
	.equ NULL_LIST, 0

	.globl INITIAL_REFCOUNT
	.equ   INITIAL_REFCOUNT, 0

	#OS-Specific
	.globl LINUX_SYSCALL
	.equ LINUX_SYSCALL, 0x80
	.globl SYSCALL_BRK
	.equ SYSCALL_BRK, 45
	.globl SYSCALL_WRITE
	.equ SYSCALL_WRITE, 4
	.globl SYSCALL_EXIT
	.equ SYSCALL_EXIT, 1
	.globl EXIT_STATUS_OK
	.equ EXIT_STATUS_OK, 0

	#POSIX
	.globl STDIN
	.equ STDIN, 0
	.globl STDOUT
	.equ STDOUT, 1
	.globl STDERR
	.equ STDERR, 2
