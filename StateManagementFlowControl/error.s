error_message_start:
	.ascii "Error occurred, exitting!\n"
error_message_end:
	.equ error_message_length, error_message_end - error_message_start

	.globl error_handler
	.type error_handler,@function
error_handler:
	movl  $STDOUT, %ebx
	movl  error_message_start, %ecx
	movl  error_message_length, %edx
	movl  $SYSCALL_WRITE, %eax
	int   $LINUX_SYSCALL
