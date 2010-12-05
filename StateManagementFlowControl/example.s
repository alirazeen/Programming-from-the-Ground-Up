	.equ NULL_LIST, 0

hello:
	.ascii "hello\n"
hello_end:
	.equ hello_len, hello_end - hello

	.globl _start

_start:
	call allocate_init

	pushl $hello
	pushl $hello_len
	call  string_create

	pushl $NULL_LIST
	pushl %eax

	call pair_create

	popl  %ebx
	pushl %eax
	pushl %ebx
	call  dereference
	popl  %ebx
	call pair_head

	pushl $STDOUT
	pushl %eax

	call  string_write

	popl  %eax
	popl  %eax
	call  dereference

	pushl $0
	pushl $0
	call  pair_create
	call  pair_create
	call  pair_create
	
	movl $0, %ebx
	movl $1, %eax
	int  $0x80

	
