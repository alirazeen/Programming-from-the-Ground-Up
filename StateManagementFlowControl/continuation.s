	.equ CONTINUATION_TYPE, 14

	.equ PROC_POSITION, BASE_SIZE + 4
	.equ ENVIRONMENT_POSITION, BASE_SIZE + 8
	.equ NEXT_CONT_POSITION, BASE_SIZE + 12
	.equ NEXT_EXPRESSION_POSITION, BASE_SIZE + 16
	.equ ERROR_CONT_POSITION, BASE_SIZE + 20

	.equ CONTINUATION_SIZE, BASE_SIZE + 24

	.equ ST_PROC, 8
	.equ ST_ENV, 12
	.equ ST_PREV_CONT, 16
	.equ ST_NEXT_EXP, 20
	.equ ST_ERROR_CONT, 24
continuation_create:
	pushl %ebp
	movl  %esp, %ebp

	pushl $CONTINUATION_SIZE
	call  allocate

	#initialize continuation
	#fixme - probably need to mess with references
	movl  $CONTINUATION_TYPE, TYPE_POSITION(%eax)
	movl  $INITIAL_REFCOUNT, REFCOUNT_POSITION(%eax)
	movl  ST_PROC(%ebp), %ebx
	movl  ST_ENV(%ebp), %ecx
	movl  ST_PREV_CONT(%ebp), %edx
	movl  %ebx, PROC_POSITION(%eax)
	movl  %ecx, ENVIRONMENT_POSITION(%eax)
	movl  %edx, NEXT_CONT_POSITION(%eax)
	movl  ST_NEXT_EXP(%ebp), %ebx
	movl  %ebx, NEXT_EXPRESSION_POSITION(%eax)

	#reference environment and continuation
	pushl %eax
	pushl %ecx
	pushl %edx
	call  reference
	popl  %edx
	call  reference
	popl  %ecx
	popl  %eax

	#handle error continuation
	movl  ST_ERROR_CONT(%ebp), %ebx
	movl  %ebx, ERROR_CONT_POSITION(%eax)

	pushl %eax
	pushl %ebx
	call  reference
	popl  %ebx
	popl  %eax

	movl  %ebp, %esp
	popl  %ebp
	ret

continuation_destroy:
	pushl %ebp
	movl  %esp, %ebp

	movl  ST_CONT(%ebp), %eax
	movl  ENVIRONMENT_POSITION(%eax), %ebx
	movl  NEXT_CONT_POSITION(%eax), %ecx
	movl  ERROR_CONT_POSITION(%eax), %edx

	pushl %eax
	pushl %ebx
	pushl %ecx
	pushl %edx
	call  dereference
	popl  %edx
	call  dereference
	popl  %ecx
	call  dereference
	popl  %ebx
	call  deallocate

	movl  %ebp, %esp
	popl  %ebp
	ret

	
