	#PURPOSE: Routines to handle usage of integers
	#
	#
	#

	#Global Constants
	.globl INTEGER_TYPE
	.equ INTEGER_TYPE, 3
	#End global
	

	#INTEGER STRUCTURE
	#TYPE#REFCOUNT#NUMBER#
	.equ NUMBER_POSITION, BASE_SIZE

	.equ INTEGER_SIZE, BASE_SIZE + 4

	.section .data


	.section .text

	.equ ST_INTEGER, 8

	.globl integer_create
	.type integer_create,@function
integer_create:
	pushl %ebp
	movl  %esp, %ebp
	subl  $4, %esp

	#allocate integer
	pushl $INTEGER_SIZE
	call  allocate

	movl  $INTEGER_TYPE, TYPE_POSITION(%eax)
	movl  $INITIAL_REFCOUNT, REFCOUNT_POSITION(%eax)
	movl  ST_INTEGER(%ebp), %ebx
	movl  %ebx, NUMBER_POSITION(%eax)

	movl  %ebp, %esp
	popl  %ebp
	ret

	.globl integer_add
	.type integer_add,@function
integer_add:
	pushl %ebp
	movl  %esp, %ebp
	
	movl  ST_INTEGER1(%ebp), %eax
	movl  ST_INTEGER2(%ebp), %ebx
	addl  %ebx, %eax

	movl  %ebp, %esp
	popl  %ebp
	ret

integer_destroy:
	pushl %ebp
	movl  %esp, %ebp

	movl  ST_INTEGER(%ebp), %eax

	pushl %eax
	call deallocate

	movl  %ebp, %esp
	popl  %ebp
	ret
