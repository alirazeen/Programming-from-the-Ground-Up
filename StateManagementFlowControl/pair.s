	#PURPOSE: Routines to handle usage of pairs
	#
	#
	#

	#GLOBAL CONSTANTS
	.globl PAIR_TYPE
	.equ PAIR_TYPE, 1
	#End Global

	#PAIR STRUCTURE
	.equ HEAD_POSITION, BASE_SIZE
	.equ TAIL_POSITION, BASE_SIZE + 4

	.equ PAIR_SIZE, BASE_SIZE + 8

	.section .data


	.section .text

	.equ ST_HEAD, 8
	.equ ST_TAIL, 12

	#NOTE - same as cons
	.globl pair_create
	.type pair_create,@function
pair_create:
	pushl %ebp
	movl  %esp, %ebp

	pushl $PAIR_SIZE
	call allocate

	#initialize pair
	movl  $PAIR_TYPE, TYPE_POSITION(%eax)
	movl  $INITIAL_REFCOUNT, REFCOUNT_POSITION(%eax)

	#Initialize head after adding ref
	movl  ST_HEAD(%ebp), %ebx
	pushl %eax
	pushl %ebx
	call  reference
	popl  %ebx
	popl  %eax
	movl  %ebx, HEAD_POSITION(%eax)

	#Initialize tail after adding ref
	mov   ST_TAIL(%ebp), %ecx
	pushl %eax
	pushl %ecx
	call  reference
	popl  %ecx
	popl  %eax
	movl  %ecx, TAIL_POSITION(%eax)

	movl  %ebp, %esp
	popl  %ebp
	ret


	.equ ST_PAIR_ADDR, 8
	.globl pair_head
	.type pair_head,@function
pair_head:
	pushl %ebp
	movl  %esp, %ebp

	movl  ST_PAIR_ADDR(%ebp), %ebx
	movl  HEAD_POSITION(%ebx), %eax

	movl  %ebp, %esp
	popl  %ebp
	ret

	.globl pair_tail
	.type pair_tail,@function
pair_tail:
	pushl %ebp
	movl  %esp, %ebp

	movl  ST_PAIR_ADDR(%ebp), %ebx
	movl  TAIL_POSITION(%ebx), %eax

	movl  %ebp, %esp
	popl  %ebp
	ret

	.globl pair_destroy
	.type pair_destroy,@function

	.equ ST_TAIL_ADDR, 12
	.globl pair_set_tail
	.type pair_set_tail,@function
pair_set_tail:
	pushl %ebp
	movl  %esp, %ebp

	movl  ST_PAIR_ADDR(%ebp), %ebx
	pushl TAIL_POSITION(%ebx)

	#FIXME - this should probably be reordered so that
	#        the list is never in a bad state
	call  dereference

	pushl ST_TAIL_ADDR(%ebp)
	
	call  reference

	popl  %eax

	movl  ST_PAIR_ADDR(%ebp), %ebx
	movl  %eax, TAIL_POSITION(%ebx)

	movl  %ebp, %esp
	popl  %ebp
	ret

pair_destroy:
	pushl %ebp
	movl  %esp, %ebp

	movl  ST_PAIR_ADDR(%ebp), %eax
	pushl %eax
	pushl HEAD_POSITION(%eax)
	call  dereference
	popl  %ebx
	popl  %eax
	pushl %eax
	pushl TAIL_POSITION(%eax)
	call  dereference
	popl  %eax  #stack now points to PAIR
	call  deallocate

	movl  %ebp, %esp
	popl  %ebp
	ret
