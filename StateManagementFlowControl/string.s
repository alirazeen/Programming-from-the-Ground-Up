	#PURPOSE: Routines to handle usage of pairs
	#
	#
	#

	#Global Constants
	.globl STRING_TYPE
	.equ STRING_TYPE, 2

	.globl STRING_EQUAL
	.equ STRING_EQUAL, 0
	.globl STRING_LESS
	.equ STRING_LESS, 1
	.globl STRING_GREATER
	.equ STRING_GREATER, 2
	#end global constants

	#STRING STRUCTURE
	#HEADER#NUMBYTES#ACTUALDATA#
	.equ NUMBYTES_POSITION, BASE_SIZE
	.equ DATA_START_POSITION, BASE_SIZE + 4 

	.equ STRING_BASE_SIZE, BASE_SIZE + 4 #Padding for null is added later

	.section .data


	.section .text

	.equ ST_SIZE, 8
	.equ ST_DATA, 12
	.equ ST_STRING_STORE, -4

	.globl string_create
	.type string_create,@function
string_create:
	pushl %ebp
	movl  %esp, %ebp
	subl  $4, %esp

	#allocate string
	movl  ST_SIZE(%ebp), %ebx
	addl  $STRING_BASE_SIZE, %ebx
	incl  %ebx #Add one for a terminating NULL if needed
	           #This isn't included in the "official" size,
	           #but is useful for converting to C strings
	pushl %ebx
	call  allocate

	movl  %eax, ST_STRING_STORE(%ebp)

	#initialize string
	movl  $STRING_TYPE, TYPE_POSITION(%eax)
	movl  $INITIAL_REFCOUNT, REFCOUNT_POSITION(%eax)
	addl  $DATA_START_POSITION, %eax

	movl  $0, %edx
	movl  ST_DATA(%ebp), %ebx
str_copy_loop:
	cmpl  %edx, ST_SIZE(%ebp)
	jz    str_copy_loop_end
	movb  (%ebx,%edx,1), %cl
	movb  %cl, (%eax)
	incl  %eax
	incl  %edx
	jmp str_copy_loop

str_copy_loop_end:

	#NULL Pad
	movb  $0, (%eax)

	movl  ST_STRING_STORE(%ebp), %eax
	movl  ST_SIZE(%ebp), %ebx 
	movl  %ebx, NUMBYTES_POSITION(%eax)
	movl  %ebp, %esp
	popl  %ebp
	ret

	.equ ST_STRING, 8
	.equ ST_FILEDESCRIPTOR, 12

	.globl string_write
	.type string_write,@function
string_write:
	pushl %ebp
	movl  %esp, %ebp

	movl  ST_STRING(%ebp), %eax
	movl  ST_FILEDESCRIPTOR(%ebp), %ebx
	movl  %eax, %ecx
	addl  $DATA_START_POSITION, %ecx
	movl  NUMBYTES_POSITION(%eax), %edx
	movl  $SYSCALL_WRITE, %eax
	int   $LINUX_SYSCALL

	movl  %ebp, %esp
	popl  %ebp
	ret

	.equ ST_STRING1, 8
	.equ ST_STRING2, 12
	.globl string_compare
	.type  string_compare,@function
string_compare:
	pushl %ebp
	movl  %esp, %ebp

	#%eax is string 1
	#%ebx is string 2
	#%cl is char from string 1
	#%dl is char from string 2
	movl ST_STRING1(%ebp), %eax
	movl ST_STRING2(%ebp), %ebx

	#Put min of both numbers into %ecx
	movl NUMBYTES_POSITION(%eax), %ecx
	movl NUMBYTES_POSITION(%eax), %edx

	cmpl %ecx, %edx
	jc run_compare
	movl %edx, %ecx

run_compare:
	#put string start in %eax and %ebx
	addl $DATA_START_POSITION, %eax
	addl $DATA_START_POSITION, %ebx

	repe
	cmpsb (%eax), (%ebx)

	jz finish_equal
	jc finish_less
	
finish_greater:
	movl  $STRING_GREATER, %eax
	movl  %ebp, %esp
	popl  %ebp
	ret

finish_equal:
	movl  $STRING_EQUAL, %eax
	movl  %ebp, %esp
	popl  %ebp
	ret

finish_less:
	movl  $STRING_LESS, %eax
	movl  %ebp, %esp
	popl  %ebp
	ret
	

string_destroy:
	pushl %ebp
	movl  %esp, %ebp

	movl  ST_STRING(%ebp), %eax

	pushl %eax
	call deallocate

	movl  %ebp, %esp
	popl  %ebp
	ret
