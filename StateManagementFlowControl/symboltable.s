	#A symbol table just uses pairs, so it doesn't have it's own type

	.equ NULL_LIST, 0

	.equ ST_SYMBOL, 12
	.equ ST_SYMBOLTABLE, 8
	.globl symboltable_search
	.type  symboltable_search,@function
symboltable_search:
	pushl %ebp
	movl  %esp, %ebp

search_entrance:
	movl  ST_SYMBOLTABLE(%ebp), %ebx

	#Are we at the end?
	cmpl  $0, %ebx
	jz    symbol_not_found

	#Nope.  Assume that the head of the list is a simple pair
	pushl %ebx
	call  pair_head

	#%eax has the pair we are looking at
	pushl %eax
	call  pair_head

	#%eax has the symbol we are comparing
	pushl %eax
	pushl ST_SYMBOL(%ebp)
	call  string_compare
	cmpl  $STRING_EQUAL, %eax
	jnz   keep_searching

	#Pop the symbols
	popl  %ebx
	popl  %ecx
	#The next entry in the stack is the pair in question.
	#return it

	#FIXME - should I add a reference?

	popl  %eax
	#value is in %eax, return
	
	movl  %ebp, %esp
	popl  %ebp
	ret

symbol_not_found:
	movl  $NULL_LIST, %eax
	movl  %ebp, %esp
	popl  %ebp
	ret

keep_searching:
	#PROJECT - convert to tail-recursive function
	pushl  ST_SYMBOLTABLE(%ebp)
	call   pair_tail
	pushl  %eax
	call   symboltable_search
	
	movl   %ebp, %esp
	popl   %ebp
	ret

	#NOTE - this returns a new symboltable each time

	.equ ST_SYMBOLTABLE, 8
	.equ ST_SYMBOL, 12
	.equ ST_VALUE, 16

	.globl symboltable_add_symbol
	.type  symboltable_add_symbol,@function
symboltable_add_symbol:
	pushl %ebp
	movl  %esp, %ebp

	pushl ST_VALUE(%ebp)
	pushl ST_SYMBOL(%ebp)
	call  pair_create

	pushl ST_SYMBOLTABLE(%ebp)
	pushl %eax
	call pair_create

	movl  %ebp, %esp
	popl  %ebp
	ret

	.globl symboltable_set_symbol
	.type  symboltable_set_symbol,@function
symboltable_set_symbol:
	pushl %ebp
	movl  %esp, %ebp

	pushl ST_SYMBOL(%ebp)
	pushl ST_SYMBOLTABLE(%ebp)
	call  symboltable_search

	cmpl  $NULL_LIST, %eax
	jz error_handler

	pushl ST_VALUE(%ebp)
	pushl %eax

	call  pair_set_tail

	movl  %esp, %ebp
	popl  %ebp
	ret
