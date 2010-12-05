
.section .data
if_symbol:
	.long SYMBOL_TYPE
	.long 1
	.long NULL
	.long 2
	.ascii "if\0"

current_continuation:
	.long 0

current_expression:
	.long 0

return_value:
	.long 0

.section .text
eval_expression:
	movl current_exp, %eax
	movl TYPE_POSITION(%eax), %ebx
	cmpl $SYMBOL_TYPE, %ebx
	jel  eval_symbol

	cmpl $PAIR_TYPE, %ebx
	jel  eval_procedure

eval_literal:
	movl current_continuation, %eax
	movl PROC_POSITION(%eax), %ecx
	jmpl *%ecx

eval_symbol:
	cmpl %eax, $if_symbol
	jel  eval_if

	cmpl %eax, $cond_symbol
	jel  eval_cond

	cmpl %eax, $lambda_symbol
	jel  eval_lambda

	cmpl %eax, $define_symbol
	jel  eval_define

	cmpl %eax, $let_symbol
	jel  eval_let

	cmpl %eax, $letstar_symbol
	jel  eval_letstar

	cmpl %eax, $letrec_symbol
	jel  eval_letrec

	cmpl %eax, $quote_symbol
	jel  eval_quote

	cmpl %eax, %quasiquote_symbol
	jel  eval_quasiquote

eval_nonform_symbol:
	pushl $current_environment
	pushl %eax
	call  lookup_symbol

	movl %eax, %ebx

	movl  current_continuation, %eax
	jmpl  *%eax

eval_procedure:
eval_proc_operator:
	movl  current_continuation, %eax
	pushl $eval_proc_operands
	pushl ENVIRONMENT_POSITION(%eax)
	pushl %eax
	pushl NEXT_EXPRESSION_POSITION(%eax)
	call  continuation_create
	movl  %eax, current_continuation
	popl  %eax
	popl  %eax
	popl  %eax
	popl  %eax

	jmpl  eval_expression
	#eval operator
eval_proc_operands:
	#eval operands into list
eval_proc_apply:
	#apply operator to operands
