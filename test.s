	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq .SStrConst0, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $0, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $42, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $17, %rax
	negq %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $2147483647, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst2, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $1, %rax
	cmpq $0, %rax
	jne L_1
	leaq .Sfalse, %rax
	jmp L_2
L_1:
	leaq .Strue, %rax
L_2:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $0, %rax
	cmpq $0, %rax
	jne L_3
	leaq .Sfalse, %rax
	jmp L_4
L_3:
	leaq .Strue, %rax
L_4:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $1, %rax
	cmpq $0, %rax
	jne L_5
	leaq .Sfalse, %rax
	jmp L_6
L_5:
	leaq .Strue, %rax
L_6:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst3, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $0, %rax
	cmpq $0, %rax
	jne L_7
	leaq .Sfalse, %rax
	jmp L_8
L_7:
	leaq .Strue, %rax
L_8:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst4, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst5, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst6, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst7, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst8, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst9, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst10, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst11, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $100, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst12, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $1, %rax
	cmpq $0, %rax
	jne L_9
	leaq .Sfalse, %rax
	jmp L_10
L_9:
	leaq .Strue, %rax
L_10:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $1, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst13, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $1, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst14, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $2, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $1, %rax
	cmpq $0, %rax
	jne L_11
	leaq .Sfalse, %rax
	jmp L_12
L_11:
	leaq .Strue, %rax
L_12:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst15, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $0, %rax
	cmpq $0, %rax
	jne L_13
	leaq .Sfalse, %rax
	jmp L_14
L_13:
	leaq .Strue, %rax
L_14:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst14, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $0, %rax
	cmpq $0, %rax
	jne L_15
	leaq .Sfalse, %rax
	jmp L_16
L_15:
	leaq .Strue, %rax
L_16:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst16, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst17, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst17, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst17, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst18, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $1, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	movq $2, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	movq $3, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	movq $4, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	movq $5, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $1, %rax
	cmpq $0, %rax
	jne L_17
	leaq .Sfalse, %rax
	jmp L_18
L_17:
	leaq .Strue, %rax
L_18:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $0, %rax
	cmpq $0, %rax
	jne L_19
	leaq .Sfalse, %rax
	jmp L_20
L_19:
	leaq .Strue, %rax
L_20:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $1, %rax
	cmpq $0, %rax
	jne L_21
	leaq .Sfalse, %rax
	jmp L_22
L_21:
	leaq .Strue, %rax
L_22:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $0, %rax
	cmpq $0, %rax
	jne L_23
	leaq .Sfalse, %rax
	jmp L_24
L_23:
	leaq .Strue, %rax
L_24:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst19, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst20, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst21, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst22, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst23, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	movq $0, %rax
	leaq .Sint, %rdi
	movq %rax, %rsi
	call printf_
	movq $0, %rax
	cmpq $0, %rax
	jne L_25
	leaq .Sfalse, %rax
	jmp L_26
L_25:
	leaq .Strue, %rax
L_26:
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst6, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst1, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leaq .SStrConst24, %rax
	leaq .Sstr, %rdi
	movq %rax, %rsi
	call printf_
	leave
	ret

# TODO some auxiliary assembly functions, if needed
malloc_:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    malloc
	movq    %rbp, %rsp
	popq    %rbp
	ret
calloc_:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    calloc
	movq    %rbp, %rsp
	popq    %rbp
	ret
printf_:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    printf
	movq    %rbp, %rsp
	popq    %rbp
	ret
	.data
.SStrConst4:
	.string "String tests:\n"
.SStrConst9:
	.string "Backslash:\\\n"
.SStrConst7:
	.string "Tab:\there\n"
.SStrConst11:
	.string "Number: "
.SStrConst21:
	.string "c"
.SStrConst2:
	.string "Boolean tests:\n"
.SStrConst0:
	.string "Integer tests:\n"
.SStrConst18:
	.string "Multi-arg test:\n"
.SStrConst12:
	.string "Bool: "
.SStrConst16:
	.string "Reuse test:\n"
.SStrConst5:
	.string "Hello, World!\n"
.SStrConst13:
	.string " + "
.SStrConst14:
	.string " = "
.SStrConst1:
	.string "\n"
.SStrConst24:
	.string "\n\n\n"
.SStrConst3:
	.string " "
.SStrConst17:
	.string "same\n"
.SStrConst20:
	.string "b"
.SStrConst10:
	.string "Mixed tests:\n"
.SStrConst22:
	.string "d"
.SStrConst23:
	.string "Edge cases:\n"
.SStrConst15:
	.string " and "
.SStrConst19:
	.string "a"
.SStrConst6:
	.string ""
.SStrConst8:
	.string "Quote:\"test\"\n"
.Sint:
	.string "%d"
.Sstr:
	.string "%s"
.Strue:
	.string "true"
.Sfalse:
	.string "false"
.Snil:
	.string "<nil>"
