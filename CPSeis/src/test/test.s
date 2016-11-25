	.file	"test.f90"
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.text
.Ltext0:
	.section	.rodata
.LC0:
	.string	"test.f90"
	.text
.globl test_module_MP_square
	.type	test_module_MP_square, @function
test_module_MP_square:
.LFB2:
	.file 1 "test.f90"
	.loc 1 9 0
	pushq	%rbp
.LCFI0:
	movq	%rsp, %rbp
.LCFI1:
	subq	$8, %rsp
.LCFI2:
	movq	%rdi, -104(%rbp)
	movq	%rsi, -112(%rbp)
.LBB2:
	.loc 1 9 0
	movl	$1, -68(%rbp)
	movq	-112(%rbp), %rax
	movl	(%rax), %eax
	movl	%eax, -64(%rbp)
	movq	-104(%rbp), %rax
	movq	%rax, -80(%rbp)
	movl	$4, -72(%rbp)
	movl	$1, -88(%rbp)
	movl	$4, -84(%rbp)
	movq	-80(%rbp), %rax
	subq	$4, %rax
	movq	%rax, -96(%rbp)
	movl	$0, -52(%rbp)
	movq	_g95_filename(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	_g95_line(%rip), %eax
	movl	%eax, -32(%rbp)
	movq	_g95_base(%rip), %rax
	movq	%rax, -40(%rbp)
	leaq	-48(%rbp), %rax
	movq	%rax, _g95_base(%rip)
	movq	$.LC0, _g95_filename(%rip)
	movl	$16, _g95_line(%rip)
	.loc 1 16 0
	movq	-112(%rbp), %rax
	movl	(%rax), %eax
	movl	%eax, -12(%rbp)
	movl	-12(%rbp), %eax
	movl	%eax, -116(%rbp)
	cmpl	$0, -116(%rbp)
	jns	.L2
	movl	$0, -116(%rbp)
.L2:
	movl	-116(%rbp), %eax
	movl	%eax, -8(%rbp)
	movl	$1, -4(%rbp)
.L3:
	movl	-8(%rbp), %eax
	testl	%eax, %eax
	je	.L4
	movq	$.LC0, _g95_filename(%rip)
	movl	$18, _g95_line(%rip)
	.loc 1 18 0
	movl	-4(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movq	-96(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, %rcx
	movl	-4(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movq	-96(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	movss	(%rax), %xmm1
	movl	-4(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movq	-96(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	movss	(%rax), %xmm0
	mulss	%xmm1, %xmm0
	movss	%xmm0, (%rcx)
	.loc 1 16 0
	leaq	-4(%rbp), %rax
	incl	(%rax)
	movl	-8(%rbp), %eax
	decl	%eax
	movl	%eax, -8(%rbp)
	jmp	.L3
.L4:
	.loc 1 9 0
	movq	-40(%rbp), %rax
	movq	%rax, _g95_base(%rip)
	movl	-52(%rbp), %eax
.LBE2:
	.loc 1 9 0
	leave
	ret
.LFE2:
	.size	test_module_MP_square, .-test_module_MP_square
.globl square_root_
	.type	square_root_, @function
square_root_:
.LFB3:
	.loc 1 26 0
	pushq	%rbp
.LCFI3:
	movq	%rsp, %rbp
.LCFI4:
	subq	$8, %rsp
.LCFI5:
	movq	%rdi, -104(%rbp)
	movq	%rsi, -112(%rbp)
.LBB3:
	.loc 1 26 0
	movl	$1, -68(%rbp)
	movq	-112(%rbp), %rax
	movl	(%rax), %eax
	movl	%eax, -64(%rbp)
	movq	-104(%rbp), %rax
	movq	%rax, -80(%rbp)
	movl	$4, -72(%rbp)
	movl	$1, -88(%rbp)
	movl	$4, -84(%rbp)
	movq	-80(%rbp), %rax
	subq	$4, %rax
	movq	%rax, -96(%rbp)
	movl	$0, -52(%rbp)
	movq	_g95_filename(%rip), %rax
	movq	%rax, -48(%rbp)
	movl	_g95_line(%rip), %eax
	movl	%eax, -32(%rbp)
	movq	_g95_base(%rip), %rax
	movq	%rax, -40(%rbp)
	leaq	-48(%rbp), %rax
	movq	%rax, _g95_base(%rip)
	movq	$.LC0, _g95_filename(%rip)
	movl	$31, _g95_line(%rip)
	.loc 1 31 0
	movq	-112(%rbp), %rax
	movl	(%rax), %eax
	movl	%eax, -12(%rbp)
	movl	-12(%rbp), %eax
	movl	%eax, -116(%rbp)
	cmpl	$0, -116(%rbp)
	jns	.L8
	movl	$0, -116(%rbp)
.L8:
	movl	-116(%rbp), %eax
	movl	%eax, -8(%rbp)
	movl	$1, -4(%rbp)
.L9:
	movl	-8(%rbp), %eax
	testl	%eax, %eax
	je	.L10
	movq	$.LC0, _g95_filename(%rip)
	movl	$32, _g95_line(%rip)
	.loc 1 32 0
	movl	-4(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movq	-96(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	movq	%rax, %rcx
	movl	-4(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movq	-96(%rbp), %rax
	leaq	(%rdx,%rax), %rax
	movss	(%rax), %xmm1
	cvtsi2ss	-4(%rbp), %xmm0
	movaps	%xmm1, %xmm2
	divss	%xmm0, %xmm2
	movaps	%xmm2, %xmm0
	movss	%xmm0, (%rcx)
	.loc 1 31 0
	leaq	-4(%rbp), %rax
	incl	(%rax)
	movl	-8(%rbp), %eax
	decl	%eax
	movl	%eax, -8(%rbp)
	jmp	.L9
.L10:
	.loc 1 26 0
	movq	-40(%rbp), %rax
	movq	%rax, _g95_base(%rip)
	movl	-52(%rbp), %eax
.LBE3:
	leave
	ret
.LFE3:
	.size	square_root_, .-square_root_
	.data
	.align 4
	.type	n.554, @object
	.size	n.554, 4
n.554:
	.long	10
	.text
.globl MAIN_
	.type	MAIN_, @function
MAIN_:
.LFB4:
	.loc 1 37 0
	pushq	%rbp
.LCFI6:
	movq	%rsp, %rbp
.LCFI7:
	addq	$-128, %rsp
.LCFI8:
.LBB4:
	.loc 1 37 0
	movl	$1, -84(%rbp)
	movl	$10, -80(%rbp)
	movl	$4, -88(%rbp)
	movl	$1, -104(%rbp)
	movl	$4, -100(%rbp)
	leaq	-64(%rbp), %rax
	movq	%rax, -96(%rbp)
	leaq	-64(%rbp), %rax
	subq	$4, %rax
	movq	%rax, -112(%rbp)
	movl	$0, -24(%rbp)
	movq	$.LC0, _g95_filename(%rip)
	movl	$47, _g95_line(%rip)
	movq	$.LC0, _g95_filename(%rip)
	movl	$47, _g95_line(%rip)
	.loc 1 47 0
	movl	n.554(%rip), %eax
	movl	%eax, -20(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, -116(%rbp)
	cmpl	$0, -116(%rbp)
	jns	.L14
	movl	$0, -116(%rbp)
.L14:
	movl	-116(%rbp), %eax
	movl	%eax, -16(%rbp)
	movl	$1, -4(%rbp)
.L15:
	movl	-16(%rbp), %eax
	testl	%eax, %eax
	je	.L16
	movq	$.LC0, _g95_filename(%rip)
	movl	$48, _g95_line(%rip)
	.loc 1 48 0
	movl	-4(%rbp), %eax
	decl	%eax
	cvtsi2ss	-4(%rbp), %xmm0
	cltq
	movss	%xmm0, -64(%rbp,%rax,4)
	.loc 1 47 0
	leaq	-4(%rbp), %rax
	incl	(%rax)
	movl	-16(%rbp), %eax
	decl	%eax
	movl	%eax, -16(%rbp)
	jmp	.L15
.L16:
	.loc 1 37 0
	movq	$.LC0, _g95_filename(%rip)
	movl	$51, _g95_line(%rip)
	.loc 1 51 0
	leaq	-64(%rbp), %rdi
	movl	$n.554, %esi
	call	test_module_MP_square
	.loc 1 53 0
	call	_g95_get_ioparm
	movq	$.LC0, _g95_filename(%rip)
	movl	$53, _g95_line(%rip)
	movl	$6, -12(%rbp)
	movq	_g95_ioparm(%rip), %rdx
	leaq	-12(%rbp), %rax
	movq	%rax, (%rdx)
	movq	_g95_ioparm(%rip), %rax
	movl	$4, 8(%rax)
	movq	_g95_ioparm(%rip), %rax
	movl	$1, 24(%rax)
	call	_g95_st_write
	leaq	-112(%rbp), %rdi
	movl	$4, %esi
	call	_g95_transfer_real_array
	call	_g95_st_write_done
	.loc 1 37 0
	movq	$.LC0, _g95_filename(%rip)
	movl	$55, _g95_line(%rip)
	.loc 1 55 0
	leaq	-64(%rbp), %rdi
	movl	$n.554, %esi
	call	square_root_
	.loc 1 57 0
	call	_g95_get_ioparm
	movq	$.LC0, _g95_filename(%rip)
	movl	$57, _g95_line(%rip)
	movl	$6, -8(%rbp)
	movq	_g95_ioparm(%rip), %rdx
	leaq	-8(%rbp), %rax
	movq	%rax, (%rdx)
	movq	_g95_ioparm(%rip), %rax
	movl	$4, 8(%rax)
	movq	_g95_ioparm(%rip), %rax
	movl	$1, 24(%rax)
	call	_g95_st_write
	leaq	-112(%rbp), %rdi
	movl	$4, %esi
	call	_g95_transfer_real_array
	call	_g95_st_write_done
	.loc 1 37 0
	movl	-24(%rbp), %eax
.LBE4:
	leave
	ret
.LFE4:
	.size	MAIN_, .-MAIN_
	.section	.debug_frame,"",@progbits
.Lframe0:
	.long	.LECIE0-.LSCIE0
.LSCIE0:
	.long	0xffffffff
	.byte	0x1
	.string	""
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.align 8
.LECIE0:
.LSFDE0:
	.long	.LEFDE0-.LASFDE0
.LASFDE0:
	.long	.Lframe0
	.quad	.LFB2
	.quad	.LFE2-.LFB2
	.byte	0x4
	.long	.LCFI0-.LFB2
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI1-.LCFI0
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE0:
.LSFDE2:
	.long	.LEFDE2-.LASFDE2
.LASFDE2:
	.long	.Lframe0
	.quad	.LFB3
	.quad	.LFE3-.LFB3
	.byte	0x4
	.long	.LCFI3-.LFB3
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI4-.LCFI3
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE2:
.LSFDE4:
	.long	.LEFDE4-.LASFDE4
.LASFDE4:
	.long	.Lframe0
	.quad	.LFB4
	.quad	.LFE4-.LFB4
	.byte	0x4
	.long	.LCFI6-.LFB4
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI7-.LCFI6
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE4:
	.section	.eh_frame,"a",@progbits
.Lframe1:
	.long	.LECIE1-.LSCIE1
.LSCIE1:
	.long	0x0
	.byte	0x1
	.string	""
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.align 8
.LECIE1:
.LSFDE1:
	.long	.LEFDE1-.LASFDE1
.LASFDE1:
	.long	.LASFDE1-.Lframe1
	.quad	.LFB2
	.quad	.LFE2-.LFB2
	.byte	0x4
	.long	.LCFI0-.LFB2
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI1-.LCFI0
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE1:
.LSFDE3:
	.long	.LEFDE3-.LASFDE3
.LASFDE3:
	.long	.LASFDE3-.Lframe1
	.quad	.LFB3
	.quad	.LFE3-.LFB3
	.byte	0x4
	.long	.LCFI3-.LFB3
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI4-.LCFI3
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE3:
.LSFDE5:
	.long	.LEFDE5-.LASFDE5
.LASFDE5:
	.long	.LASFDE5-.Lframe1
	.quad	.LFB4
	.quad	.LFE4-.LFB4
	.byte	0x4
	.long	.LCFI6-.LFB4
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI7-.LCFI6
	.byte	0xd
	.uleb128 0x6
	.align 8
.LEFDE5:
	.text
.Letext0:
	.section	.debug_info
	.long	0x3aa
	.value	0x2
	.long	.Ldebug_abbrev0
	.byte	0x8
	.uleb128 0x1
	.long	.Ldebug_line0
	.quad	.Letext0
	.quad	.Ltext0
	.string	"G95 Fortran 95 4.0.3 (g95 0.90!) Jul 27 2006"
	.byte	0x1
	.string	"test.f90"
	.string	"/home/mengewm/workspace/cpseis/src/test"
	.uleb128 0x2
	.long	0x115
	.byte	0x1
	.string	"test_module_MP_square"
	.byte	0x1
	.byte	0x9
	.byte	0x1
	.long	0x115
	.quad	.LFB2
	.quad	.LFE2
	.byte	0x1
	.byte	0x56
	.uleb128 0x3
	.string	"x"
	.byte	0x1
	.byte	0x9
	.long	0x11d
	.byte	0x3
	.byte	0x76
	.sleb128 -104
	.uleb128 0x3
	.string	"n"
	.byte	0x1
	.byte	0x9
	.long	0x131
	.byte	0x3
	.byte	0x76
	.sleb128 -112
	.uleb128 0x4
	.string	"U3"
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -8
	.uleb128 0x4
	.string	"U2"
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -12
	.uleb128 0x4
	.string	"U1"
	.long	0x13c
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -48
	.uleb128 0x5
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -52
	.uleb128 0x4
	.string	"U0"
	.long	0x186
	.byte	0x1
	.byte	0x3
	.byte	0x76
	.sleb128 -96
	.uleb128 0x6
	.string	"i"
	.byte	0x1
	.byte	0x9
	.long	0x115
	.byte	0x2
	.byte	0x76
	.sleb128 -4
	.byte	0x0
	.uleb128 0x7
	.string	"int4"
	.byte	0x4
	.byte	0x5
	.uleb128 0x8
	.long	0x122
	.uleb128 0x9
	.byte	0x8
	.long	0x128
	.uleb128 0x7
	.string	"real4"
	.byte	0x4
	.byte	0x4
	.uleb128 0x8
	.long	0x136
	.uleb128 0x9
	.byte	0x8
	.long	0x115
	.uleb128 0xa
	.long	0x176
	.byte	0x18
	.byte	0x1
	.byte	0x0
	.uleb128 0xb
	.string	"filename"
	.byte	0x1
	.byte	0x0
	.long	0x176
	.byte	0x2
	.byte	0x23
	.uleb128 0x0
	.uleb128 0xb
	.string	"next"
	.byte	0x1
	.byte	0x0
	.long	0x184
	.byte	0x2
	.byte	0x23
	.uleb128 0x8
	.uleb128 0xb
	.string	"line"
	.byte	0x1
	.byte	0x0
	.long	0x115
	.byte	0x2
	.byte	0x23
	.uleb128 0x10
	.byte	0x0
	.uleb128 0x9
	.byte	0x8
	.long	0x17c
	.uleb128 0x7
	.string	"int1"
	.byte	0x1
	.byte	0x5
	.uleb128 0xc
	.byte	0x8
	.uleb128 0xd
	.long	0x1e4
	.string	"array1"
	.byte	0x28
	.byte	0x1
	.byte	0x0
	.uleb128 0xb
	.string	"offset"
	.byte	0x1
	.byte	0x0
	.long	0x176
	.byte	0x2
	.byte	0x23
	.uleb128 0x0
	.uleb128 0xb
	.string	"rank"
	.byte	0x1
	.byte	0x0
	.long	0x115
	.byte	0x2
	.byte	0x23
	.uleb128 0x8
	.uleb128 0xb
	.string	"esize"
	.byte	0x1
	.byte	0x0
	.long	0x115
	.byte	0x2
	.byte	0x23
	.uleb128 0xc
	.uleb128 0xb
	.string	"base"
	.byte	0x1
	.byte	0x0
	.long	0x184
	.byte	0x2
	.byte	0x23
	.uleb128 0x10
	.uleb128 0xb
	.string	"info"
	.byte	0x1
	.byte	0x0
	.long	0x1e4
	.byte	0x2
	.byte	0x23
	.uleb128 0x18
	.byte	0x0
	.uleb128 0xe
	.long	0x1f4
	.long	0x115
	.uleb128 0xf
	.long	0x115
	.byte	0x2
	.byte	0x0
	.uleb128 0x2
	.long	0x281
	.byte	0x1
	.string	"square_root_"
	.byte	0x1
	.byte	0x1a
	.byte	0x1
	.long	0x115
	.quad	.LFB3
	.quad	.LFE3
	.byte	0x1
	.byte	0x56
	.uleb128 0x3
	.string	"x"
	.byte	0x1
	.byte	0x1a
	.long	0x281
	.byte	0x3
	.byte	0x76
	.sleb128 -104
	.uleb128 0x3
	.string	"n"
	.byte	0x1
	.byte	0x1a
	.long	0x28c
	.byte	0x3
	.byte	0x76
	.sleb128 -112
	.uleb128 0x4
	.string	"U9"
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -8
	.uleb128 0x4
	.string	"U8"
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -12
	.uleb128 0x4
	.string	"U7"
	.long	0x13c
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -48
	.uleb128 0x5
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -52
	.uleb128 0x6
	.string	"i"
	.byte	0x1
	.byte	0x1a
	.long	0x115
	.byte	0x2
	.byte	0x76
	.sleb128 -4
	.uleb128 0x4
	.string	"U6"
	.long	0x186
	.byte	0x1
	.byte	0x3
	.byte	0x76
	.sleb128 -96
	.byte	0x0
	.uleb128 0x8
	.long	0x286
	.uleb128 0x9
	.byte	0x8
	.long	0x128
	.uleb128 0x8
	.long	0x136
	.uleb128 0x2
	.long	0x39f
	.byte	0x1
	.string	"MAIN_"
	.byte	0x1
	.byte	0x25
	.byte	0x1
	.long	0x115
	.quad	.LFB4
	.quad	.LFE4
	.byte	0x1
	.byte	0x56
	.uleb128 0x10
	.byte	0x1
	.long	.LASF1
	.byte	0x1
	.byte	0x39
	.byte	0x1
	.byte	0x1
	.uleb128 0x11
	.long	0x2d9
	.byte	0x1
	.long	.LASF0
	.byte	0x1
	.byte	0x39
	.byte	0x1
	.byte	0x1
	.uleb128 0x12
	.long	0x39f
	.uleb128 0x12
	.long	0x115
	.byte	0x0
	.uleb128 0x10
	.byte	0x1
	.long	.LASF2
	.byte	0x1
	.byte	0x39
	.byte	0x1
	.byte	0x1
	.uleb128 0x4
	.string	"U17"
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -8
	.uleb128 0x10
	.byte	0x1
	.long	.LASF3
	.byte	0x1
	.byte	0x39
	.byte	0x1
	.byte	0x1
	.uleb128 0x10
	.byte	0x1
	.long	.LASF1
	.byte	0x1
	.byte	0x35
	.byte	0x1
	.byte	0x1
	.uleb128 0x11
	.long	0x31d
	.byte	0x1
	.long	.LASF0
	.byte	0x1
	.byte	0x35
	.byte	0x1
	.byte	0x1
	.uleb128 0x12
	.long	0x39f
	.uleb128 0x12
	.long	0x115
	.byte	0x0
	.uleb128 0x10
	.byte	0x1
	.long	.LASF2
	.byte	0x1
	.byte	0x35
	.byte	0x1
	.byte	0x1
	.uleb128 0x4
	.string	"U16"
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -12
	.uleb128 0x10
	.byte	0x1
	.long	.LASF3
	.byte	0x1
	.byte	0x35
	.byte	0x1
	.byte	0x1
	.uleb128 0x4
	.string	"U13"
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -16
	.uleb128 0x4
	.string	"U12"
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -20
	.uleb128 0x5
	.long	0x115
	.byte	0x1
	.byte	0x2
	.byte	0x76
	.sleb128 -24
	.uleb128 0x6
	.string	"x.data"
	.byte	0x1
	.byte	0x25
	.long	0x3a5
	.byte	0x2
	.byte	0x76
	.sleb128 -64
	.uleb128 0x6
	.string	"x"
	.byte	0x1
	.byte	0x25
	.long	0x186
	.byte	0x3
	.byte	0x76
	.sleb128 -112
	.uleb128 0x6
	.string	"i"
	.byte	0x1
	.byte	0x25
	.long	0x115
	.byte	0x2
	.byte	0x76
	.sleb128 -4
	.uleb128 0x6
	.string	"n"
	.byte	0x1
	.byte	0x25
	.long	0x115
	.byte	0x9
	.byte	0x3
	.quad	n.554
	.byte	0x0
	.uleb128 0x9
	.byte	0x8
	.long	0x186
	.uleb128 0x13
	.long	0x128
	.uleb128 0x14
	.byte	0x9
	.byte	0x0
	.byte	0x0
	.section	.debug_abbrev
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x10
	.uleb128 0x6
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x25
	.uleb128 0x8
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x1b
	.uleb128 0x8
	.byte	0x0
	.byte	0x0
	.uleb128 0x2
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x1
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0xc
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x5
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x4
	.uleb128 0x34
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0xc
	.uleb128 0x2
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x5
	.uleb128 0x34
	.byte	0x0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0xc
	.uleb128 0x2
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x6
	.uleb128 0x34
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x7
	.uleb128 0x24
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.byte	0x0
	.byte	0x0
	.uleb128 0x8
	.uleb128 0x26
	.byte	0x0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.uleb128 0x9
	.uleb128 0xf
	.byte	0x0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.uleb128 0xa
	.uleb128 0x13
	.byte	0x1
	.uleb128 0x1
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.byte	0x0
	.byte	0x0
	.uleb128 0xb
	.uleb128 0xd
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0xc
	.uleb128 0xf
	.byte	0x0
	.uleb128 0xb
	.uleb128 0xb
	.byte	0x0
	.byte	0x0
	.uleb128 0xd
	.uleb128 0x13
	.byte	0x1
	.uleb128 0x1
	.uleb128 0x13
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.byte	0x0
	.byte	0x0
	.uleb128 0xe
	.uleb128 0x1
	.byte	0x1
	.uleb128 0x1
	.uleb128 0x13
	.uleb128 0x49
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.uleb128 0xf
	.uleb128 0x21
	.byte	0x0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xb
	.byte	0x0
	.byte	0x0
	.uleb128 0x10
	.uleb128 0x2e
	.byte	0x0
	.uleb128 0x3f
	.uleb128 0xc
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x3c
	.uleb128 0xc
	.byte	0x0
	.byte	0x0
	.uleb128 0x11
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x1
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0xc
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x3c
	.uleb128 0xc
	.byte	0x0
	.byte	0x0
	.uleb128 0x12
	.uleb128 0x5
	.byte	0x0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.uleb128 0x13
	.uleb128 0x1
	.byte	0x1
	.uleb128 0x49
	.uleb128 0x13
	.byte	0x0
	.byte	0x0
	.uleb128 0x14
	.uleb128 0x21
	.byte	0x0
	.uleb128 0x2f
	.uleb128 0xb
	.byte	0x0
	.byte	0x0
	.byte	0x0
	.section	.debug_pubnames,"",@progbits
	.long	0x43
	.value	0x2
	.long	.Ldebug_info0
	.long	0x3ae
	.long	0x7f
	.string	"test_module_MP_square"
	.long	0x1f4
	.string	"square_root_"
	.long	0x291
	.string	"MAIN_"
	.long	0x0
	.section	.debug_aranges,"",@progbits
	.long	0x2c
	.value	0x2
	.long	.Ldebug_info0
	.byte	0x8
	.byte	0x0
	.value	0x0
	.value	0x0
	.quad	.Ltext0
	.quad	.Letext0-.Ltext0
	.quad	0x0
	.quad	0x0
	.section	.debug_str,"",@progbits
.LASF0:
	.string	"_g95_transfer_real_array"
.LASF2:
	.string	"_g95_st_write"
.LASF1:
	.string	"_g95_st_write_done"
.LASF3:
	.string	"_g95_get_ioparm"
	.ident	"GCC: (GNU) 4.0.3 (g95 0.90!) Jul 27 2006"
	.section	.note.GNU-stack,"",@progbits
