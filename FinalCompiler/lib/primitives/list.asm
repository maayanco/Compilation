	
//LmakeList:	
//	MOV(R1,FPARG(0))
//	MOV(R4,IMM(1 + 0));
//	PUSH(R4);
//	CALL(MALLOC);
//	DROP(1);
//	MOV(R2, R0);
//	MOV(R10,IMM(0));
//	MOV(R11,IMM(1));
//L_loop_start_3:
//	CMP(R10,IMM(0));
//	JUMP_EQ(L_loop_end_3);
//	MOV(INDD(R2,R11), INDD(R1,R10));
//	INCR(R10);
//	INCR(R11);
//	JUMP(L_loop_start_3);
//L_loop_end_3:
//	MOV(R3,FPARG(1));
//	PUSH(R3);
//	CALL(MALLOC);
//	DROP(1);
//	MOV(INDD(R2,0),R0);
//	MOV(R10,IMM(0));
//	MOV(R11,IMM(2));
//L_loop_start_2:
//	CMP(R10,R3);
//	JUMP_EQ(L_loop_end_2);
//	MOV(R7,INDD(R2,0));
//	MOV(INDD(R7,R10),FPARG(R11));
//	INCR(R10);
//	INCR(R11);
//	JUMP(L_loop_start_2);
//L_loop_end_2:
//	MOV(R4, IMM(3));
//	PUSH(R4);
//	CALL(MALLOC);
//	DROP(1);
//	MOV(INDD(R0,0),IMM(T_CLOSURE));
//	MOV(INDD(R0,1),R2);
//	MOV(INDD(R0,2),LABEL(LlistBody));
//	JUMP(L_clos_exit_1);
LlistBody:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R1,IMM(SOB_NIL));
	MOV(R9,FPARG(1));
	MOV(R10,IMM(1 + R9));
LloopBodystart1:
	CMP(R10,IMM(1));
	JUMP_EQ(LloopBodyEnd1);
	PUSH(R1);
	PUSH(FPARG(R10));
	PUSH(IMM(2));
	PUSH(IMM(777));
	CALL(LconsBody);
	MOV(R1,R0);
	DROP(IMM(4));
	DECR(R10);
	JUMP(LloopBodystart1);
LloopBodyEnd1:
	MOV(FPARG(1), IMM(1));
	MOV(FPARG(2), R1);
	MOV(R0,FPARG(2));

	POP(FP);
	RETURN;
//L_clos_exit_1:
//	CMP(R0, IMM(SOB_VOID));
//	JUMP_EQ(L_R0_is_void_1);
//	PUSH(R0);
//	CALL(WRITE_SOB);
//	DROP(1);
//	PUSH(IMM(10));
//	CALL(PUTCHAR);
//	DROP(1); 
