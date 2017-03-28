
	
LcalcCommonDivider:

        //PUSH(FP);
        //MOV(FP, SP);
        PUSH(R1);
        PUSH(R2);
        PUSH(R3);
        PUSH(R4);
        PUSH(R5);
        PUSH(R6);
        PUSH(R7);
        PUSH(R8);
        PUSH(R9);
        PUSH(R10);
        PUSH(R11);
        PUSH(R12);
        PUSH(R13);
        PUSH(R14);

        MOV(R2,FPARG(1)); /* NUM OF ARGS*/
        MOV(R1,IMM(1)); /* MULT */
        MOV(R3,IMM(2));
LcalcCommonDividerLoop:
        CMP(R2,IMM(0));
        JUMP_EQ(LcalcCommonDividerLoopEnd);
        MOV(R7,FPARG(R3));
        CMP(INDD(R7,0),IMM(T_FRACTION));
        JUMP_NE(LcalcCommonDividerLoopAfterMulLoopContinuation);        
        MUL(R1,INDD(INDD(R7,2),1));

LcalcCommonDividerLoopAfterMulLoopContinuation:
        DECR(R2);
        INCR(R3);
        JUMP(LcalcCommonDividerLoop);

LcalcCommonDividerLoopEnd:
        PUSH(R1);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        
        POP(R14);
        POP(R13);
        POP(R12);
        POP(R11);
        POP(R10);
        POP(R9);
        POP(R8);
        POP(R7);
        POP(R6);
        POP(R5);
        POP(R4);
        POP(R3);
        POP(R2);
        POP(R1);
        //POP(FP);
        RETURN;
     
     
LplusBody:

        PUSH(FP);
        MOV(FP, SP);
        PUSH(R1);
        PUSH(R2);
        PUSH(R3);
        PUSH(R4);
        PUSH(R5);
        PUSH(R6);
        PUSH(R7);
        PUSH(R8);
        PUSH(R9);
        PUSH(R10);
        PUSH(R11);
        PUSH(R12);
        PUSH(R13);
        PUSH(R14);
	
	CALL(LcalcCommonDivider);
	MOV(R4,INDD(R0,1)); /* R4 WILL CONTAIN THE COMMON DIVIDER!! */

	CMP(FPARG(1),IMM(0));
	JUMP_EQ(LplusBodyZeroArgs);
	
	MOV(R2,FPARG(1)) // int i = num of args
	MOV(R1,IMM(0));  // sum
	MOV(R3,IMM(2));  // index of arg to add
        MOV(R5,IMM(0)); /* TEMPORARY HELPER FOR THE ADDITION PROCESS*/
L_add_loop:
	CMP(R2,IMM(0));
	JUMP_EQ(L_end_loop);
	MOV(R7,IMM(FPARG(R3))); /* IN R7 WE HAVE EITHER T_INTEGER OR T_FRACTION*/
	CMP(IND(R7),IMM(T_FRACTION));
	JUMP_EQ(LaddLoopAddFraction);
	CMP(IND(R7),IMM(T_INTEGER));
	JUMP_EQ(LaddLoopAddInteger);
    LaddLoopAddFraction:
        MOV(R5,INDD(INDD(R7,1),1));
        MUL(R5,R4);
        DIV(R5,INDD(INDD(R7,2),1));
        ADD(R1,R5);
        JUMP(LaddLoopContinueBody);
    LaddLoopAddInteger:
        MOV(R5,INDD(R7,1));
        MUL(R5,R4);
        ADD(R1,R5);
	JUMP(LaddLoopContinueBody);
    LaddLoopContinueBody:
	DECR(R2); // i--
	INCR(R3); // next arg
	JUMP(L_add_loop);

L_end_loop:	
        PUSH(R1);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        MOV(R1,R0);
        PUSH(R4);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        MOV(R2,R0);
        PUSH(R2);
        PUSH(R1);
        
        CALL(MAKE_SOB_FRACTION);
        DROP(2);

        JUMP(LplusBodyExit);
        
LplusBodyZeroArgs:
        PUSH(IMM(0));
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        
LplusBodyExit:

        POP(R14);
        POP(R13);
        POP(R12);
        POP(R11);
        POP(R10);
        POP(R9);
        POP(R8);
        POP(R7);
        POP(R6);
        POP(R5);
        POP(R4);
        POP(R3);
        POP(R2);
        POP(R1);
        POP(FP);
        RETURN;
	
	
	
