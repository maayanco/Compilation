LstringToSymbolBody:
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
	CMP(FPARG(1), IMM(1));
	JUMP_NE(L_error_incorrect_num_of_args);
	MOV(R4, 69);
	CMP(IND(R4),IMM(-1));
	JUMP_EQ(LstringToSymbolBodyLoopEndNotFound);
	LstringToSymbolBodyLoopStart:
	PUSH(IND(R4));
	PUSH(FPARG(2));
	PUSH(2);
	PUSH(0); /* AS THOUGH THIS IS THE ENV OR SOMETHING.. */
	CALL(LcompareStringsBody);
	DROP(4);
	CMP(INDD(R0,1),IMM(1));
	JUMP_EQ(LstringToSymbolBodyLoopEndFound);
	CMP(INDD(R0,1),IMM(2));
	JUMP_EQ(LstringToSymbolBodyLoopEndNotFound);
	MOV(R4,INDD(R4,1));
	JUMP(LstringToSymbolBodyLoopStart);
LstringToSymbolBodyLoopEndFound:
	PUSH(IND(R4));
	CALL(MAKE_SOB_SYMBOL);
	DROP(1);
	JUMP(LstringToSymbolBodyExit);
	LstringToSymbolBodyLoopEndNotFound:
	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R4,1),R0); /* NOW INSTEAD OF 2 (NIL) WE SHOULD HAVE THE ADDRESS OF THE NEXT NODE*/
	MOV(INDD(R0,0),FPARG(2));
	MOV(INDD(R0,1),IMM(2));
	PUSH(FPARG(2)); /* SHOULD BE THE ADDRESS OF THE STRING!! */
	CALL(MAKE_SOB_SYMBOL);
	DROP(1);
	JUMP(LstringToSymbolBodyExit);
	LstringToSymbolBodyExit:
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
