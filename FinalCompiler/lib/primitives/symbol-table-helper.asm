LsymbolTableHelperBody:
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
	
	//This helper prints the symbol table (linked list)
	SHOW("HIIIIIIIIIIIIIIIIIIII",R0);
	MOV(R1, 67);
   LsymbolTableLoopStart:
        CMP(INDD(R1,1),IMM(2));
        JUMP_EQ(LsymbolTableLoopEnd);
        SHOW("-----NODE START:---",R1);
        SHOW("NODE[0]- PTR TO ELEMENT:",INDD(R1,0));
        SHOW("NODE[0]- PTR TO ELEMENT:",INDD(R1,1));
        SHOW("-----NODE END------",R1);
        MOV(R1,INDD(R1,2));
        JUMP(LsymbolTableLoopStart);
   LsymbolTableLoopEnd:
	MOV(R0,IMM(2));
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