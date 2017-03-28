// need update!! - this is a simple add function for integers

LlargerThanBody:

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
	CMP(FPARG(1),IMM(1));
	JUMP_EQ(LlargerThanOneArg);
	MOV(R2,FPARG(1)) // int i = num of args
	MOV(R1,IMM(1));  // flag, if false (0) than jump to the end and return it's value
	MOV(R3,IMM(2));  // index of arg to add
	MOV(R4,IMM(1)); /* Will contain numeratorLeft * denumeratorRight*/
	MOV(R5,IMM(1)); /* Will contain numeratorLeft * denumeratorRight*/
	//SHOW("HI",R0);
LlargerThanLoop:
	//SHOW("R3 IS:",R3);
	CMP(IND(FPARG(R3)),IMM(T_INTEGER));
	JUMP_EQ(LlargerThanSetInteger);
  LlargerThanSetFraction:
	//SHOW("",INDD(INDD(FPARG(R3),1),1));
	//SHOW("",INDD(INDD(FPARG(R3),2),1));
	MOV(R4,INDD(INDD(FPARG(R3),1),1));
	MOV(R5,INDD(INDD(FPARG(R3),2),1));
	//SHOW("FRAC - LEFT SIDE IS:",R4);
	//SHOW("FRAC - RIGHT SIDE IS:",R5);
	JUMP(LlargerThanLoopCont);
  LlargerThanSetInteger:
	MOV(R4,INDD(FPARG(R3),1));
	MOV(R5,IMM(1));
	//SHOW("INT -LEFT SIDE IS:",R4);
	//SHOW("INT -RIGHT SIDE IS:",R5);
	JUMP(LlargerThanLoopCont);

  LlargerThanLoopCont:

	DECR(R2); // i--
	INCR(R3); // next arg

	CMP(R2,IMM(0));
	JUMP_EQ(LlargerThenloopEnd);

	CMP(IND(FPARG(R3)),IMM(T_FRACTION));
	JUMP_EQ(LlargerThanLoopSetFraction);

   LlargerThanLoopSetInteger:
	//SHOW("INT - MULTIPLYING R5 WITH:",INDD(FPARG(R3),1));
	MUL(R5,INDD(FPARG(R3),1));
	JUMP(LlargerThanLoopContinue);
   LlargerThanLoopSetFraction:
	//SHOW("FRAC - MULTIPLYING R4 WITH:", INDD(INDD(FPARG(R3),2),1));
	//SHOW("FRAC - MULTIPLYING R5 WITH:", INDD(INDD(FPARG(R3),1),1));
	MUL(R4,INDD(INDD(FPARG(R3),2),1));
	MUL(R5,INDD(INDD(FPARG(R3),1),1));
	JUMP(LlargerThanLoopContinue);
   LlargerThanLoopContinue:
	CMP(R4,R5);
	//SHOW("LEFT AFTER MULT IS:",R4);
	//SHOW("RIGHT AFTER MULT IS:",R5);
	JUMP_LE(LlargerThanLoopEndFalse);

	/* DECR(R2); // i--
	INCR(R3); // next arg */
	
	JUMP(LlargerThanLoop);

LlargerThenloopEnd:
	//SHOW("AT LOOP END",R1);
	//PUSH(R1);
	CMP(R1,IMM(1));
	JUMP_EQ(LlargerThanLoopEndTrue);
	CMP(R1,IMM(0));
	JUMP_EQ(LlargerThanLoopEndFalse);
    LlargerThanLoopEndTrue:
        MOV(R0,IMM(SOB_TRUE));
        JUMP(LlargerThanExit);


LlargerThanLoopEndFalse:
	//SHOW("AT LOOP END FALSE",R0);
	//PUSH(IMM(0));
	//CALL(MAKE_SOB_BOOL);
	//DROP(1);
	MOV(R0,IMM(SOB_FALSE));
	JUMP(LlargerThanExit);

LlargerThanOneArg:
	//SHOW("AT LOOP END ONE ARG",R0);
	//PUSH(IMM(1));
	//CALL(MAKE_SOB_BOOL);
	//DROP(1);
	MOV(R0,IMM(SOB_TRUE));
	JUMP(LlargerThanExit);
LlargerThanExit:
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