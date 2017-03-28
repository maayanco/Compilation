
     
LdivBody:

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

	CMP(FPARG(1),IMM(1));
	JUMP_EQ(LdivBodyOneArg);
	
	MOV(R2,FPARG(1)) /* int i = num of args */
	DECR(R2);

	/* MOV(R1,FPARG(2)); */ /* mult Numerator */
	/* MOV(R6,IMM(1)); */ /* mult denumerator */
	MOV(R3,IMM(3));  // index of arg to add
        MOV(R5,IMM(0)); /* TEMPORARY HELPER FOR THE ADDITION PROCESS*/
	CMP(IND(FPARG(2)),IMM(T_FRACTION));
	//SHOW("",IND(FPARG(2)));
	//SHOW("",INDD(FPARG(2),1));

	JUMP_EQ(LdivInitializeRegistersForFraction);
LdivInitializeRegistersForInteger:
	MOV(R1,INDD(FPARG(2),1));
	MOV(R6,IMM(1));
	//SHOW("INITIALIZE NUMERATOR:",R1);
	//SHOW("INITIALIZE DENUMERATOR:",R6);
	JUMP(LdivLoop);
LdivInitializeRegistersForFraction:
	MOV(R1,INDD(INDD(FPARG(2),1),1));
	MOV(R6,INDD(INDD(FPARG(2),2),1));
	//SHOW("INITIALIZE NUMERATOR:",R1);
	//SHOW("INITIALIZE DENUMERATOR:",R6);
	JUMP(LdivLoop);
LdivLoop:
	CMP(R2,IMM(0));
	JUMP_EQ(LdivEndLoop);
	MOV(R7,IMM(FPARG(R3))); /* IN R7 WE HAVE EITHER T_INTEGER OR T_FRACTION*/
	CMP(IND(R7),IMM(T_FRACTION));
	JUMP_EQ(LdivLoopAddFraction);
	CMP(IND(R7),IMM(T_INTEGER));
	//SHOW("FOR CMP",IND(R7));
	JUMP_EQ(LdivLoopAddInteger);
    LdivLoopAddFraction:
	//SHOW("IN ADD FRACTION",R0);
	//SHOW(">>>>",INDD(INDD(R7,1),1));
	//SHOW(">>>>",INDD(INDD(R7,2),1));
	//SHOW("NUMERATOR BEFORE:",R1);
        //SHOW("GONNA MULTIPLY BY:",INDD(INDD(R7,2),1));
        MUL(R1,INDD(INDD(R7,2),1));
        //SHOW("NUMERATOR AFTER:",R1);
        //SHOW("!!",R0);        
	//SHOW("DENUMERATOR BEFORE:",R6);
        //SHOW("GONNA MULTIPLY BY:",INDD(INDD(R7,1),1));
        MUL(R6,INDD(INDD(R7,1),1));
        //SHOW("DENUMERATOR AFTER:",R6);
        //SHOW("!!!!!!!!!!!!",R0);        
        JUMP(LdivLoopContinueBody);
    LdivLoopAddInteger:
	//SHOW("IN ADD INTEGER",R0);
	//SHOW(">>>",INDD(R7,1));
        //SHOW("DENUMERATOR BEFORE:",R6);
        //SHOW("GONNA MULTIPLY BY:",INDD(R7,1));
        MUL(R6,INDD(R7,1));
        //SHOW("DENUMERATOR AFTER:",R6);
        //SHOW("!!!!!!!!!!!!",R0);
	JUMP(LdivLoopContinueBody);
    LdivLoopContinueBody:
	DECR(R2); // i--
	INCR(R3); // next arg
	JUMP(LdivLoop);

LdivEndLoop:	
        PUSH(R1);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        MOV(R1,R0);
        PUSH(R6);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        MOV(R2,R0);
        PUSH(R2);
        PUSH(R1);
        CALL(MAKE_SOB_FRACTION);
        DROP(2);

        JUMP(LdivBodyExit);
        
LdivBodyOneArg:
	MOV(R0,FPARG(2));
        /*PUSH(IMM(1));
        CALL(MAKE_SOB_INTEGER);
        DROP(1);*/
        
LdivBodyExit:

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
	
	
	
