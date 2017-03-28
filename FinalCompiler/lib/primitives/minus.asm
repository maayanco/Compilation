LmakeMinusBody:

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
	//SHOW("AFTER COMMON DIVIDER!!",INDD(R0,1));
	MOV(R4,INDD(R0,1)); /* R4 WILL CONTAIN THE COMMON DIVIDER!! */
	MOV(R5,IMM(0));     /* TEMPORARY HELPER FOR THE ADDITION PROCESS*/

    CMP(FPARG(1),IMM(1));
    JUMP_LT(L_error_no_args_for_sub);

    MOV(R2,FPARG(1)); // int i = num of args

	MOV(R3,IMM(2));   // index of arg to sub
	MOV(R7,IMM(FPARG(R3)));
	CMP(INDD(R7,0),IMM(T_FRACTION));
	JUMP_EQ(L_first_arg_fraction);
	CMP(INDD(R7,0),IMM(T_INTEGER));
	JUMP_EQ(L_first_arg_integer);

    L_first_arg_fraction:
        //SHOW("!!!!!!!!!!!!!!!",R7);
        MOV(R5,INDD(INDD(R7,1),1));
        CMP(FPARG(1),IMM(1));       // check if one argument only
        JUMP_EQ(L_one_arg);
        //SHOW("NUMERATOR:", INDD(INDD(R7,1),1));
        //SHOW("DENUMERATOR:",INDD(INDD(R7,2),1));
        //SHOW("1",R5);
        MUL(R5,R4);
        //SHOW("AFTER MULT",R5);
        DIV(R5,INDD(INDD(R7,2),1));
        //SHOW("AFTER DIVISION",R5);
        //SHOW("CURR NUM IS:",R5);
        MOV(R1,R5);
    	JUMP(L_sub_loop);

    L_first_arg_integer:
        MOV(R5,INDD(R7,1));

        CMP(FPARG(1),IMM(1));      // check if one argument only
        JUMP_EQ(L_one_arg);

        MUL(R5,R4);
        //SHOW("CURR NUM IS:",R5);
        MOV(R1,R5);

L_sub_loop:
	DECR(R2); //i--
	CMP(R2,IMM(0));
	JUMP_EQ(L_end_loop_minus);
	INCR(R3); // next arg

	MOV(R7,IMM(FPARG(R3))); /* IN R7 WE HAVE EITHER T_INTEGER OR T_FRACTION*/
	
	CMP(IND(R7),IMM(T_FRACTION));
	JUMP_EQ(LsubLoopSubFraction);
	
	CMP(IND(R7),IMM(T_INTEGER));
	JUMP_EQ(LsubLoopSubInteger);
	
    LsubLoopSubFraction:
        //SHOW("!!!!!!!!!!!!!!!",R7);
        MOV(R5,INDD(INDD(R7,1),1));
        //SHOW("NUMERATOR:", INDD(INDD(R7,1),1));
        //SHOW("DENUMERATOR:",INDD(INDD(R7,2),1));
        //SHOW("1",R5);
        MUL(R5,R4);
        //SHOW("AFTER MULT",R5);
        DIV(R5,INDD(INDD(R7,2),1));
        //SHOW("AFTER DIVISION",R5);
        //SHOW("CURR NUM IS:",R5);
        SUB(R1,R5);
        JUMP(LsubLoopContinueBody);
    LsubLoopSubInteger:
        MOV(R5,INDD(R7,1));
        MUL(R5,R4);
        //SHOW("CURR NUM IS:",R5);
        SUB(R1,R5);
    LsubLoopContinueBody:
	JUMP(L_sub_loop);


    L_one_arg:
        MOV(R3,IMM(0));
        SUB(R3,R5);
        MOV(R1,R3);

L_end_loop_minus:	
        PUSH(R1);
        //SHOW("FIRST ARG IS:",R1);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        MOV(R1,R0);
        PUSH(R4);
        //SHOW("SECOND ARG IS:",R4);
        CALL(MAKE_SOB_INTEGER);
        DROP(1);
        MOV(R2,R0);
        PUSH(R2);
        PUSH(R1);
        
        CALL(MAKE_SOB_FRACTION);
        DROP(2);
        
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