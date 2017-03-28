LcompareStringsBody:
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
  
  //SHOW("",FPARG(1));
  //SHOW("",FPARG(2));
  //SHOW("",FPARG(3));
  
  CMP(FPARG(1), IMM(2));
  /*SHOW("FIRSTCHECK1",FPARG(0));*/
  JUMP_NE(L_error_incorrect_num_of_args);
  
  /*SHOW("FIRSTCHECK2",R0);*/
  
  MOV(R1,FPARG(2)); 
  MOV(R2,FPARG(3)); 
  
  CMP(IND(R1),IMM(T_STRING));
  JUMP_NE(L_error_incorrect_type);
  CMP(IND(R2),IMM(T_STRING));
  //SHOW("IN COMPARE STRINGS:",IND(R2));
  JUMP_NE(L_error_incorrect_type);

  CMP(INDD(R1,1),INDD(R2,1));
  JUMP_NE(LcompareStringsBodyNotEqual);
  
  MOV(R3,INDD(R1,1));
  INCR(R3);

LcompareStringsBodyLoop:
  CMP(R3,IMM(1));
  JUMP_EQ(LcompareStringsBodyEqual);
  CMP(INDD(R1,R3),INDD(R2,R3));
  JUMP_NE(LcompareStringsBodyNotEqual);
  DECR(R3);
  JUMP(LcompareStringsBodyLoop);
LcompareStringsBodyEqual:
  //PUSH(IMM(1));
  MOV(R0,IMM(SOB_TRUE));
  JUMP(LcompareStringsBodyEnd);
LcompareStringsBodyNotEqual:
  //PUSH(IMM(0));
  MOV(R0,IMM(SOB_FALSE));
  JUMP(LcompareStringsBodyEnd);
LcompareStringsBodyEnd:
  //CALL(MAKE_SOB_BOOL);
  //DROP(1);
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
