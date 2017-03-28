LnotBody:
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
  
  MOV(R1,FPARG(2));
  CMP(IND(R1),IMM(T_BOOL));
  JUMP_EQ(LnotBodyBool);
  JUMP(LnotBodyBoolTrue);
  
LnotBodyBool:
  CMP(INDD(R1,1),IMM(0));
  JUMP_EQ(LnotBodyBoolFalse);
  JUMP_NE(LnotBodyBoolTrue);
 LnotBodyBoolFalse:
  MOV(R0,IMM(SOB_TRUE));
  //PUSH(IMM(1));
  JUMP(LnotBodyExit);
 LnotBodyBoolTrue:
  MOV(R0,IMM(SOB_FALSE));
  //PUSH(IMM(0));
  JUMP(LnotBodyExit);
LnotBodyExit:
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