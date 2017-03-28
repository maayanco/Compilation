LisRationalBody:
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
  
  /* Check if INTEGER*/
  MOV(R0, FPARG(2));
  CMP(INDD(R0,0),IMM(T_INTEGER));
  JUMP_EQ(LisRationalBodyTrue);
  
  /* Check if fraction*/
  CMP(INDD(R0,0), IMM(T_FRACTION));
  JUMP_EQ(LisRationalBodyTrue);
  
  
LisRationalBodyFalse:
  //PUSH(IMM(0));
  MOV(R0,IMM(SOB_FALSE));
  JUMP(LisRationalBodyExit);
  
LisRationalBodyTrue:
  //PUSH(IMM(1));
  MOV(R0,IMM(SOB_TRUE));
  JUMP(LisRationalBodyExit);
  
  
LisRationalBodyExit:
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

