
LvectorBody:
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
  CMP(FPARG(1),IMM(0));
  JUMP_EQ(LvectorBodyLoopEnd);
  MOV(R1,0);
  
LvectorBodyLoopStart:
  INCR(R1);
  MOV(R2, R1);
  ADD(R2,IMM(1));

  PUSH(FPARG(R2));

  CMP(R1,FPARG(1));
  JUMP_NE(LvectorBodyLoopStart);
LvectorBodyLoopEnd:
  PUSH(FPARG(1));
  CALL(MAKE_SOB_VECTOR);
  DROP(FPARG(1));
  DROP(IMM(1));
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