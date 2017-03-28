
LmakeStringBody:
  PUSH(FP);
  MOV(FP, SP);

  CMP(FPARG(1),IMM(1));
  JUMP_LT(L_error_incorrect_num_of_args);
  CMP(FPARG(1),IMM(2));
  JUMP_GT(L_error_incorrect_num_of_args);

 

  CMP(IND(FPARG(2)),IMM(T_INTEGER));
  JUMP_NE(L_error_incorrect_type);

  CMP(FPARG(1),IMM(1)); // in case only one arg
  JUMP_EQ(L_make_zeros);
  
  CMP(IND(FPARG(3)),IMM(T_CHAR));
  JUMP_NE(L_error_incorrect_type);
  
  CMP(INDD(FPARG(2),1),IMM(0));
  JUMP_EQ(LmakeStringBodyEmptyString);

  MOV(R0,INDD(FPARG(2),1)); 
  MOV(R1,FPARG(3)); 
  MOV(R2,R0); 
  ADD(R2,IMM(1)); 
LmakeStringBodyLoopStart:
  PUSH(INDD(R1,1));
  DECR(R0);
  CMP(R0,IMM(0));
  JUMP_NE(LmakeStringBodyLoopStart);
LmakeStringBodyLoopEnd:
  PUSH(INDD(FPARG(2),1));
  CALL(MAKE_SOB_STRING);
  DROP(R2);
  JUMP(LmakeStringBodyExit);
LmakeStringBodyEmptyString:
  PUSH(IMM(0));
  CALL(MAKE_SOB_STRING);
  DROP(1);
  JUMP(LmakeStringBodyExit);

L_make_zeros:
  MOV(R5,INDD(FPARG(2),1)); 
  PUSH(IMM(0));
  CALL(MAKE_SOB_CHAR);
  DROP(1);
  MOV(R1,R0);
  MOV(R2,R5); 
  ADD(R2,IMM(1)); 
LmakeStringBodyLoopStart2:
  PUSH(INDD(R1,1));
  DECR(R5);
  CMP(R5,IMM(0));
  JUMP_NE(LmakeStringBodyLoopStart2);
  JUMP(LmakeStringBodyLoopEnd);

LmakeStringBodyExit:
  POP(FP);
  RETURN;


