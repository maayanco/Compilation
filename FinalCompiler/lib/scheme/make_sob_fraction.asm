/* 
 * Should receive 2 integers! (ptr's to integers)
 */

/* MAKE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP,SP);
  
  PUSH(FPARG(0)); // NUM1 
  PUSH(FPARG(1)); // NUM2 
  PUSH(IMM(2));   // arg's num
  PUSH(IMM(00));  // Faux env
 
  CALL(LgcdBody);
  DROP(4);
  MOV(R0,INDD(R0,1));
  MOV(R1,INDD(FPARG(0),1));
  MOV(R2,INDD(FPARG(1),1));
  DIV(R1,R0);
  DIV(R2,R0);
  CMP(R2,IMM(1));
  JUMP_EQ(LmakeSobPotentialFractionMakeInteger);
LmakeSobPotentialFractionMakeFraction:
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  MOV(R2,R0);
  PUSH(R1);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  MOV(R1,R0);
  PUSH(R2);
  PUSH(R1);
  CALL(MAKE_SOB_ONLY_FRACTION);
  DROP(2);
  JUMP(LmakeSobPotentialFractionExit);
LmakeSobPotentialFractionMakeInteger:
  PUSH(R1);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  JUMP(LmakeSobPotentialFractionExit);
LmakeSobPotentialFractionExit:
  POP(FP);
  RETURN;
  

 MAKE_SOB_ONLY_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_FRACTION);
  MOV(R1,FPARG(0));
  MOV(R2,FPARG(1));
  CMP(INDD(R2,1),IMM(0));
  JUMP_LT(denominatorIsNegative);
  JUMP(ContinueMakeSOBOnlyFraction);
  
denominatorIsNegative:
  MOV(R3,INDD(R2,1));
  SHOW("",R3);
  MUL(R3,IMM(-1));
  SHOW("",R3);
  //MOV(FPARG(1),R3);
  PUSH(R3);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  SHOW("",R0);
  SHOW("",INDD(R0,0));
  SHOW("",INDD(R0,1));
  MOV(FPARG(1),R0);
  SHOW("",FPARG(1));
  SHOW("",INDD(FPARG(1),0));
  SHOW("",INDD(FPARG(1),1));
  
  
ContinueMakeSOBOnlyFraction:
  MOV(IND(R0), T_FRACTION);
  MOV(INDD(R0, 1), FPARG(0));
  MOV(INDD(R0, 2), FPARG(1));
  SHOW("MAKING FRACTION:",INDD(INDD(R0,1),1));
  SHOW("MAKING FRACTION:",INDD(INDD(R0,2),1));
  POP(FP);
  RETURN;

  */
  
  /* 
 * Should receive 2 integers! (ptr's to integers)
 */

 

 
 
MAKE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP,SP);
  
  CMP(INDD(FPARG(1),1),IMM(0));
  JUMP_EQ(L_error_second_arg_is_zero);
  CMP(INDD(FPARG(0),1),IMM(0));
  JUMP_EQ(MAKE_INTEGER_ZERO);
  
  PUSH(FPARG(0)); // NUM1 
  PUSH(FPARG(1)); // NUM2 
  PUSH(IMM(2)); // arg's num
  PUSH(IMM(00)); // Faux env
  CALL(LgcdBody);
  DROP(4);
  MOV(R0,INDD(R0,1));
  MOV(R1,INDD(FPARG(0),1));
  MOV(R2,INDD(FPARG(1),1));
  DIV(R1,R0);
  DIV(R2,R0);
  CMP(R2,IMM(1));
  JUMP_EQ(LmakeSobPotentialFractionMakeInteger);
LmakeSobPotentialFractionMakeFraction:
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  MOV(R2,R0);
  PUSH(R1);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  MOV(R1,R0);
  PUSH(R2);
  PUSH(R1);
  CALL(MAKE_SOB_ONLY_FRACTION);
  DROP(2);
  JUMP(LmakeSobPotentialFractionExit);
LmakeSobPotentialFractionMakeInteger:
  PUSH(R1);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  JUMP(LmakeSobPotentialFractionExit);
LmakeSobPotentialFractionExit:
  POP(FP);
  RETURN;
  
MAKE_INTEGER_ZERO:
  PUSH(IMM(0));
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  JUMP(LmakeSobPotentialFractionExit);
  
 MAKE_SOB_ONLY_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_FRACTION);
  MOV(INDD(R0, 1), FPARG(0));
  MOV(INDD(R0, 2), FPARG(1));
  POP(FP);
  RETURN;

 