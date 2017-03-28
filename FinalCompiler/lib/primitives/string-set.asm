LstringSetBody:
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
  
  CMP(FPARG(1), IMM(3));
  JUMP_NE(L_error_incorrect_num_of_args);
  
  MOV(R1, FPARG(2)); /* R1<-POINTER TO STRING*/
  CMP(INDD(R1,0),IMM(T_STRING));
  JUMP_NE(L_error_incorrect_type);
  MOV(R2,FPARG(3)); 
  CMP(IND(R2),IMM(T_INTEGER));
  JUMP_NE(L_error_incorrect_type);
  MOV(R2,INDD(R2,1)); /* R2<-4*/
  MOV(R3,FPARG(4)); /* R3<-POINTER TO #\Y*/
  CMP(IND(R3),IMM(T_CHAR));
  JUMP_NE(L_error_incorrect_type);
  CMP(INDD(R1,1),R2);
  JUMP_LE(L_error_arg2_is_smaller_than_string);
  
  ADD(R2, IMM(2));
  MOV(INDD(R1,R2),INDD(R3,1));
  
  MOV(R0,IMM(SOB_VOID));
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