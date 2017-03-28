/* DOESN'T TAKE INTO ACCOUNT T_FRACTION!!! */

LvectorSetBody:
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
  MOV(R1,FPARG(2));
  CMP(INDD(R1,0),IMM(T_VECTOR));
  //SHOW("",INDD(R1,0));
  JUMP_NE(L_error_incorrect_type);
  MOV(R2,FPARG(3));
  CMP(INDD(R2,0),IMM(T_INTEGER));
  //SHOW("",INDD(R2,0));
  JUMP_NE(L_error_incorrect_type);
  MOV(R3,FPARG(4));
  //CMP(INDD(R3,0),IMM(T_INTEGER));
  //JUMP_NE(L_error_incorrect_type);
  CMP(INDD(R2,1),INDD(R1,1));
  JUMP_GE(L_error_not_valid_index);
  
// in R1: T_VECTOR
//   in R2: T_INTEGER 
//   in R3: T_INTEGER 
   
   
  MOV(R2,INDD(R2,1));
  ADD(R2,IMM(2));
  //SHOW("INDEX IS:",R2);
  SHOW("",IND(FPARG(4)));
  SHOW("",INDD(FPARG(4),1));
  SHOW("",INDD(INDD(FPARG(4),1),1));
  SHOW("",INDD(FPARG(4),2));
  SHOW("",INDD(INDD(FPARG(4),2),1));
  MOV(INDD(R1,R2),FPARG(4));
/////////////  MOV(R2,INDD(FPARG(2),R2));/*PUSH(INDD(R1,R2));
  //SHOW("",
  //PUSH(INDD(R2,1));
////////  MOV(INDD(R2,1),INDD(R3,1));
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
        
        
