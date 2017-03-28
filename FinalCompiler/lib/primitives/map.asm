
LmapBody:
        //SHOW("THIS IS QUITE CRAZY",R0);
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
   ///SHOW("HI111",R0);
   CMP(FPARG(1), IMM(2));
   JUMP_NE(L_error_incorrect_num_of_args);
   CMP(IND(FPARG(2)),IMM(T_CLOSURE)); //FPARG2 <- LAMBDA
   JUMP_NE(L_error_incorrect_type);
   CMP(IND(FPARG(3)),IMM(T_NIL));
   JUMP_EQ(LmapBodyEmptyList);
   CMP(IND(FPARG(3)),IMM(T_PAIR)); //FPARG3 <- LIST
   JUMP_NE(L_error_incorrect_type);
   
   MOV(R3,FPARG(3)); // R3<- CONTAINS THE ORIGINAL LIST 
   MOV(R4,IMM(SOB_NIL)); //R4<- WILL CONTAIN THE START OF THE NEW LIST
   PUSH(SOB_NIL);
   PUSH(SOB_NIL);
   CALL(MAKE_SOB_PAIR);
   DROP(2);
   MOV(R4,R0);
   MOV(R5,R4);
   //SHOW("HOLA",R0);
LmapLoopStart:

   CMP(R3,IMM(SOB_NIL));
   JUMP_EQ(LmapLoopEnd);

   PUSH(INDD(R3,1));
   PUSH(IMM(1));
   PUSH(INDD(FPARG(2),1));
   CALLA(INDD(FPARG(2),2));
   DROP(3);

   //SHOW("BEFORE PUSHING",R0);
   //PUSH(SOB_NIL);
   //PUSH(INDD(R3,1));
   //CALL(MAKE_SOB_PAIR);
   //DROP(2);
   //PUSH(R0);
   //CALL(WRITE_SOB_PAIR);
   //DROP(1);
   //PUSH(R0);
   //PUSH(FPARG(2));
   //PUSH(IMM(2));
   //PUSH(IMM(0));
   //CALL(LmakeApplyBody);
   //DROP(4);
   //SHOW("AFTER PUSHING",R0);

   MOV(INDD(R5,1),R0); // SET R5 IN PLACE 1 AS THE RESULT OF THE APPLY
   CMP(INDD(R3,2),IMM(SOB_NIL));
   JUMP_NE(LmapLoopCreatePair);
 LmapLoopCreateNil:
   MOV(INDD(R5,2),IMM(SOB_NIL));
   JUMP(LmapLoopBodyContinue);
 LmapLoopCreatePair:
   PUSH(SOB_NIL);
   PUSH(SOB_NIL);
   CALL(MAKE_SOB_PAIR);
   DROP(2);
   MOV(INDD(R5,2),R0); //SET R5 IN PLACE 2 AS A NEW "EMPTY" PAIR.. 
   JUMP(LmapLoopBodyContinue);
LmapLoopBodyContinue:
   MOV(R3,INDD(R3,2));
   MOV(R5, INDD(R5,2));

   JUMP(LmapLoopStart);
LmapLoopEnd:
   //SHOW("HELLO IN LOOP END",R0);
   //PUSH(R4);
   //CALL(WRITE_SOB_PAIR);
   //DROP(1);
   //SHOW("HELLO", R0);

   MOV(R0,R4);
   JUMP(LmapBodyExit);

LmapBodyEmptyList:
   //SHOW("HELLO IN OTHER CASE!",R0);
   MOV(R0,IMM(SOB_NIL));
   JUMP(LmapBodyExit);
LmapBodyExit:
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


// what i need to do? 
/*(map (lambda (x i dg) gdfag) '(1 2 3 4))*/
