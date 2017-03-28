
LappendBody:
       
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

	MOV(R1,FPARG(1)); /* R1<-NUM OF ARGS*/
	CMP(R1,IMM(0));
	JUMP_EQ(LappendBodyEmptyArgs);
	CMP(R1,IMM(1));
 	JUMP_EQ(LappendBodyOnlyOneList);
	MOV(R5,R1);
	INCR(R1);
	
	MOV(R2,IMM(2)); /* index i*/
	MOV(R3,FPARG(2)); /* WILL CONTAIN THE LIST WE ARE BUILDING */
	MOV(R4,FPARG(3));
LappendBodyLoopStart:
 	CMP(R2,R1);
 	JUMP_EQ(LappendBodyLoopEnd);
	
	CMP(IND(R4),IMM(T_CLOSURE));
	JUMP_NE(LappendBodyLoopAppendListAndElement);

	PUSH(R4);	
	PUSH(R3);	
	PUSH(IMM(2));
	PUSH(IMM(0));
	//SHOW("GONNA CALL APPEND TWO LISTS",R0);
	CALL(LappendTwoListsBody);
	SHOW("AFTER CALLING APPEND OF TWO LISTS",R0);
	DROP(4);
	MOV(R3,R0);

	JUMP(LappendBodyLoopContinue);
   LappendBodyLoopAppendListAndElement:
	PUSH(R4);
	PUSH(R3);
	PUSH(IMM(2));
	PUSH(IMM(0));
	//SHOW("GONNA CALL APPEND LIST AND ELEMENT",IND(R4));
	CALL(LappendListAndElementBody);
	DROP(4);
	MOV(R3,R0);
		

   LappendBodyLoopContinue:
	INCR(R2); // i++
	INCR(R5);
	MOV(R4,FPARG(R5));
	JUMP(LappendBodyLoopStart);
LappendBodyLoopEnd:
	MOV(R0,R3);
	JUMP(LappendBodyExit);
LappendBodyOnlyOneList:
	MOV(R0,FPARG(2));
	JUMP(LappendBodyExit);
LappendBodyEmptyArgs:
	MOV(R0,IMM(SOB_NIL));
	JUMP(LappendBodyExit);
LappendBodyExit:
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






LappendListAndElementBody:

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
	
	MOV(R1,FPARG(2)); //R1<-LIST1

	PUSH(SOB_NIL);
	PUSH(SOB_NIL);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	MOV(R2,R0); //R2<-BEGININING OF THE NEW LIST
	MOV(R3,R0); //R3<-CURRENT NODE OF THE NEW LIST

LappendListAndElementListLoopStart:
	CMP(R1,IMM(SOB_NIL));
	JUMP_EQ(LappendListAndElementListLoopEnd);

	MOV(INDD(R3,1),INDD(R1,1));
	CMP(INDD(R1,2),IMM(SOB_NIL));
	JUMP_NE(LappendListAndElementListLoopNewPair);
   LappendListAndElementListLoopNewArg:
	MOV(INDD(R3,2),FPARG(3));
	JUMP(LappendListAndElementListLoopNewArgContinue);
   LappendListAndElementListLoopNewPair:
	//CREATE A NEW PAIR
	PUSH(SOB_NIL);
	PUSH(SOB_NIL);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	MOV(INDD(R3,2),R0); //SET THE NEW PAIR AS THE SECOND ELEMENT OF THE NEW NODE
	JUMP(LappendListAndElementListLoopNewArgContinue);

    LappendListAndElementListLoopNewArgContinue:	
	MOV(R1,INDD(R1,2)); //UPDATE R1 TO POINT TO IT'S SECOND ELEMENT
	MOV(R3,INDD(R3,2)); //UPDATE R3 TO POINT TO IT'S SECOND ELEMENT
	JUMP(LappendListAndElementListLoopStart);
LappendListAndElementListLoopEnd:
	MOV(R0,R2);
	/////PUSH(R2);
	////CALL(WRITE_SOB_PAIR);
	////DROP(1);
	JUMP(LappendListAndElementExit);
	//WE WILL NEED TO RUN ALL OVER LIST1 THAT WAS PROVIDED IN FPARG(2) UNTIL WE REACH '()
	//THEN WE WILL START 

LappendListAndElementExit:
	
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






LappendTwoListsBody:

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
	
	MOV(R1,FPARG(2)); //R1<-LIST1

	PUSH(SOB_NIL);
	PUSH(SOB_NIL);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	MOV(R2,R0); //R2<-BEGININING OF THE NEW LIST
	MOV(R3,R0); //R3<-CURRENT NODE OF THE NEW LIST

LappendTwoListsBodyFirstListLoopStart:
	CMP(R1,IMM(SOB_NIL));
	JUMP_EQ(LappendTwoListsBodyFirstListLoopEnd);
	MOV(INDD(R3,1),INDD(R1,1));
	//CREATE A NEW PAIR
	PUSH(SOB_NIL);
	PUSH(SOB_NIL);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	MOV(INDD(R3,2),R0); //SET THE NEW PAIR AS THE SECOND ELEMENT OF THE NEW NODE
	MOV(R1,INDD(R1,2)); //UPDATE R1 TO POINT TO IT'S SECOND ELEMENT
	MOV(R3,INDD(R3,2)); //UPDATE R3 TO POINT TO IT'S SECOND ELEMENT
	JUMP(LappendTwoListsBodyFirstListLoopStart);
LappendTwoListsBodyFirstListLoopEnd:
	MOV(R0,R2);
	MOV(R1,FPARG(3));


   LappendTwoListsBodySecondListLoopStart:
	CMP(R1,IMM(SOB_NIL));
	JUMP_EQ(LappendTwoListsBodySecondListLoopEnd);

	MOV(INDD(R3,1),INDD(R1,1));
	CMP(INDD(R1,2),IMM(SOB_NIL));
	JUMP_NE(LappendTwoListsBodySecondListLoopNewPair);
   LappendTwoListsBodySecondListLoopNewArg:
	MOV(INDD(R3,2),IMM(SOB_NIL));
	JUMP(LappendTwoListsBodySecondListLoopNewArgContinue);
   LappendTwoListsBodySecondListLoopNewPair:
	//CREATE A NEW PAIR
	PUSH(SOB_NIL);
	PUSH(SOB_NIL);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	MOV(INDD(R3,2),R0); //SET THE NEW PAIR AS THE SECOND ELEMENT OF THE NEW NODE
	JUMP(LappendTwoListsBodySecondListLoopNewArgContinue);

   LappendTwoListsBodySecondListLoopNewArgContinue:	
	MOV(R1,INDD(R1,2)); //UPDATE R1 TO POINT TO IT'S SECOND ELEMENT
	MOV(R3,INDD(R3,2)); //UPDATE R3 TO POINT TO IT'S SECOND ELEMENT
	JUMP(LappendTwoListsBodySecondListLoopStart);
   LappendTwoListsBodySecondListLoopEnd:
	MOV(R0,R2);
	JUMP(LappendTwoListsBodyExit);

LappendTwoListsBodyExit:
	
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




