LmakeApplyBody:

        PUSH(FP);
        MOV(FP, SP);

        
    //SHOW("",FPARG(1));
    CMP(FPARG(1),IMM(2));
    JUMP_NE(L_error_incorrect_num_of_args);
    
    MOV(R3,FPARG(2)); /* the procedure to apply */
    CMP(INDD(R3,0),T_CLOSURE);

    JUMP_NE(L_error_cannot_apply_non_clos);

    MOV(R2,IMM(0)); /* int counter=0 */
   //SHOW("HI MAAYAN",R0);

    /* loop */
    MOV(R1,FPARG(3));
L_count_num_list_args:


    CMP(R1,IMM(SOB_NIL)); // NULL
    MOV(R9,INDD(R1,2));
    //SHOW("curr arg",R9);
    //SHOW("",R1);
    
    JUMP_EQ(L_count_end);

    CMP(INDD(R1,0),IMM(T_PAIR));
    //SHOW("SHOULD BE A PAIR:",INDD(R1,0));
    JUMP_NE(L_error_incorrect_type);

    PUSH(INDD(R1,1));
    MOV(R1,INDD(R1,2));
    INCR(R2); /* counter++ */
    //SHOW("counter",R2);
    JUMP(L_count_num_list_args);

L_count_end:

    PUSH(IMM(R2));
    PUSH(INDD(R3,1)); /* env' */
    PUSH(FPARG(-1));
    MOV(R1,FPARG(-2)); /* save old fp in R1 */
	MOV(R7,FPARG(1) + 4); /* old frame size */

	/* loop update frame */
			//MOV(R6,IMM(R2 + 3));  /* int i = num of args + 3 */
	MOV(R5,R2);				 /* int i = num of args*/
	MOV(R4,IMM(-2));
	SUB(R4,IMM(R2));  		 
	MOV(R10,IMM(R4)); 		 /* pointer to last arg of new frame */
	MOV(R11,IMM(FPARG(1)+1)) /* pointer to last arg of old frame */

L_loop_update_start:
          //SHOW("HI MAAYAN LOOP UPDATE START",R0);
	CMP(R5,IMM(0));
	JUMP_EQ(L_update_next_step);
	MOV(FPARG(R11),FPARG(R10));
	INCR(R10);
	DECR(R11);
	DECR(R5);
	JUMP(L_loop_update_start);

L_update_next_step:
         //SHOW("HI MAAYAN UPDATE NEXT STEP",R0);
	MOV(R5,IMM(3));
	DECR(R10);
	SUB(R10,IMM(R2));

L_loop_continue_update:
  //SHOW("HI MAAYAN LOOP CONTINUE UPDATE",R0);
	CMP(R5,IMM(0));
	JUMP_EQ(L_update_end);
	MOV(FPARG(R11),FPARG(R10));
	DECR(R10);
	DECR(R11);
	DECR(R5);
	JUMP(L_loop_continue_update);

L_update_end:
  //SHOW("HI MAAYAN LOOP UPDATE END",R0);
	DROP(R7);
	MOV(FP,R1);
        //SHOW("HI MAAYAN BEFORE JUMPA",R0);
        //SHOW("",INDD(R3,2));
        //SHOW("",FPARG(0));
        //SHOW("",FPARG(1));
        //SHOW("",FPARG(2));
	JUMPA(INDD(R3,2));
	//SHOW("HI MAAYAN END OF APPLY",R0);
        POP(FP);
        RETURN;

