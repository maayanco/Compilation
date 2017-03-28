/* scheme/write_sob_integer.asm
 * Take a pointer to a Scheme integer object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  
  
  MOV(R0,FPARG(0));
  MOV(R1,INDD(R0,1));
  MOV(R1,INDD(R1,1));
  MOV(R2,INDD(R0,2));
  MOV(R2,INDD(R2,1));
  PUSH(R1);
  CALL(WRITE_INTEGER);
  DROP(1);
  
  PUSH(IMM('/'));
  CALL(PUTCHAR);
  DROP(1);
  
  /* MOV(R0,FPARG(1)); */
  /* MOV(R0, INDD(R0,1)); */
  PUSH(R2);
  CALL(WRITE_INTEGER);
  DROP(1);
  
  
  POP(FP);
  RETURN;

