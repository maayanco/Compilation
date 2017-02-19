/* cisc.c

#include <stdio.h>
#include <stdlib.h>

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1
#define T_VOID 		937610
#define T_NIL 		722689
#define T_BOOL 		741553
#define T_CHAR 		181048
#define T_INTEGER 	945311
#define T_STRING 	799345
#define T_SYMBOL 	368031
#define T_PAIR 		885397
#define T_VECTOR 	335728
#define T_CLOSURE 	276405
#include "cisc.h"

int main()
{
  START_MACHINE;

  JUMP(CONTINUE);

#include "char.lib"
#include "io.lib"
#include "math.lib"
#include "string.lib"
#include "system.lib"

CONTINUE:
 	MOV (R0, IMM(5));
	CMP (R0, IMM(0));
	JUMP_EQ(Lend1);
	MOV (R0, IMM(6));
	CMP (R0, IMM(0));
	JUMP_EQ(Lend1);
	MOV(R0, IMM(7));

	CMP (R0, IMM(T_NIL));
	JUMP_EQ(Lend1);
	SHOW ("STR[0] = ", R0);
Lend1:
	STOP_MACHINE;
	return 0;
}
