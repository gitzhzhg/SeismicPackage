/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/**/
/*
     communication.h 
     objective: define main variables using in my PVM test
*/

#define MASTER          	0
#define NCITIES         	1
#define EVOLVE          	2 
#define SEND_MEMBERS          	2 
#define MEMBER          	2 
#define NEW_MEMBERS    	 	3
#define FROM_THE_MEMBER 	4 
#define INPUT_FILE      	5
#define END_GLOBAL_EVOLUTION 	6
#define IS_IT_CONVERGING	7
#define ABOUT_THE_CONVERGENCE	8
#define DIRECTORY		9

/*
    Definig master & worker components
*/

#define CMASTER "sudgast"
#define CITIES  "process"

/** end of file **/

