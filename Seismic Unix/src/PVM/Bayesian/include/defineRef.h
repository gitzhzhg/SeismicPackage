/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* couple of definitions */
#define I       	cmplx(0,1)
#define IC      	cmplx(0,-1)
#define TAUMAX      	0.489

/* function declaration */

void RTd(int iL1, int iL2);

void RTu(int iL1, int iL2);

void horSlowness();

void Bessels(float arg);

void Rm();

void Rp();

void filter();
