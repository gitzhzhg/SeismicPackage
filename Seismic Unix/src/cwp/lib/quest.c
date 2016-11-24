/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
QUEST - Functions to ESTimate Quantiles:

quest		returns estimate - use when entire array is in memory
questalloc	returns quantile estimator - use before questupdate
questupdate	updates and returns current estimate - use for large
		numbers of floats, too big to fit in memory at one time
questfree	frees quantile estimator

******************************************************************************
quest:
Input:
p		quantile to be estimated (0.0<=p<=1.0 is required)
n		number of samples in array x (n>=5 is required)
x		array[n] of floats

Returned:	the estimate of a specified quantile

******************************************************************************
questalloc:
Input:
p		quantile to be estimated (0.0<=p<=1.0 is required)
n		number of samples in array x (n>=5 is required)
x		array[n] of floats

Returned:	pointer to a quantile estimator

******************************************************************************
questupdate:
Input:
q		pointer to quantile estimator (as returned by questalloc)
n		number of samples in array x
x		array[n] of floats

Returned:	quantile estimate

******************************************************************************
questfree:
q		pointer to quantile estimator (as returned by questalloc)

******************************************************************************
Notes:
quest:
The estimate should approach the sample quantile in the limit of large n.

The estimate is most accurate for cumulative distribution functions
that are smooth in the neighborhood of the quantile specified by p.

This function is an implementation of the algorithm published by
Jain and Chlamtac (1985).

questalloc:
This function must be called before calling function questupdate.
See also notes in questupdate.

questupdate:
The estimate should approach the sample quantile in the limit of large n.

The estimate is most accurate for cumulative distribution functions
that are smooth in the neighborhood of the quantile specified by p.

This function is an implementation of the algorithm published by
Jain, R. and Chlamtac, I., 1985, The PP algorithm for dynamic
calculation of quantiles and histograms without storing observations:
Comm. ACM, v. 28, n. 10.

******************************************************************************
Reference:
Jain, R. and Chlamtac, I., 1985, The PP algorithm for dynamic
calculation of quantiles and histograms without storing observations:
Comm. ACM, v. 28, n. 10.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/07/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

float quest(float p, int n, float x[])
/*****************************************************************************
Return an estimate of a specified quantile 
******************************************************************************
Input:
p		quantile to be estimated (0.0<=p<=1.0 is required)
n		number of samples in array x (n>=5 is required)
x		array[n] of floats

Returned:	the estimate of a specified quantile
******************************************************************************
Notes:
The estimate should approach the sample quantile in the limit of large n.

The estimate is most accurate for cumulative distribution functions
that are smooth in the neighborhood of the quantile specified by p.

This function is an implementation of the algorithm published by
Jain, R. and Chlamtac, I., 1985, The PP algorithm for dynamic
calculation of quantiles and histograms without storing observations:
Comm. ACM, v. 28, n. 10.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/07/89
*****************************************************************************/
{
	int m0,m1,m2,m3,m4,mp,mm,i,j;
	float q[5],f1,f2,f3,d1,d2,d3,xtemp,fm0,fm1,fm2,fm3,fm4,fmp,fmm;
	register float q0,q1,q2,q3,q4,qtemp;
		
	/* initialize marker heights to first 5 x values sorted */
	q[0] = x[0];  q[1] = x[1];  q[2] = x[2];  q[3] = x[3];  q[4] = x[4];
	for (i=1; i<5; i++) {
		for (j=i; j>0 && q[j-1]>q[j]; j--) {
			qtemp = q[j-1];
			q[j-1] = q[j];
			q[j] = qtemp;
		}
	}
	q0 = q[0];  q1 = q[1];  q2 = q[2];  q3 = q[3];  q4 = q[4];
		
	/* initialize marker positions */
	m0 = 0;  m1 = 1;  m2 = 2;  m3 = 3;  m4 = 4;
	
	/* initialize desired marker positions */
	f1 = 2*p;  f2 = 4*p;  f3 = 2+2*p;
	
	/* compute increments in desired marker positions */
	d1 = p/2;  d2 = p;  d3 = (1+p)/2;
	
	/* loop over elements in array */
	for (i=5; i<n; i++) {
		xtemp = x[i];
		
		/* increment marker locations and update min and max */
		if (xtemp<q0) {
			m1++;  m2++;  m3++;  m4++;  q0 = xtemp;
		} else if (xtemp<q1) {
			m1++;  m2++;  m3++;  m4++;
		} else if (xtemp<q2) {
			m2++;  m3++;  m4++;
		} else if (xtemp<q3) {
			m3++;  m4++;
		} else if (xtemp<q4) {
			m4++;
		} else {
			m4++;  q4 = xtemp;
		}
		
		/* increment desired marker positions */
		f1+=d1;  f2+=d2;  f3+=d3;
		
		/* adjust height and location of marker 1, if necessary */
		mp = m1+1;  mm = m1-1;
		if (f1>=mp && m2>mp) {
			fmp = mp;  fm2 = m2;  fm1 = m1;  fm0 = m0;
			qtemp = q1+((fmp-fm0)*(q2-q1)/(fm2-fm1)+
				(fm2-fmp)*(q1-q0)/(fm1-fm0))/(fm2-fm0);
			if (q0<qtemp && qtemp<q2)
				q1 = qtemp;
			else
				q1 = q1+(q2-q1)/(fm2-fm1);
			m1 = mp;
		} else if (f1<=mm && m0<mm) {
			fmm = mm;  fm2 = m2;  fm1 = m1;  fm0 = m0;
			qtemp = q1-((fmm-fm0)*(q2-q1)/(fm2-fm1)+
				(fm2-fmm)*(q1-q0)/(fm1-fm0))/(fm2-fm0);
			if (q0<qtemp && qtemp<q2)
				q1 = qtemp;
			else
				q1 = q1-(q0-q1)/(fm0-fm1);
			m1 = mm;
		}
		
		/* adjust height and location of marker 2, if necessary */
		mp = m2+1;  mm = m2-1;
		if (f2>=mp && m3>mp) {
			fmp = mp;  fm3 = m3;  fm2 = m2;  fm1 = m1;
			qtemp = q2+((fmp-fm1)*(q3-q2)/(fm3-fm2)+
				(fm3-fmp)*(q2-q1)/(fm2-fm1))/(fm3-fm1);
			if (q1<qtemp && qtemp<q3)
				q2 = qtemp;
			else
				q2 = q2+(q3-q2)/(m3-m2);
			m2 = mp;
		} else if (f2<=mm && m1<mm) {
			fmm = mm;  fm3 = m3;  fm2 = m2;  fm1 = m1;
			qtemp = q2-((fmm-fm1)*(q3-q2)/(fm3-fm2)+
				(fm3-fmm)*(q2-q1)/(fm2-fm1))/(fm3-fm1);
			if (q1<qtemp && qtemp<q3)
				q2 = qtemp;
			else
				q2 = q2-(q1-q2)/(fm1-fm2);
			m2 = mm;
		}
		
		/* adjust height and location of marker 3, if necessary */
		mp = m3+1;  mm = m3-1;
		if (f3>=mp && m4>mp) {
			fmp = mp;  fm4 = m4;  fm3 = m3;  fm2 = m2;
			qtemp = q3+((fmp-fm2)*(q4-q3)/(fm4-fm3)+
				(fm4-fmp)*(q3-q2)/(fm3-fm2))/(fm4-fm2);
			if (q2<qtemp && qtemp<q4)
				q3 = qtemp;
			else
				q3 = q3+(q4-q1)/(fm4-fm3);
			m3 = mp;
		} else if (f3<=mm && m2<mm) {
			fmm = mm;  fm4 = m4;  fm3 = m3;  fm2 = m2;
			qtemp = q3-((fmm-fm2)*(q4-q3)/(fm4-fm3)+
				(fm4-fmm)*(q3-q2)/(fm3-fm2))/(fm4-fm2);
			if (q2<qtemp && qtemp<q4)
				q3 = qtemp;
			else
				q3 = q3-(q2-q3)/(fm2-fm3);
			m3 = mm;
		}
	}
	return q2;
}

typedef struct _QuestStateStruct { /* quantile estimator state */
	int m0,m1,m2,m3,m4;
	float q0,q1,q2,q3,q4,f0,f1,f2,f3,f4,d0,d1,d2,d3,d4;
} QuestState;

void *questalloc (float p, int n, float x[])
/*****************************************************************************
Alloc, init, & return pointer to a quantile estimator.
******************************************************************************
Input:
p		quantile to be estimated (0.0<=p<=1.0 is required)
n		number of samples in array x (n>=5 is required)
x		array[n] of floats

Returned:	pointer to a quantile estimator
******************************************************************************
Notes:
This function must be called before calling function questupdate.

See also notes in questupdate.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/07/89
*****************************************************************************/
{
	int i,j;
	float q[5],qtemp;
	QuestState *s;
	
	/* allocate space for state */
	s = (QuestState *)malloc(sizeof *s);
	
	/* initialize marker heights to first 5 x values sorted */
	q[0] = x[0];  q[1] = x[1];  q[2] = x[2];  q[3] = x[3];  q[4] = x[4];
	for (i=1; i<5; i++) {
		for (j=i; j>0 && q[j-1]>q[j]; j--) {
			qtemp = q[j-1];
			q[j-1] = q[j];
			q[j] = qtemp;
		}
	}
	s->q0 = q[0];
	s->q1 = q[1];
	s->q2 = q[2];
	s->q3 = q[3];
	s->q4 = q[4];
		
	/* initialize marker positions */
	s->m0 = 0;  s->m1 = 1;  s->m2 = 2;  s->m3 = 3;  s->m4 = 4;
	
	/* initialize desired marker positions */
	s->f0 = 0;  s->f1 = 2*p;  s->f2 = 4*p;  s->f3 = 2+2*p;  s->f4 = 4;
	
	/* compute increments in desired marker positions */
	s->d0 = 0;  s->d1 = p/2;  s->d2 = p;  s->d3 = (1+p)/2;  s->d4 = 1;
	
	/* if more than 5 x values input, then update state */
	if (n>5) qtemp = questupdate(s,n-5,&x[5]);
	
	/* return pointer to state */
	return s;
}

float questupdate (void *q, int n, float x[])
/*****************************************************************************
Update and return a quantile estimate 
******************************************************************************
Input:
q		pointer to quantile estimator (as returned by questalloc)
n		number of samples in array x
x		array[n] of floats

Returned:	quantile estimate
******************************************************************************
Notes:
The estimate should approach the sample quantile in the limit of large n.

The estimate is most accurate for cumulative distribution functions
that are smooth in the neighborhood of the quantile specified by p.

This function is an implementation of the algorithm published by
Jain, R. and Chlamtac, I., 1985, The PP algorithm for dynamic
calculation of quantiles and histograms without storing observations:
Comm. ACM, v. 28, n. 10.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/07/89
*****************************************************************************/
{
	QuestState *s=q;
	int m0,m1,m2,m3,m4,mp,mm,i;
	float q0,q1,q2,q3,q4,qtemp,f0,f1,f2,f3,f4,d0,d1,d2,d3,d4,xtemp;
	
	/* get state variables */
	m0 = s->m0;  m1 = s->m1;  m2 = s->m2;  m3 = s->m3;  m4 = s->m4;
	q0 = s->q0;  q1 = s->q1;  q2 = s->q2;  q3 = s->q3;  q4 = s->q4;
	f0 = s->f0;  f1 = s->f1;  f2 = s->f2;  f3 = s->f3;  f4 = s->f4;
	d0 = s->d0;  d1 = s->d1;  d2 = s->d2;  d3 = s->d3;  d4 = s->d4;
	
	/* loop over elements in array */
	for (i=0; i<n; i++) {
		xtemp = x[i];
		
		/* increment marker locations and update min and max */
		if (xtemp<q0) {
			m1++;  m2++;  m3++;  m4++;  q0 = xtemp;
		} else if (xtemp<q1) {
			m1++;  m2++;  m3++;  m4++;
		} else if (xtemp<q2) {
			m2++;  m3++;  m4++;
		} else if (xtemp<q3) {
			m3++;  m4++;
		} else if (xtemp<q4) {
			m4++;
		} else {
			m4++;  q4 = xtemp;
		}
		
		/* increment desired marker positions */
		f0+=d0;  f1+=d1;  f2+=d2;  f3+=d3;  f4+=d4;
		
		/* adjust height and location of marker 1, if necessary */
		mp = m1+1;  mm = m1-1;
		if (f1>=mp && m2>mp) {
			qtemp = q1+((mp-m0)*(q2-q1)/(m2-m1)+
				(m2-mp)*(q1-q0)/(m1-m0))/(m2-m0);
			if (q0<qtemp && qtemp<q2)
				q1 = qtemp;
			else
				q1 = q1+(q2-q1)/(m2-m1);
			m1 = mp;
		} else if (f1<=mm && m0<mm) {
			qtemp = q1-((mm-m0)*(q2-q1)/(m2-m1)+
				(m2-mm)*(q1-q0)/(m1-m0))/(m2-m0);
			if (q0<qtemp && qtemp<q2)
				q1 = qtemp;
			else
				q1 = q1-(q0-q1)/(m0-m1);
			m1 = mm;
		}
		
		/* adjust height and location of marker 2, if necessary */
		mp = m2+1;  mm = m2-1;
		if (f2>=mp && m3>mp) {
			qtemp = q2+((mp-m1)*(q3-q2)/(m3-m2)+
				(m3-mp)*(q2-q1)/(m2-m1))/(m3-m1);
			if (q1<qtemp && qtemp<q3)
				q2 = qtemp;
			else
				q2 = q2+(q3-q2)/(m3-m2);
			m2 = mp;
		} else if (f2<=mm && m1<mm) {
			qtemp = q2-((mm-m1)*(q3-q2)/(m3-m2)+
				(m3-mm)*(q2-q1)/(m2-m1))/(m3-m1);
			if (q1<qtemp && qtemp<q3)
				q2 = qtemp;
			else
				q2 = q2-(q1-q2)/(m1-m2);
			m2 = mm;
		}
		
		/* adjust height and location of marker 3, if necessary */
		mp = m3+1;  mm = m3-1;
		if (f3>=mp && m4>mp) {
			qtemp = q3+((mp-m2)*(q4-q3)/(m4-m3)+
				(m4-mp)*(q3-q2)/(m3-m2))/(m4-m2);
			if (q2<qtemp && qtemp<q4)
				q3 = qtemp;
			else
				q3 = q3+(q4-q1)/(m4-m3);
			m3 = mp;
		} else if (f3<=mm && m2<mm) {
			qtemp = q3-((mm-m2)*(q4-q3)/(m4-m3)+
				(m4-mm)*(q3-q2)/(m3-m2))/(m4-m2);
			if (q2<qtemp && qtemp<q4)
				q3 = qtemp;
			else
				q3 = q3-(q2-q3)/(m2-m3);
			m3 = mm;
		}
	}
	
	/* set state variables */
	s->m0 = m0;  s->m1 = m1;  s->m2 = m2;  s->m3 = m3;  s->m4 = m4;
	s->q0 = q0;  s->q1 = q1;  s->q2 = q2;  s->q3 = q3;  s->q4 = q4;
	s->f0 = f0;  s->f1 = f1;  s->f2 = f2;  s->f3 = f3;  s->f4 = f4;
	s->d0 = d0;  s->d1 = d1;  s->d2 = d2;  s->d3 = d3;  s->d4 = d4;
	
	/* return current quantile estimate */
	return q2;
}

void questfree (void *q)
/*****************************************************************************
free quantile estimator.
******************************************************************************
Input:
q		pointer to quantile estimator (as returned by questalloc)
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 05/07/89
*****************************************************************************/
{
	free(q);
}
