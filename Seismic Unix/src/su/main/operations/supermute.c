/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPERMUTE: $Revision: 1.5 $ ; $Tue May 15 14:59:26 EDT 2001 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUPERMUTE - permute or transpose a 3d datacube	 		",
" 									",
" supermute <stdin >sdout	 					",
" 									",
" Required parameters:							",
" none									",
" 									",
" Optional parameters:							",
" n1=ns from header		number of samples in the fast direction	",
" n2=ntr from header		number of samples in the med direction	",	
" n3=1				number of samples in the slow direction	",
" 									",
" o1=1				new fast direction			",
" o2=2				new med direction			",
" o3=3				new slow direction			",
" 									",
" d1=1				output interval in new fast direction	",
" d2=1				output interval in new med direction	",
" d3=1				output interval in new slow direction	",
" 									",
" Notes:								",
" header fields d1 and d2 default to d1=1.0 and d2=1.0			",
NULL};

/* Credits:
 *
 *	VT: Matthias Imhof
 *
 * Trace header fields accessed: ns, ntr
 * Trace header fields modified: d1=1, f1=1, d2=1, f2=1, ns, ntr
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	float ***data;		/* data					*/
	int n[4];		/* old number of points			*/
	int m[4];		/* old number of points			*/
	int o[4];		/* new output order			*/
	int d[4];		/* new output interval			*/
	int verbose;
	int check;

	int	p1[4];
	int	p2[4];
	int	p3[4];

	/* note: array elem 0 is always dummy! */

	int	i, j, k, p, q, r;
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	if (!getparint("verbose", &verbose))	verbose=0;
	if (!getparint("check", &check))	check=0;

	/* Get info from first trace */ 
	if (!gettr(&tr))  err("cannot get first trace");

	if (!getparint("n1", &n[1]))	n[1] = (int) tr.ns;
	if (!getparint("n2", &n[2]))	n[2] = (int) tr.ntr;
	if (!getparint("n3", &n[3]))	n[3] = 1;
	for(i=1; i<=3; i++)
	{  if (! (n[i])) err("invalid dimensions: %d %d %d", n[1], n[2], n[3]);
	}
	if (verbose)
	{  warn("old dimensions: %d %d %d", n[1], n[2], n[3]);
	}

	if (!getparint("o1", &o[1]))	o[1] = 1;
	if (!getparint("o2", &o[2]))	o[2] = 2;
	if (!getparint("o3", &o[3]))	o[3] = 3;
	for(i=1; i<=3; i++)
	{  if (o[i] < 1 || o[i] > 3) err("invalid permutation %d: %d", i, o[i]);
	}
	if (verbose)
	{  warn("new order: %d %d %d", o[1], o[2], o[3]);
	}

	m[1] = n[o[1]];
	m[2] = n[o[2]];
	m[3] = n[o[3]];
	if (verbose)
	{  warn("new dimension: %d %d %d", m[1], m[2], m[3]);
	}

	if (!getparint("d1", &d[1]))	d[1] = 1;
	if (!getparint("d2", &d[2]))	d[2] = 1;
	if (!getparint("d3", &d[3]))	d[3] = 1;
        checkpars();

	for(i=1; i<=3; i++)
	{  if (!(d[i])) err("invalid interval %d: %d", i, d[i]);
	}
	if (verbose)
	{  warn("new interval: %d %d %d", d[1], d[2], d[3]);
	}

	for(i=1; i<=3; i++)
	{  p1[i] = p2[i] = p3[i] = 0;
	}
	p1[o[1]] = 1;
	p2[o[2]] = 1;
	p3[o[3]] = 1;
	if (verbose)
	{  warn("perm 1: %d %d %d", p1[1], p1[2], p1[3]);
	   warn("perm 2: %d %d %d", p2[1], p2[2], p2[3]);
	   warn("perm 3: %d %d %d", p3[1], p3[2], p3[3]);
	}

	data = alloc3float(m[1], m[2], m[3]);
	if (! data) err("not enough memory: %d %d %d", m[1], m[2], m[3]);
	if (verbose) warn("allocated %d*%d*%d floats", m[1], m[2], m[3]);

	/* Main loop over traces */
	for(k=1; k<=n[3]; k++)
	{  for(j=1; j<=n[2]; j++)
	   {  if (!((k==1) && (j==1)))
	      {  if (check)
	         {  warn("read trace %d %d", k, j);
	         }
		 if (!(gettr(&tr)))
	         {  err("wrong amount of data: %d, %d (%d), %d (%d)", 
		 	n[1], n[2], j, n[3], k);
	         }
	      }
	      
	      for(i=1; i<=n[1]; i++)
	      {  int t = i-1;
	         p = p1[1] * i + p1[2] * j + p1[3] * k;
	         q = p2[1] * i + p2[2] * j + p2[3] * k;
	         r = p3[1] * i + p3[2] * j + p3[3] * k;
	         
		 if (d[1] < 0)
		 {  p = m[1] - p + 1;
		 }
		 
		 if (d[2] < 0)
		 {  q = m[2] - q + 1;
		 }

		 if (d[3] < 0)
		 {  r = m[3] - r + 1;
		 }
		 
		 if (verbose==2)
		 {  fprintf(stderr, "%4d %4d %4d to %4d %4d %4d\n", 
		 	    i, j, k, p, q, r);
		 }
		 
		 if (check)
		 {  if ((p < 1) || (p > m[1]) || 
		 	(q < 1) || (q > m[2]) ||
			(r < 1) || (r > m[3]))
	   	    {  err("access error: %d (%d) %d, %d (%d) %d, %d (%d) %d",
		           1, p, m[1], 1, q, m[2], 1, r, m[3]);
		    }
		 }	
		 
		 p--;
		 q--;
		 r--;
		 
		 #if 0
		 if (check)
		 {  fprintf(stderr, "%d %d %d\n", r, q, p);
		 }
		 #endif
		 
		 #if 1
		 data[r][q][p] = tr.data[t];
		 #else
		 data[r][q][p] = r*100+q*10+p;
		 #endif
	      }
	      if (check)
	      {  warn("next trace");
	      }
	   }
	}

	if (verbose==2)
	{  fprintf(stderr, "m1 m2 m3: %d %d %d\n", m[1], m[2], m[3]);
	   for(r=1; r<=m[3]; r++)
	   {  for(q=1; q<=m[2]; q++)
	      {  for(p=1; p<=m[1]; p++)
	         {  i = p-1;
	            j = q-1;
		    k = r-1;
		    fprintf(stderr, "%16.6f", data[k][j][i]);
		 }
		 fprintf(stderr, "\n");
	      }
	      fprintf(stderr, "\n");
	   }
	}

	tr.ns = (m[1]+abs(d[1])-1) / abs(d[1]);
	tr.ntr = ((m[2]+abs(d[2]-1) / abs(d[2])) * 
		  (m[3]+abs(d[3]-1) / abs(d[3])));
	tr.d1 = 1.0;
	tr.f1 = 1.0;
	tr.d2 = 1.0;
	tr.f2 = 1.0;
	

	for(r=1; r<=m[3]; r+=abs(d[3]))
	{  for(q=1; q<=m[2]; q+=abs(d[2]))
	   {  int t = 0;
	      for(p=1; p<=m[1]; p+=abs(d[1]))
	      {  i = p-1;
	         j = q-1;
		 k = r-1;
		 
		 if (verbose==3)
		 {  fprintf(stderr, "%4d %4d %4d %f\n", p, q, r, data[k][j][i]);
		 }
		 
	         tr.data[t] = data[k][j][i];
		 t++;
	      }
	      puttr(&tr);
	   }
	}

	free3float(data);

	return(CWP_Exit());
}
