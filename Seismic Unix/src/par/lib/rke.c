/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "par.h"
#include "rke.h"

/*********************** self documentation **********************/
/************************************************************************
RKE - integrate a system of n-first order ordinary differential equations
      from a given time to a target time using the Runge Kutta method,
      enhanced the the error checking algorithm created by R. England (1969).

*************************************************************************
Credits :
Ordinary differential equation solver, Runge-Kutta-England technique.
Made available for SU by the author's permission, 1 Sept 2006.
Copyright 1988: François Pinard <pinard@iro.umontreal.ca>, 1988.

*************************************************************************
Reference: 
England, R. (1969), Error estimates for Runge-Kutta type solutions to
systems of ordinary differential equations,
The Computer Journal, Volume 12, Issue 2, pp. 166-170

Abstract:
Means of estimating the truncation error of Runge-Kutta type methods are 
considered. A number of existing processes have various disadvantages, 
giving a valid estimate only for certain types of equation. The conditions 
are stated for a process to give a valid estimate when applied to a general 
system of differential equations, and a number of fifth order estimates 
are proposed for a fourth order Runge-Kutta type method. Some numerical tests 
are applied to these processes, which suggest their superiority over 
Merson's process. 

*************************************************************************
Author: François Pinard <pinard@iro.umontreal.ca>, 1988.
Modified: John Stockwell, Center for Wave Phenomena (CWP),
	  Colorado School of Mines (CSM), August 2004, for
	  integration into the CWP/SU Seismic Unix package. Some 
	  stylistic changes were made, and some SU routines were
          incorporated into the modified code.
*************************************************************************/
/**************** end self doc ********************************/

/* Initialize a new system of equations. */
rke_variables
rke_init (int number, int (*routine)())	
/*************************************************************************
rke_init - initiate a newly allocated reentrancy block for RKE routines.
**************************************************************************
Input:
number		Number of simultaneous equations
routine		function representing system of equations being solved 
**************************************************************************
Author:	François Pinard <pinard@iro.umontreal.ca>, 1988.
***************************************************************************/
{
	rke_variables var;

	var = (rke_variables) malloc (sizeof (struct struct_rke_variables));

	var->n_equations = number;
	var->eval_routine = routine;
	var->minimum_step = RKE_STEP_MIN;
	var->maximum_step = RKE_STEP_MAX;
	var->current_step = 1.0;
	var->error_slope = RKE_ERR_SLOPE_INIT; 
	var->error_bias = RKE_ERR_BIAS_INIT;
	var->accepted_steps = 0;
	var->rejected_steps = 0;

	return var;
}



/* Terminate a set of equations. */
void
rke_term(rke_variables var)
/**************************************************************************
rke_term - terminate a set of equations in a call to RKE routines
***************************************************************************
Author:	François Pinard <pinard@iro.umontreal.ca>, 1988.
***************************************************************************/
/* Reentrency block */
{
	free (var);
}

/* Main routine of the module, ODE solver. */
/* Perform a consistent move of time in the system. */
int				/* !0 if success */
rke_solve (rke_variables var, double *time,
		double *variables,double aimed_time)
/***************************************************************************
rke_variables var		 Reentrency block
double *time			 Current value of time
double variables[]		 Current variables
double aimed_time		 Value of time which is aimed for
****************************************************************************
Author:	François Pinard <pinard@iro.umontreal.ca>, 1988.
***************************************************************************/
{
	double whole_step;		/* Signed integration step size */
	double quarter_step;		/* 0.25 * whole_step */
	double half_step;		/* 0.50 * whole_step */
	double three_quarter_step;	/* 0.75 * whole_step */
	double estimated_error;	/* Error as estimated by England method */
	double allowable_error;	/* Maximum error that user permits */
	int within_tolerance;	/* Allowable error has not been passed */
	int all_errors_small;	/* All errors within 2% of tolerances */
	int length_of_array;	/* Length of temporary arrays, is bytes */
	int k;			/* Index in various arrays */

	double *dp=NULL, *vt=NULL, *v=NULL, *d=NULL;
	double *a1=NULL, *a2=NULL, *a3=NULL, *a4=NULL;
	double *a5=NULL, *a6=NULL, *a7=NULL;

	/* Allocate the work arrays. */
	length_of_array = var->n_equations * sizeof (double);

	dp = (double *) ealloc1double (length_of_array);
	vt = (double *) ealloc1double (length_of_array);
	v	= (double *) ealloc1double (length_of_array);
	d	= (double *) ealloc1double (length_of_array);
	a1 = (double *) ealloc1double (length_of_array);
	a2 = (double *) ealloc1double (length_of_array);
	a3 = (double *) ealloc1double (length_of_array);
	a4 = (double *) ealloc1double (length_of_array);
	a5 = (double *) ealloc1double (length_of_array);
	a6 = (double *) ealloc1double (length_of_array);
	a7 = (double *) ealloc1double (length_of_array);

	/* The integration will continue if a minimum step could bring the
	 * system closer to the time that is aimed for, even if we have to
	 * overshoot it a little.
         */

	while (2 * fabs(aimed_time - *time) > var->minimum_step) {
		/* Evaluate initial step size and direction. */
		if((whole_step = aimed_time - *time) > 0.0) {
			if(whole_step > var->current_step)
				whole_step = var->current_step;
		} else {
			if(whole_step < - var->current_step)
				whole_step = - var->current_step;
		}

		/* Evaluate initial differentials. */
		if(! (*var->eval_routine) (*time, variables, dp))
						return 0;

		/* Loop integrating at this time point until integration 
		 * error is within tolerances.	In any case, adjust 
		 * integration step size.
		 */
		do {

	 		/* Calculate various step sizes. */
			quarter_step = 0.25 * whole_step;
			half_step = quarter_step + quarter_step;
			three_quarter_step = half_step + quarter_step;

			/* Perform a partial computation for one step of
			 * Runge-Kutta 4th order integration, as far as
			 * necessary to chain it to England method for 
			 * estimating integration errors.
			 */

			for(k=0; k<var->n_equations; ++k) {
				a1[k] = half_step * dp[k];
				v[k] = variables[k] + 0.5*a1[k];
			}

			if(!(*var->eval_routine)
				(*time + quarter_step, v, d)) return 0;

			for(k=0; k< var->n_equations; ++k) {
				a2[k] = half_step * d[k];
				v[k] = variables[k] + 0.25 * (a1[k] + a2[k]);
			}

			if(! (*var->eval_routine)
					(*time + quarter_step, v, d)) return 0;

			for(k=0; k< var->n_equations; ++k) {
				a3[k] = half_step * d[k];
				v[k] = variables[k] + (-a2[k] + a3[k] + a3[k]);
			}

			if(! (*var->eval_routine)
				(*time + half_step, v, d)) return 0;

			for(k=0; k< var->n_equations; ++k) {
				a4[k] = half_step * d[k];
				vt[k] = variables[k]
					+ (a1[k] + 4.0*a3[k] + a4[k])/6.0;
			}

			if(! (*var->eval_routine)
				(*time + half_step, vt, d)) return 0;

			for(k=0; k< var->n_equations; ++k) {
				a5[k] = half_step * d[k];
				v[k] = vt[k] + 0.5*a5[k];
			}

			if(! (*var->eval_routine)
				(*time + three_quarter_step, v, d)) return 0;

			for(k=0; k< var->n_equations; ++k) {
				a6[k] = half_step * d[k];
				v[k] = vt[k] + 0.25*(a5[k] + a6[k]);
			}

			if(! (*var->eval_routine)
				(*time + three_quarter_step, v, d)) return 0;

			for(k=0; k< var->n_equations; ++k) {
				a7[k] = half_step * d[k];
				v[k] = variables[k]
					+ (-a1[k] - 96.0*a2[k] 
						+ 92.0*a3[k] - 121.0*a4[k]
						+ 144.0*a5[k] + 6.0*a6[k] 
						- 12.0*a7[k])/6.0;
			}

			/* Perform England error analysis 
			 *  on partial integration.
			 */

			if(!(*var->eval_routine)
				(*time + whole_step, v, d)) return 0;

			within_tolerance = 1;
			all_errors_small = 1;

			for(k=0; k< var->n_equations; ++k) {
				estimated_error = fabs((-a1[k]	
							+ 4.0*a3[k]
							+ 17.0*a4[k]
							- 23.0*a5[k]
							+ 4.0*a7[k] 
							- half_step*d[k])/90.0);
				allowable_error = fabs(whole_step)
							*(var->error_slope*fabs (vt[k]) + var->error_bias);
				if(estimated_error > allowable_error) {
					within_tolerance = 0;
					break;
				} else if(estimated_error>0.02*allowable_error){
						all_errors_small = 0;
				}
			}
		if(within_tolerance) {
			++var->accepted_steps;

			/* Complete the Runge-Kutta step and return values. */
			for(k=0; k< var->n_equations; ++k) {
				v[k] = vt[k] + (-a6[k] + a7[k] + a7[k]);
			}

			if(!(*var->eval_routine)
				(*time + whole_step, v, d)) return 0;

			*time+=whole_step;

			for(k=0; k<var->n_equations; ++k) {
				variables[k] = vt[k] 
						+ (a5[k] + 4.0*a7[k] 
							+ half_step*d[k])/ 6.0;
			}

			/* Increment step size if desirable. */
			if(all_errors_small
				&& fabs(whole_step) == var->current_step) {
			  if(2 * var->current_step > var->maximum_step) {
				  var->current_step = var->maximum_step;
			  } else { 
				var->current_step *= 2;
			  }
			}
		} else {

			++var->rejected_steps;

			/* Decrement step size if possible. */
			if (fabs (whole_step) > var->minimum_step) {
                  		if (var->current_step < 2 * var->minimum_step)
                    		  var->current_step = var->minimum_step;
                  		else
                    		  var->current_step *= 0.5;
                  		if (aimed_time > *time)
                    		  whole_step = var->current_step;
                  		else
                    		  whole_step = - var->current_step;
                	} else {
                		return 0;       /* Convergence failed */
			}
            	}


		} while (!within_tolerance);
	}
	return 1;			/* Convergence succeeded */
	
}
