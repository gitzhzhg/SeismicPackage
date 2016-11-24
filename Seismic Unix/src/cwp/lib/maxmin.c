/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*************************************************************************
maxmin - subroutines that pertain to maximum and minimum values
min_index - find the value of the index where an array is a minimum

**************************************************************************
function prototypes:
int max_index(int n, float *a,int inc);
int min_index(int n, float *a,int inc);

*************************************************************************
Author: Balasz Nemeth: Potash Corporation, given to CWP in 2008
**************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

int max_index(int n, float *a,int inc)
/************************************************************************
max_index - find the value of the index where an array is a maximum
*************************************************************************
Input:
n	number of values in array
a	array
Returns:
im 	max index value

*************************************************************************
Author: Balasz Nemeth
*************************************************************************/
{
        int im=0,i;
        for(i=1;i<n;i+=inc)
                if(a[im]< a[i]) im=i;
        return(im);
}


int min_index(int n, float *a,int inc)
/************************************************************************
min_index - find the value of the index where an array is a minimum
*************************************************************************
Input:
n	number of values in array
a	array
Returns:
im 	max index value
*************************************************************************
Author: Balasz Nemeth
*************************************************************************/
{
        int im=0,i;
        for(i=1;i<n;i+=inc)
                if(a[im]> a[i]) im=i;
        return(im);
}

