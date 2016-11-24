/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/**********************************************************************
Tetra - Tetrahedra subroutines

tetra_volume - find volume of tetrahedra give 4 points
area3d - find area of a triangle in 3D defined by 3 points

***********************************************************************
Function Prototypes:
float tetra_volume(float x0[3], float x1[3], float x2[3], float x3[3]);
float area3d( float x0[3], float x1[3], float x2[3]);
***********************************************************************
tetra_volume:
Input:
x0[3]	x,y,z of 1st point
x1[3]	x,y,z of 2nd point
x2[3]	x,y,z of 3rd point
x3[3]	x,y,z of 4th point

Returns:  tetrahedra volume

area_3d:
Input:
x0[3]		x,y,z of 1st point
x1[3]		x,y,z of 2nd point
x2[3]		x,y,z of 3rd point

Returns:
***********************************************************************
Notes:
tetra_volume:
The formula:
                | x0[0]   x0[1]   x0[2]   1 |
                | x1[0]   x1[1]   x1[2]   1 |
           v =  | x2[0]   x2[1]   x2[2]   1 | / 6;
                | x3[0]   x3[1]   x3[2]   1 |

area3d:

***********************************************************************
References:
***********************************************************************
Author: CWP: Zhaobo Meng, October 1997
***********************************************************************/
/**************** end self doc ********************************/

#include "tetra.h"

float tetra_volume(float x0[3], float x1[3], float x2[3], float x3[3])
/**********************************************************************
tetra_volume - find volume of tetrahedra give 4 points
***********************************************************************
Function Prototype:
float tetra_volume(float x0[3], float x1[3], float x2[3], float x3[3]);
***********************************************************************
Input:
x0[3]	x,y,z of 1st point
x1[3]	x,y,z of 2nd point
x2[3]	x,y,z of 3rd point
x3[3]	x,y,z of 4th point

Returns:  tetrahedra volume
***********************************************************************
Notes:

The formula:
                | x0[0]   x0[1]   x0[2]   1 |
                | x1[0]   x1[1]   x1[2]   1 |
           v =  | x2[0]   x2[1]   x2[2]   1 | / 6;
                | x3[0]   x3[1]   x3[2]   1 |

***********************************************************************
Author: CWP: Zhaobo Meng, October 1997
***********************************************************************/
{
      float vv;

      vv=-( x1[0]*(x2[1]*x3[2]-x2[2]*x3[1])
           -x1[1]*(x2[0]*x3[2]-x2[2]*x3[0])
           +x1[2]*(x2[0]*x3[1]-x3[0]*x2[1]));

      vv+=  x0[0]*(x2[1]*x3[2]-x2[2]*x3[1])
           -x0[1]*(x2[0]*x3[2]-x2[2]*x3[0])
           +x0[2]*(x2[0]*x3[1]-x3[0]*x2[1]);

      vv-=  x0[0]*(x1[1]*x3[2]-x1[2]*x3[1])
           -x0[1]*(x1[0]*x3[2]-x1[2]*x3[0])
           +x0[2]*(x1[0]*x3[1]-x3[0]*x1[1]);

      vv+= x0[0]*(x1[1]*x2[2]-x1[2]*x2[1])
           -x0[1]*(x1[0]*x2[2]-x1[2]*x2[0])
           +x0[2]*(x1[0]*x2[1]-x2[0]*x1[1]);

      vv=ABS(vv)/6.0;
      return vv;
}

float area3d( float x0[3], float x1[3], float x2[3])
/***********************************************************************
area3d - area of a triangle in 3D defined by 3 points

************************************************************************
Function Prototype:
float area3d( float x0[3], float x1[3], float x2[3]);

************************************************************************
Input:
x0[3]		x,y,z of 1st point
x1[3]		x,y,z of 2nd point
x2[3]		x,y,z of 3rd point

Returns:
	area of triangle
************************************************************************
Notes:
************************************************************************
References:
************************************************************************
Author: CWP: Zhaobo Meng  October 1997
***********************************************************************/
{
      float d1,d2,d3;
      d1=sqrt((x0[0]-x1[0])*(x0[0]-x1[0])+
              (x0[1]-x1[1])*(x0[1]-x1[1])+
              (x0[2]-x1[2])*(x0[2]-x1[2]));

      d2=sqrt((x0[0]-x2[0])*(x0[0]-x2[0])+
              (x0[1]-x2[1])*(x0[1]-x2[1])+
              (x0[2]-x2[2])*(x0[2]-x2[2]));

      d3=sqrt((x1[0]-x2[0])*(x1[0]-x2[0])+
              (x1[1]-x2[1])*(x1[1]-x2[1])+
              (x1[2]-x2[2])*(x1[2]-x2[2]));

      return sqrt((d1+d2+d3)*(d2+d3)*(d1+d3)*(d1+d2))/12.0;
}

