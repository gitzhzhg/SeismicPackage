/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "par.h"


int findSiso (float a1313, float pl, float gx, float gz, float *px, float *pz,
	      float *vgx, float *vgz, float *g11, float *g13, float *g33,
	      int r)
/*****************************************************************************
find isotropic P-root
******************************************************************************
Input:
a1313		stiffness element
pl		tangential slowness component
gx		cos(horizontal/interface)
gz		sin(horizontal/interface)
r		=1 reflection =0 transmission


Output:
*px		pointer to new slowness component
*pz		pointer to new slowness component
*vgx		pointer to new group velocity component
*vgz		pointer to new group velocity component
*gij 		pointers to polarizations
******************************************************************************
Notes:
routine returns 1 if real root found.
******************************************************************************
Credits:  Andreas Rueger, Colorado School of Mines, 02/06/94
******************************************************************************/
{
	
	float pms,pm=0.0,pxd,pzd,vgxd,vgzd;

	pxd  = *px;
	pzd  = *pz;

	/* reflection */
	if(r == 1){
	      pm= -(pzd*gx - gz*pxd);

	/* transmission */
	} else if(r == 0){
	      pms = 1/a1313 -pl*pl;

	      /* overcritical incidence */
	      if(pms < 0.0) return -1;

	      pm   = sqrt(pms);

	} else 
	    err(" wrong mode in findSiso");

	pxd  = pl*gx - pm*gz;
	pzd  = pm*gx + pl*gz;

	vgxd = pxd*a1313;
	vgzd = pzd*a1313;

	*g11 = vgzd*pzd;
	*g33 = vgxd*pxd;
	*g13 = -vgxd*pzd;

	*vgx  = vgxd;
	*vgz = vgzd;
	*px  = pxd;
	*pz  = pzd;

	return 1;
}
