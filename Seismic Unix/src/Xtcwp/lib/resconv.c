/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/******************************************************************************
RESCONV - general purpose resource type converters

XtcwpStringToFloat	convert  string to float in resource

*******************************************************************************
Function Prototype:
void XtcwpStringToFloat (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal);

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 08/28/90
******************************************************************************/
/**************** end self doc ********************************/

#include "Xtcwp/Xtcwp.h"

void XtcwpStringToFloat (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal)
{
	static float result;
	
	/* ensure number of arguments is zero */
	if (*nargs!=(args-args)/*0*/)
		XtWarning("String to Float conversion needs no arguments!");
	
	/* convert string in fromVal to float in toVal */
	if (sscanf((char *)fromVal->addr,"%f",&result)==1) {

		/* toVal points to the result */
		toVal->size = sizeof(float); 
		toVal->addr = (char *)&result;
	
	/* if sscanf fails */
	} else {
		XtStringConversionWarning((char *)fromVal->addr,"Float");
	}
}
