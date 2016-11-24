*********************************************************
*							*
*	TRISO: Ray Tracing over Tabular Transversely 	*
*	       Isotropic Media. 			*
*							*
*	Author: Sebastien Geoltrain			*
*							*
*	Copyrights: Center for Wave Phenomena,		*
*		    Mathematics Department,		*
*		    Colorado School of Mines,		*
*		    Golden, CO 80401.			*
*							*
*	All Rights Reserved.				*
*							*
*********************************************************

	integer stdin, stdout, stderr, temp1, temp2, temp3
	common /io/ stdin, stdout, stderr, temp1, temp2, temp3

*	Definition of logical unit numbers
*	... to be modified to comply with your system


	stdin = 5
	stdout = 6
	stderr = 0
        temp1 = 10
        temp2 = 20
        temp3 = 30

*	call to the ray tracing code

	call cshotprof()

	stop
	end
