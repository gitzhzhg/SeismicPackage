/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "wproc.h"

/*
 * return = 0:  program found
 * return = 1:  program not found
 * return = 2:  program found, but not executable
 * return = 3:  logical spws_bin, not assigned (VMS only)
 */

int wpAccess(const char *prog)
{
	int retval = 1;
	char *envPath, *path, *progWithPath, *ptr;
        char env_var[30];

#ifndef VMS
        strcpy(env_var, "PATH");
#else
        strcpy(env_var, "SPWS_BIN");
#endif /* VMS */
	envPath = getenv(env_var);

	if (envPath)
	{
		assert(path = malloc(strlen(envPath) + 1));
		strcpy(path, envPath);

		assert(progWithPath = malloc(strlen(path) + strlen(prog) + 2));
#ifndef VMS
		for (ptr = strtok(path, ":"); ptr; ptr = strtok(NULL, ":"))
		{
			strcpy(progWithPath, ptr);
			strcat(progWithPath, "/");
			strcat(progWithPath, prog);

			if (!access(progWithPath, R_OK | X_OK))
			{
				retval = 0;
				break;
			}
			else if (!access(progWithPath, R_OK))
			{
				retval = 2;
			}
		}
#else
		strcpy(progWithPath, path);
		strcat(progWithPath, prog);
		strcat(progWithPath, ".");

		if (!access(progWithPath, R_OK | X_OK))
		{
			retval = 0;
		}
		else if (!access(progWithPath, R_OK))
		{
			retval = 2;
		}
#endif /* VMS */
		free(path);
		free(progWithPath);
	}
	else
	{
#ifdef VMS
		retval = 3;
#else
		assert(0);
#endif /* VMS */
	}

	return retval;
}
