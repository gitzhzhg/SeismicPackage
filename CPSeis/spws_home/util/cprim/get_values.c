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
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "cprim.h"

int cprim_getI(char *buff, char *prompt, int *value)
{
	int retval = 0;
	char *str, *ptr;

	if (cprim_getStr(buff, prompt, &str))
	{
		retval = -1;
	}
	else
	{
		*value = (int) strtol(str, &ptr, 10);
		if (*ptr != '\0')	/* no extra chars in str */
			retval = -1;
		free((void *) str);
	}

	return retval;
}

int cprim_getF(char *buff, char *prompt, float *value)
{
	int retval = 0;
	char *str, *ptr;

	if (cprim_getStr(buff, prompt, &str))
	{
		retval = -1;
	}
	else
	{
		*value = (float) strtod(str, &ptr);
		if (*ptr != '\0')	/* no extra chars in str */
			retval = -1;
		free((void *) str);
	}

	return retval;
}

int cprim_getStr(char *buff, char *prompt, char **str)
{
	char *ptr;
	int len;

	assert(strlen(prompt));			/* null prompt is of no use */

	ptr = cprim_prompt(buff, prompt);	/* find prompt */
	if (!ptr)
		return -1;

	ptr += strlen(prompt);			/* skip over prompt */
	ptr += strspn(ptr, " \t");		/* skip over white-space */
	if (*ptr++ != '=')			/* next must be = */
		return -1;
	ptr += strspn(ptr, " \t");		/* skip over more white-space */

	len = (int) strcspn(ptr, " \t,\n)");	/* skip over non-white-space */
	if (len == 0)
		return -1;

	*str = (char *) malloc(sizeof(char) * (size_t) (len + 1));
	assert(*str);
	strncpy(*str, ptr, len);
	(*str)[len] = '\0';			/* the terminator */

	return 0;
}

int cprim_nextI(char *buff, int *value)
{
	char *ptr;
	char *str = strtok(buff, " \t,\n)");
	if (!str)
		return -2;

	*value = (int) strtol(str, &ptr, 10);
	if (*ptr != '\0')	/* no extra chars in str */
		return -1;

	return 0;
}

int cprim_nextF(char *buff, float *value)
{
	char *ptr;
	char *str = strtok(buff, " \t,\n)");
	if (!str)
		return -2;

	*value = (float) strtod(str, &ptr);
	if (*ptr != '\0')	/* no extra chars in str */
		return -1;

	return 0;
}

char *cprim_prompt(char *buff, char *prompt)
{
	char *retval = strstr(buff, prompt);
	int len;

	/*
	 * Prompt found?
	 */
	if ((char *) NULL == retval)
		return (char *) NULL;

	/*
	 * Adjacent left char alright?
	 */
	if (!(retval      == buff	/* 1st thing in buff */
	 || *(retval - 1) == ' '
	 || *(retval - 1) == '\t'
	 || *(retval - 1) == ','))
		return (char *) NULL;

	/*
	 * Adjacent right char alright?
	 */
	len = (int) strlen(prompt);
	if (!(*(retval + len) == ' '
	 ||   *(retval + len) == '\t'
	 ||   *(retval + len) == '='))
		return (char *) NULL;

	return retval;
}
