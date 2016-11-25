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
#include "oprim/history.hh"

#include "cprim.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

History::History(char *file, char *process)
	: _records(0), _constructorError(0), _constructorInfo((char *) NULL)
{
	int len = (int) strlen(file);
	if (file[len - 4] != '.' || file[len - 3] != 'b'
	 || file[len - 2] != 'y' || file[len - 1] != 't')
	{
		_constructorError = -1;
		static char *error = "Input file not a byt file";
		_constructorInfo = error;
		return;
	}

	char *fileName = new char[len + 2];
	strcpy(fileName, file);
	fileName[len - 3] = '\0';
	strcat(fileName, "glbl");
	FILE *stream = fopen(fileName, "r");
	delete [] fileName;

	if (!stream)
	{
		_constructorError = -1;
		static char *error = "Can not open glbl file";
		_constructorInfo = error;
		return;
	}

	enum { looking4Current, looking4Process, looking4End, done } state;
	char buff[BUFF_SIZE];

	for (state = looking4Current, _records = 0;
		state != done && fgets(buff, BUFF_SIZE, stream);)
	{
		if (buff[strlen(buff) - 1] != '\n')	/* record too long */
		{
			fclose(stream);
			_constructorError = -1;
			static char *error = "Record too long in glbl file";
			_constructorInfo = error;
			return;
		}


		switch (state)
		{
			case looking4Current:
				if (strstr(buff, "Current history record."))
					state = looking4Process;
				break;
			case looking4Process:
                           //Check old byte file formats
			   if( (strstr(buff,"**** CRAY PROCESSING SYSTEM ****") ||
                                strstr(buff,"HISTORY IPN"))
				 && !strncmp(buff, process, strlen(process))
				 && buff[strlen(process)] == ' ')
				{
					state = looking4End;
				}
                           //Check new byte file formats
                           else if( strstr(buff,"HISTORY IPN")
				 && strstr(buff,"PROCESS") 
                                 && strstr(buff,process)
				 && buff[strlen(process)] == '*')
				{
					state = looking4End;
				}
				break;
			case looking4End:
				if (strstr(buff,
					"**** CRAY PROCESSING SYSTEM ****"))
				{
					state = done;
				}
                                else if (strstr(buff,"HISTORY IPN"))
                                  {
                                        state = done;
                                  }
				else
				{
					addToHistory(buff);
				}
				break;
			case done:
			default:
				assert(0);
		}
	}

	if (state != done && !feof(stream))
	{
		fclose(stream);
		_constructorError = -1;
		static char *error = "Error reading glbl file";
		_constructorInfo = error;
		return;
	}

	if (EOF == fclose(stream))
	{
		fclose(stream);
		_constructorError = -1;
		static char *error = "Error closing glbl file";
		_constructorInfo = error;
		return;
	}

	static char *error1 = "No current history in glbl file";
	static char *error2 = "Process not found in glbl file history";

	switch (state)
	{
		case looking4Current:
			assert(0 == _records);
			_constructorError = -1;
			_constructorInfo = error1;
			break;
		case looking4Process:
			assert(0 == _records);
			_constructorError = -1;
			_constructorInfo = error2;
			break;
		case looking4End:
		case done:
			_constructorError = 0;
			break;
		default:
			assert(0);
	}
}

History::~History()
{
	for (int i = 0; i < _records; i++)
		free((void *) _history[i]);

	if (_records)
		free((void *) _history);
}

void History::addToHistory(char *buff)
{
	if (_records++)
		_history = (char **) realloc((void *) _history,
			(size_t) _records * sizeof(char *));
	else
		_history = (char **) malloc(sizeof(char *));

	assert(_history);

	(_history)[_records - 1] = (char *) malloc(strlen(buff) + (size_t) 1);
	assert((_history)[_records - 1]);
	strcpy((_history)[_records - 1], buff);
}

int History::constructedOK(char **info)
{
	*info = _constructorInfo;
	return _constructorError;
}

int History::getInt(char *prompt, int *value)
{
	int i;
	for (i = 0; i < _records; i++)
		if (0 == cprim_getI(_history[i], prompt, value))
			break;

	if (i == _records)
		return -1;
	else
		return  0;
}

int History::getFloat(char *prompt, float *value)
{
	int i;
	for (i = 0; i < _records; i++)
		if (0 == cprim_getF(_history[i], prompt, value))
			break;

	if (i == _records)
		return -1;
	else
		return  0;
}

int History::getStr(char *prompt, char **value)
{
	char *locValue;
	int i;
	for (i = 0; i < _records; i++)
		if (0 == cprim_getStr(_history[i], prompt, &locValue))
			break;

	if (i == _records)
	{
		return -1;
	}
	else
	{
		/*
		 * To be safe, switch form malloc/free to new/delete.
		 */
		*value = new char[(int) strlen(locValue) + 1];
		strcpy(*value, locValue);
		free((void *) locValue);

		return  0;
	}
}

int History::getFloatArray(char *prompt, int *count, float **values)
{
	assert(strlen(prompt));			/* null prompt is of no use */

	char *ptr;

	int record;
	for (record = 0; record < _records; record++)
	{
		ptr = cprim_prompt(_history[record], prompt); /* find prompt */
		if (ptr)
			break;
	}

	if (record == _records)
		return -1;

	ptr += strlen(prompt);			/* skip over prompt */
	ptr += strspn(ptr, " \t");		/* skip over white-space */
	if (*ptr++ != '=')			/* next must be = */
		return -1;

	/*
	 * Might skip a line before (
	 */
	for (ptr += strspn(ptr, " \t"); *ptr == '\n'; ptr += strspn(ptr, " \t"))
	{
		record++;	/* try next record */

		if (record == _records)	/* at end of history */
			return -1;
		else
			ptr = _history[record];
	}

	if (*ptr++ != '(')			/* next must be ( */
		return -1;
	ptr += strspn(ptr, " \t");		/* skip over more white-space */

	float *temp = (float *) malloc(sizeof(float) * (size_t) INC_SIZE);
	assert(temp);
	int tempSize = INC_SIZE;
	char buff[BUFF_SIZE];
	int len;
	char *endOfNumber;

	for (*count = 0; *ptr != ')';)
	{
		/* skip over non-white-space */
		len = (int) strcspn(ptr, " \t,\n)");
	
		if (len == 0)	/* no non-white space */
		{
			record++;	/* try next record */

			if (record == _records)	/* at end of history */
			{
				free((void *) temp);
				return -1;
			}
			else
			{
				ptr = _history[record];
				/* skip over white-space */
				ptr += strspn(ptr, " \t,");
				continue;
			}
		}

		assert(len < BUFF_SIZE);
		strncpy(buff, ptr, len);
		buff[len] = '\0';		/* the terminator */

		(*count)++;
		if (*count > tempSize)
		{
			tempSize += INC_SIZE;
			temp = (float *) realloc((void *) temp,
				(size_t) tempSize * sizeof(float));
		}

		temp[*count - 1] = strtod(buff, &endOfNumber);
		if (*endOfNumber != '\0')	/* no extra chars in number */
		{
			free((void *) temp);
			return -1;
		}

		ptr += len;			/* skip over number */
		ptr += strspn(ptr, " \t,");	/* skip over white-space */
	}

	*values = new float[*count];

	for (int i = 0; i < *count; i++)
		(*values)[i] = temp[i];

	free((void *) temp);

	return 0;
}

int History::getPairedFloatArrays(char *prompt, int *count,
	float **values1, float **values2)
{
	int    unpairedCount ;
	float *unpairedValues;

	if (getFloatArray(prompt, &unpairedCount, &unpairedValues))
	{
		return -1;
	}
	else
	{
		if (unpairedCount % 2)	/* odd number cannot be pairs */
		{
			delete [] unpairedValues;
			return -1;
		}
		else
		{
			*count = unpairedCount / 2;
			*values1 = new float[*count];
			*values2 = new float[*count];

			for (int i = 0, j = 0; i < *count; i++)
			{
				(*values1)[i] = unpairedValues[j++];
				(*values2)[i] = unpairedValues[j++];
			}

			delete [] unpairedValues;
			return 0;
		}
	}
}
