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
#include "vect/ll_v3d_data.hh"
#include "vect/trans_3d_to_2d.hh"
#include "vect/vect_3d_data.hh"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

V3dDataElement::V3dDataElement(int pts, float *x, float *y, float *z,
	float degreesZ, float degreesY,
	float xOffset, float xFactor, float yOffset,
	float yFactor, float zOffset, float zFactor,
	float xExp, float yExp, float zExp)
{
	_data3D = new Vect3DData (pts, x, y, z);
	_trans  = new Trans3Dto2D(_data3D, degreesZ, degreesY,
		xOffset, xFactor, yOffset, yFactor, zOffset, zFactor,
		xExp, yExp, zExp);
}

V3dDataElement::~V3dDataElement()
{
	delete _trans ;
	delete _data3D;
}

V3dDataLinkedList::V3dDataLinkedList(const char *inputFile,
	const char *outputFile,  const char *outFormat, V3dFileType type,
	float degreesZ, float degreesY, int xCol, int yCol, int zCol, float nil)
	: _inputFile((char *) NULL), _outputFile((char *) NULL),
	  _inputFileType(type), _degreesZ(degreesZ), _degreesY(degreesY),
	  _xOffset(0.0), _xFactor(1.0), _yOffset(0.0),
	  _yFactor(1.0), _zOffset(0.0), _zFactor(1.0),
	  _xExp(1.0), _yExp(1.0), _zExp(1.0),
	  _xCol(xCol), _yCol(yCol), _zCol(zCol), _nil(nil),
	  _constructorError(0)
{
	assert((char *) NULL != inputFile && NoFile != type);
	_inputFile = new char[strlen(inputFile) + 1];
	strcpy(_inputFile, inputFile);

	if ((char *) NULL == outputFile)
	{
		_outputFile = new char[strlen(_inputFile) + 5];
		strcpy(_outputFile, _inputFile);
		strcat(_outputFile, ".new");
	}
	else
	{
		_outputFile = new char[strlen(outputFile) + 1];
		strcpy(_outputFile, outputFile);
	}

	assert((char *) NULL != outFormat);
	_outFormat = new char[strlen(outFormat) + 1];
	strcpy(_outFormat, outFormat);

	FILE *stream;

	switch (_inputFileType)
	{
		case Ascii:
			stream = fopen(_inputFile, "r");

			if ((FILE *) NULL == stream)
			{
				fprintf(stderr, "Error opening input file %s\n",
					_inputFile);

				_constructorError = 1;
			}
			else
			{
				if (!readAsciiFile(stream))
					_constructorError = 1;

				int check = fclose(stream);
				if (EOF == check)
				{
					fprintf(stderr,
						"Error closing input file %s\n",
						_inputFile);

					_constructorError = 1;
				}
			}
			break;
		default:
			assert(0);
	}
}

V3dDataLinkedList::V3dDataLinkedList(float degreesZ, float degreesY)
	: _inputFile((char *) NULL), _outputFile((char *) NULL),
	  _inputFileType(NoFile), _degreesZ(degreesZ), _degreesY(degreesY),
	  _xOffset(0.0), _xFactor(1.0), _yOffset(0.0),
	  _yFactor(1.0), _zOffset(0.0), _zFactor(1.0),
	  _xExp(1.0), _yExp(1.0), _zExp(1.0),
	  _constructorError(0)
{
	/* do nothing */
}

void V3dDataLinkedList::getInitRanges(float *xMin, float *xMax,
	float *yMin, float *yMax, float *zMin, float *zMax)
{
	*xMin = _xInitMin;
	*xMax = _xInitMax;
	*yMin = _yInitMin;
	*yMax = _yInitMax;
	*zMin = _zInitMin;
	*zMax = _zInitMax;
}

V3dDataLinkedList::~V3dDataLinkedList()
{
	if ((char *) NULL != _inputFile)
		delete _inputFile;

	if ((char *) NULL != _outputFile)
		delete _outputFile;
}

Trans3Dto2D *V3dDataLinkedList::add(int pts, float *x, float *y, float *z)
{
	V3dDataElement *theElement = new V3dDataElement(pts, x, y, z,
		_degreesZ, _degreesY, _xOffset, _xFactor, _yOffset, _yFactor,
		_zOffset, _zFactor, _xExp, _yExp, _zExp);
	BaseLinkedList::add((Element *) theElement);

	return theElement->_trans;
}

void V3dDataLinkedList::setAngles(float degreesZ, float degreesY, int doMod)
{
	_degreesZ = degreesZ;
	_degreesY = degreesY;

	Trans3Dto2D *ptr;
	void *p;
	for (ptr = top(&p); (Trans3Dto2D *) NULL != ptr; ptr = next(&p))
		ptr->setAngles(_degreesZ, _degreesY, doMod);
}

void V3dDataLinkedList::setScale (float xOffset, float xFactor, float yOffset,
	float yFactor, float zOffset, float zFactor)
{
	_xOffset = xOffset;
	_xFactor = xFactor;
	_yOffset = yOffset;
	_yFactor = yFactor;
	_zOffset = zOffset;
	_zFactor = zFactor;

	Trans3Dto2D *ptr;
	void *p;
	for (ptr = top(&p); (Trans3Dto2D *) NULL != ptr; ptr = next(&p))
		ptr->setScale(_xOffset, _xFactor, _yOffset, _yFactor,
			zOffset, _zFactor, 0);
}

void V3dDataLinkedList::setExpansion(float xExp, float yExp, float zExp)
{
	_xExp = xExp;
	_yExp = yExp;
	_zExp = zExp;

	Trans3Dto2D *ptr;
	void *p;
	for (ptr = top(&p); (Trans3Dto2D *) NULL != ptr; ptr = next(&p))
		ptr->setExpansion(_xExp, _yExp, _zExp, 0);
}

int V3dDataLinkedList::saveFile(char *file, V3dFileType type)
{
	int retval = 1;	/* Returns 1 if saves successfully, 0 if error. */
	FILE *inStream, *outStream;

	switch (type)
	{
		case Ascii:
			inStream = fopen(_inputFile, "r");

			const char *outputFilePtr;
			if ((char *) NULL == file)
				outputFilePtr = _outputFile;
			else
				outputFilePtr = file       ;

			outStream = fopen(outputFilePtr, "w");

			if ((FILE *) NULL == inStream)
			{
				fprintf(stderr, "Error opening input file %s\n",
					_inputFile);

				retval = 0;
			}
			else if ((FILE *) NULL == outStream)
			{
				fprintf(stderr, "Error opening output file %s\n"
					, outputFilePtr);

				retval = 0;
			}
			else
			{
				if (!saveAsciiFile(inStream, outStream))
					retval = 0;
			}

			if ((FILE *) NULL != inStream)
				if (EOF == fclose(inStream))
				{
					fprintf(stderr,
						"Error closing input file %s\n",
						_inputFile);

					retval = 0;
				}

			if ((FILE *) NULL != outStream)
				if (EOF == fclose(outStream))
				{
					fprintf(stderr,
						"Error closing output file %s\n"
						, outputFilePtr);

					retval = 0;
				}

			break;
		default:
			assert(0);
	}

	return retval;
}

int V3dDataLinkedList::findAllVectors(float x, float y, float z,
	Vect3DData ***vectors, int **indices)
{
	int retval = 0;

	Vect3DData **locVectors = (Vect3DData **) NULL;
	int         *locIndices = (int         *) NULL;
	V3dDataElement *ptr;
	void *p;
	Vect3DData *data3D;
	int numPts, i;

	for (ptr = (V3dDataElement *) BaseLinkedList::top(&p);
		ptr;
		ptr = (V3dDataElement *) BaseLinkedList::next(&p))
	{
		data3D = ptr->_data3D;
		numPts = data3D->getNumPts();

		for (i = 0; i < numPts; i++)
			if ((data3D->getX(i) == x)
			 && (data3D->getY(i) == y)
			 && (data3D->getZ(i) == z))
			{
				/*
				 * Should be able to just use ANSI realloc, but
				 * SUN screws up when reallocing NULL array.
				 */
				if (locVectors)
				{
					assert(locVectors = (Vect3DData **)
						realloc((void *) locVectors,
						(size_t) (retval + 1) *
						sizeof(Vect3DData *)));
					assert(locIndices = (int *)
						realloc((void *) locIndices,
						(size_t) (retval + 1) *
						sizeof(int)));
				}
				else
				{
					assert(locVectors = (Vect3DData **)
						malloc(sizeof(Vect3DData *)));
					assert(locIndices = (int *)
						malloc(sizeof(int)));
				}

				locVectors[retval  ] = data3D;
				locIndices[retval++] = i;
			}
	}

	if (0 < retval)
	{
		/*
		 * Switch to newed arrays so C++ will surely handle.
		 */
		*vectors = new Vect3DData *[retval];
		*indices = new int         [retval];

		for (i = 0; i < retval; i++)
		{
			(*vectors)[i] = locVectors[i];
			(*indices)[i] = locIndices[i];
		}

		free((void *) locVectors);
		free((void *) locIndices);
	}

	return retval;
}

const char *getStringFromCommandLine(const char *specifier,
	const char *defaultString, int argc, const char * const *argv)
{
	const char *retval = V3dDataLinkedList::getFromCommandLine(
		specifier, argc, argv);

	if ((const char *) NULL == retval)
		retval = defaultString;

	return retval;
}

int getIntFromCommandLine(const char *specifier, int defaultInt,
	int argc, const char * const *argv)
{
	int retval;

	const char *string = V3dDataLinkedList::getFromCommandLine(
		specifier, argc, argv);

	if ((const char *) NULL == string)
	{
		retval = defaultInt;
	}
	else
	{
		if (!V3dDataLinkedList::stringToInt(string, &retval))
		{
			fprintf(stderr,
"Warning:  invalid int %s for %s, default of %d will be used\n",
				string, specifier, defaultInt);

			retval = defaultInt;
		}
	}

	return retval;
}

float getFloatFromCommandLine(const char *specifier, float defaultFloat,
	int argc, const char * const *argv)
{
	float retval;

	const char *string = V3dDataLinkedList::getFromCommandLine(
		specifier, argc, argv);

	if ((const char *) NULL == string)
	{
		retval = defaultFloat;
	}
	else
	{
		if (!V3dDataLinkedList::stringToFloat(string, &retval))
		{
			fprintf(stderr,
"Warning:  invalid float %s for %s, default of %f will be used\n",
				string, specifier, defaultFloat);

			retval = defaultFloat;
		}
	}

	return retval;
}

int getBoolFromCommandLine(const char *specifier, int defaultBool,
	int argc, const char * const *argv)
{
	int retval;

	/*
	 * isOnCommandLine returns numbers of times, not just 1 or 0.
	 */
	if (V3dDataLinkedList::isOnCommandLine(specifier, argc, argv))
		retval = 1;
	else
		retval = defaultBool;

	return retval;
}

int getStringArrayFromCommandLine(const char *startSpecifier,
	const char *endSpecifier, int defaultNum, const char **defaultArray,
	const char ***returnArray, int argc, const char * const *argv)
{
	int retval = V3dDataLinkedList::getArrayFromCommandLine(startSpecifier,
		endSpecifier, argc, argv, returnArray);

	if (0 == retval)
	{
		retval = defaultNum;

		/*
		 * Make a copy so client can always safely delete.
		 */
		assert(*returnArray = new const char *[retval]);

		for (int i = 0; i < retval; i++)
			(*returnArray)[i] = defaultArray[i];
	}

	return retval;
}

int V3dDataLinkedList::readAsciiFile(FILE *stream)
{
	int retval = 1;

	/* ANSI spec says should not need +1 for fgets, but play it safe. */
	char line[_MAX_CHARS + 1];

	if (initLineParsing(input))
	{
		/*
		 * _totalLines starts at 1, it refers to current line
		 * since it is used in error messages.
		 */
		for (_linesThisVector = 0, _totalVectorLines = 0,
			_totalLines = 1;
			(char *) NULL != fgets(line, _MAX_CHARS, stream);
			_totalLines++)
		{
			if ('\n' != line[strlen(line) - 1])
			{
				fprintf(stderr,
"Error:  Line %d of input file %s is too long, more than %d chars\n",
					_totalLines, _inputFile,
					_MAX_CHARS - 2);

				retval = 0;
				break;
			}

			if (isComment(line))
				continue;

			if (!parseLineForInput(line))
			{
				retval = 0;
				break;
			}
		}

		_totalLines--;	/* Not needed, but being consistent. */

		if (retval && _linesThisVector != 0)
			flushLineParsing();

		cleanupLineParsing();
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int V3dDataLinkedList::saveAsciiFile(FILE *inStream, FILE *outStream)
{
	int retval = 1;

	/* ANSI spec says should not need +1 for fgets, but play it safe. */
	char  inputLine [_MAX_CHARS + 1];
	char  outputLine[_MAX_CHARS + 1];
	char *outputLinePtr;

	if (initLineParsing(output))
	{
		Trans3Dto2D *trans = top(&_p);
		if ((Trans3Dto2D *) NULL == trans)
			_current3DData = (Vect3DData *) NULL;
		else
			_current3DData = (Vect3DData *) trans->getData();

		/*
		 * _totalLines starts at 1, it refers to current line
		 * since it is used in error messages.
		 */
		for (_linesThisVector = 0, _totalVectorLines = 0,
			_totalLines = 1;
			(char *) NULL != fgets(inputLine, _MAX_CHARS, inStream);
			_totalLines++)
		{
			if ('\n' != inputLine[strlen(inputLine) - 1])
			{
				fprintf(stderr,
"Error:  Line %d of input file %s is too long, more than %d chars\n",
					_totalLines, _inputFile,
					_MAX_CHARS - 2);

				retval = 0;
				break;
			}

			if (isComment(inputLine))
			{
				outputLinePtr = inputLine;
			}
			else
			{
				if (parseLineForOutput(inputLine, outputLine))
				{
					outputLinePtr = outputLine;
				}
				else
				{
					retval = 0;
					break;
				}
			}

			if (EOF == fputs(outputLinePtr, outStream))
			{
				fprintf(stderr,
"Error:  Error writing line %d to output file\n",
					_totalLines, 2);

				retval = 0;
				break;
			}
		}

		_totalLines--;	/* Not needed, but being consistent. */

		cleanupLineParsing();
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int V3dDataLinkedList::initLineParsing(parsingType type)
{
	int retval = 1;

	if (_xCol == _yCol)
	{
		fprintf(stderr, "Error:  x and y are both set to column %d\n",
			_xCol);
		retval = 0;
	}

	if (_xCol == _zCol)
	{
		fprintf(stderr, "Error:  x and z are both set to column %d\n",
			_xCol);
		retval = 0;
	}

	if (_yCol == _zCol)
	{
		fprintf(stderr, "Error:  y and z are both set to column %d\n",
			_yCol);
		retval = 0;
	}

	if (1 == retval)
	{
		switch (type)
		{
			case input:
				/* Use malloc so we can use realloc. */
				_allocatedSize   = (size_t) _ALLOCATION_INC;
				assert(_xArray = (float *) malloc(
					sizeof(float) * _allocatedSize));
				assert(_yArray = (float *) malloc(
					sizeof(float) * _allocatedSize));
				assert(_zArray = (float *) malloc(
					sizeof(float) * _allocatedSize));
				break;
			case output:
				_xArray = _yArray = _zArray = (float *) NULL;
				break;
			default:
				assert(0);
		}
				

		_maxTokens = _xCol;

		if (_yCol > _maxTokens)
			_maxTokens = _yCol;

		if (_zCol > _maxTokens)
			_maxTokens = _zCol;

		_tokenPtr = new float *[_maxTokens];

		for (int i = 0; i < _maxTokens; i++)
			_tokenPtr[i] = (float *) NULL;

		_tokenPtr[_xCol - 1] = &_x;
		_tokenPtr[_yCol - 1] = &_y;
		_tokenPtr[_zCol - 1] = &_z;
	}

	return retval;
}

int V3dDataLinkedList::parseLineForInput(char *line)
{
	int retval, tokens;
	char *ptr;
	static const char between[] = " ,\t\n";

	for (retval = 1, ptr = strtok(line, between), tokens = 0;
		(char *) NULL != ptr && tokens < _maxTokens;
		ptr = strtok((char *) NULL, between), tokens++)
	{
		if ((float *) NULL != _tokenPtr[tokens])
			if (!stringToFloat(ptr, _tokenPtr[tokens]))
			{
				fprintf(stderr,
"Error:  invalid float %s in line %d of input file %s\n",
					ptr, _totalLines, _inputFile);
				retval = 0;
				break;
			}
	}

	if (1 == retval)
	{
		if (0 == tokens)		/* blank line = end of vector */
		{
			if (_linesThisVector > 0)
				flushLineParsing();
		}
		else if (tokens < _maxTokens)	/* not enough tokens */
		{
			fprintf(stderr,
"Error:  only %d of required %d values found in line %d of input file %s\n",
				tokens, _maxTokens,  _totalLines, _inputFile);
			retval = 0;
		}
		else				/* next data point */
		{
			if (0 == _totalVectorLines)
			{
				_xInitMin = _xInitMax = _x;
				_yInitMin = _yInitMax = _y;
				_zInitMin = _zInitMax = _z;
			}
			else
			{
				if (_x != _nil)
				{
					if (_xInitMin == _nil || _x < _xInitMin)
						_xInitMin = _x;

					if (_xInitMax == _nil || _x > _xInitMax)
						_xInitMax = _x;
				}

				if (_y != _nil)
				{
					if (_yInitMin == _nil || _y < _yInitMin)
						_yInitMin = _y;

					if (_yInitMax == _nil || _y > _yInitMax)
						_yInitMax = _y;
				}

				if (_z != _nil)
				{
					if (_zInitMin == _nil || _z < _zInitMin)
						_zInitMin = _z;

					if (_zInitMax == _nil || _z > _zInitMax)
						_zInitMax = _z;
				}
			}

			_linesThisVector++;
			_totalVectorLines++;

			if (_linesThisVector > _allocatedSize)
			{
				_allocatedSize += (size_t) _ALLOCATION_INC;
				assert(_xArray = (float *) realloc(_xArray,
					sizeof(float) * _allocatedSize));
				assert(_yArray = (float *) realloc(_yArray,
					sizeof(float) * _allocatedSize));
				assert(_zArray = (float *) realloc(_zArray,
					sizeof(float) * _allocatedSize));
			}

			_xArray[_linesThisVector - 1] = _x;
			_yArray[_linesThisVector - 1] = _y;
			_zArray[_linesThisVector - 1] = _z;
		}
	}

	return retval;
}

int V3dDataLinkedList::parseLineForOutput(char *inputLine, char *outputLine)
{
	int retval;
	int shouldBeBlankLine;	/* used as boolean */

	/*
	 * Assumes all Vect3DData classes have > 0 pts.
	 * With 0 pts, how would they have been read in from file?
	 */
	if ((Vect3DData *) NULL == _current3DData ||
		_current3DData->getNumPts() == _linesThisVector)
	{
		shouldBeBlankLine = 1;
	}
	else
	{
		_x = _current3DData->getXWithNil(_linesThisVector);
		_y = _current3DData->getYWithNil(_linesThisVector);
		_z = _current3DData->getZWithNil(_linesThisVector);

		shouldBeBlankLine = 0;
	}

	int tokens, outputLength, tokenLength, numSpaces;
	char *ptr, *outPtr;
	static const char between[] = " ,\t\n";
	char buff[_MAX_CHARS + 1];

	for (retval = 1, ptr = strtok(inputLine, between), tokens = 0,
		outputLine[0] = '\0', outputLength = 0, numSpaces = 0;
		(char *) NULL != ptr;
		ptr = strtok((char *) NULL, between), tokens++)
	{
		if (shouldBeBlankLine)
		{
			fprintf(stderr,
				"Error:  extra line %d in input file %s\n",
				_totalLines, _inputFile);

			retval = 0;
			break;
		}
		else
		{
			if (tokens < _maxTokens
				&& (float *) NULL != _tokenPtr[tokens])
			{
				sprintf(buff, _outFormat, *_tokenPtr[tokens]);
				outPtr = buff;
			}
			else
			{
				outPtr = ptr;
			}

			/*
			 * Use tokenLength so assert can be checked
			 * before strcpy.
			 */
			tokenLength = (int) strlen(outPtr);
			assert(outputLength + tokenLength + numSpaces
				<= _MAX_CHARS);

			/*
			 * Space between tokens, but no leading space.
			 */
			if (0 < numSpaces)
				strcpy(outputLine + outputLength++ , " ");
			else
				numSpaces = 1;

			strcpy(outputLine + outputLength, outPtr);
			outputLength += tokenLength;
		}
	}

	if (0 == retval)		/* error detected in for loop */
	{
	}
	else if (0 == tokens)		/* blank line = end of vector */
	{
		if (_linesThisVector > 0)
		{
			if (shouldBeBlankLine)
			{
				Trans3Dto2D *trans = next(&_p);
				if ((Trans3Dto2D *) NULL == trans)
					_current3DData = (Vect3DData *) NULL;
				else
					_current3DData =
						(Vect3DData *) trans->getData();

				_linesThisVector = 0;
			}
			else
			{
				fprintf(stderr,
"Error:  missing line %d of input file %s\n",
					_totalLines, _inputFile);
				retval = 0;
			}
		}
		else
		{
			/* Do nothing, extra blank line. */
		}
	}
	else if (tokens < _maxTokens)	/* not enough tokens */
	{
		fprintf(stderr,
"Error:  only %d of required %d values found in line %d of input file %s\n",
			tokens, _maxTokens,  _totalLines, _inputFile);
		retval = 0;
	}
	else				/* next data point */
	{
		_linesThisVector++;
		_totalVectorLines++;
	}

	assert(outputLength < _MAX_CHARS);
	outputLine[outputLength++] = '\n';
	outputLine[outputLength  ] = '\0';

	return retval;
}

void V3dDataLinkedList::flushLineParsing()
{
	assert(_linesThisVector != 0);

	add(_linesThisVector, _xArray, _yArray, _zArray);

	_linesThisVector = 0;
}

void V3dDataLinkedList::cleanupLineParsing()
{
	if ((float *) NULL != _xArray)
		free(_xArray);
	if ((float *) NULL != _yArray)
		free(_yArray);
	if ((float *) NULL != _zArray)
		free(_zArray);

	delete [] _tokenPtr;
}

int V3dDataLinkedList::isComment(char *line)
{
	return strlen(line) > 0 && '#' == line[0];
}

const char *V3dDataLinkedList::getFromCommandLine(const char *specifier,
	int argc, const char * const *argv)
{
	const char *retval = (const char *) NULL;

	for (int i = 1; i < argc; i++)
	{
		if (0 == strcmp(specifier, argv[i]))
		{
			i++;	/* string is next */

			if ((const char *) NULL == retval)
			{
				if (i < argc)
					retval = argv[i];
				else
					fprintf(stderr,
"Warning:  command line argument %s with no value, default will be used\n",
						specifier);
			}
			else
			{
				fprintf(stderr,
"Warning:  command line argument %s entered more than once, 1st will be used\n",
					specifier);
			}
		}
	}

	return retval;
}

int V3dDataLinkedList::isOnCommandLine(const char *specifier,
	int argc, const char * const *argv)
{
	int retval = 0;

	for (int i = 1; i < argc; i++)
		if (0 == strcmp(specifier, argv[i]))
			if (retval++)
				fprintf(stderr,
"Warning:  command line argument %s entered more than once\n", specifier);

	return retval;
}

int V3dDataLinkedList::getArrayFromCommandLine(const char *startSpecifier,
	const char *endSpecifier, int argc, const char * const *argv,
	const char ***returnArray)
{
	int retval = 0;
	int startIndex, i;
	enum { before, during, after} state = before;

	for (i = 1; i < argc; i++)
		     if (0 == strcmp(startSpecifier, argv[i]))
		{
			switch (state)
			{
				case before:
					state = during;
					break;
				case during:
					fprintf(stderr,
"Warning:  array start specifier %s entered within array, \n\
it will be input as array member\n", startSpecifier);
					if (0 == retval++)
						startIndex = i;
					break;
				case after:
					fprintf(stderr,
"Warning:  array start specifier %s entered twice, 2nd is ignored\n",
						startSpecifier);
					break;
				default:
					assert(0);
			}
		}
		else if (0 == strcmp(  endSpecifier, argv[i]))
		{
			switch (state)
			{
				case before:
					fprintf(stderr,
"Warning:  array end specifier %s entered before start specifier %s, ignored\n",
						endSpecifier, startSpecifier);
					break;
				case during:
					state = after;
					break;
				case after:
					fprintf(stderr,
"Warning:  array end specifier %s entered twice, 2nd is ignored\n",
						endSpecifier);
					break;
				default:
					assert(0);
			}
		}
		else
		{
			switch (state)
			{
				case before:
				case after:
					/* do nothing */
					break;
				case during:
					if (0 == retval++)
						startIndex = i;
					break;
				default:
					assert(0);
			}
		}

	switch (state)
	{
		case before:
			/* do nothing */
			break;
		case during:
			fprintf(stderr,
"Warning:  command line ends during array, end specifier %s is assumed\n",
				endSpecifier);
			/* no break */
		case after:
			if (retval)
			{
				assert(*returnArray = new const char *[retval]);

				for (i = 0; i < retval; i++)
					(*returnArray)[i] = argv[i+startIndex];
			}
			else
			{
				fprintf(stderr,
"Warning:  empty array specified by %s, default will be used\n",
					startSpecifier);
			}
			break;
		default:
			assert(0);
	}


	return retval;
}

int V3dDataLinkedList::stringToInt(const char *string, int *value)
{
	/* assert not needed as string comes from fgets with _MAX_CHARS
	assert(strlen(string) <= _MAX_CHARS); */

	int retval;
	int localValue;
	char junk[_MAX_CHARS + 1];

	if (1 == sscanf((char *) string, "%d%s", &localValue, junk))
	{
		*value = localValue;
		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int V3dDataLinkedList::stringToFloat(const char *string, float *value)
{
	/* assert not needed as string comes from fgets with _MAX_CHARS
	assert(strlen(string) <= _MAX_CHARS); */

	int retval;
	float localValue;
	char junk[_MAX_CHARS + 1];

	if (1 == sscanf((char *) string, "%g%s", &localValue, junk))
	{
		*value = localValue;
		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}
