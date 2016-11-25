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
#ifndef _LL_V3D_DATA_HH
#define _LL_V3D_DATA_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <assert.h>
#include <stdio.h>
#include <stddef.h>
#include <iostream.h>

class V3dDataElement : public Element
{
	friend class V3dDataLinkedList;

	private:

		class Vect3DData  *_data3D;
		class Trans3Dto2D *_trans ;

		V3dDataElement()
			{ assert(0); }
		V3dDataElement(int pts, float *x, float *y, float *z,
			float degreesZ, float degreesY,
			float xOffset, float xFactor, float yOffset,
			float yFactor, float zOffset, float zFactor,
			float xExp, float yExp, float zExp);
		~V3dDataElement();
		int operator ==(void * const trans) const
			{ return((class Trans3Dto2D *) trans == _trans); }
		void print() const
			{ cout << " " << _trans; }
};

class V3dDataLinkedList : public BaseLinkedList
{
	public:

		enum V3dFileType
		{
			Ascii ,
			NoFile
		};

		V3dDataLinkedList(const char *inputFile,
			const char *outputFile = (const char *) NULL,
			const char *outFormat  = (const char *) "%f",
			V3dFileType type = Ascii,
			float degreesZ = 0.0, float degreesY = 0.0,
			int xCol = 1, int yCol = 2, int zCol = 3,
			float nil = -1e-30);
		V3dDataLinkedList(float degreesZ = 0.0, float degreesY = 0.0);
		int constructorError()
			{ return _constructorError; }
		void getInitRanges(float *xMin, float *xMax,
			float *yMin, float *yMax, float *zMin, float *zMax);
		~V3dDataLinkedList();
		class Trans3Dto2D *add(int pts, float *x, float *y, float *z);
		void remove(class V3dData *plot)
			{ BaseLinkedList::remove((void *) plot); }
		V3dDataElement *find(class Trans3Dto2D *trans)
		{
			return (V3dDataElement *)
				BaseLinkedList::find((void *) trans);
		}
		class Trans3Dto2D *top    (void **p = (void **) NULL)
		{
			V3dDataElement *ptr = (V3dDataElement *)
				BaseLinkedList::top    (p);
			return (ptr ? ptr->_trans : (class Trans3Dto2D *) NULL);
		}
		class Trans3Dto2D *bottom (void **p = (void **) NULL)
		{
			V3dDataElement *ptr = (V3dDataElement *)
				BaseLinkedList::bottom (p);
			return (ptr ? ptr->_trans : (class Trans3Dto2D *) NULL);
		}
		class Trans3Dto2D *next   (void **p = (void **) NULL)
		{
			V3dDataElement *ptr = (V3dDataElement *)
				BaseLinkedList::next   (p);
			return (ptr ? ptr->_trans : (class Trans3Dto2D *) NULL);
		}
		class Trans3Dto2D *prev   (void **p = (void **) NULL)
		{
			V3dDataElement *ptr = (V3dDataElement *)
				BaseLinkedList::prev   (p);
			return (ptr ? ptr->_trans : (class Trans3Dto2D *) NULL);
		}
		class Trans3Dto2D *current(void **p = (void **) NULL)
		{
			V3dDataElement *ptr = (V3dDataElement *)
				BaseLinkedList::current(p);
			return (ptr ? ptr->_trans : (class Trans3Dto2D *) NULL);
		}
		void setAngles(float degreesZ, float degreesY, int doMod = 0);
		void getAngles(float *degreesZ, float *degreesY)
			{ *degreesZ = _degreesZ;  *degreesY = _degreesY; }
		void setScale (float xOffset, float xFactor, float yOffset,
			float yFactor, float zOffset, float zFactor);
		void getScale (float *xOffset, float *xFactor, float *yOffset,
			float *yFactor, float *zOffset, float *zFactor)
		{
			*xOffset = _xOffset;  *xFactor = _xFactor;
			*yOffset = _yOffset;  *yFactor = _yFactor;
			*zOffset = _zOffset;  *zFactor = _zFactor;
		}
		void setExpansion(float xExp, float yExp, float zExp);
		void getExpansion(float *xExp, float *yExp, float *zExp)
			{ *xExp = _xExp;  *yExp = _yExp;  *zExp = _zExp; }
		/* Returns 1 if saves successfully, 0 if error. */
		int saveFile(char *file = (char *) NULL,
			V3dFileType type = Ascii);
		int findAllVectors(float x, float y, float z,
			Vect3DData ***vectors, int **indices);
		friend const char *getStringFromCommandLine(
			const char *specifier, const char *defaultString,
			int argc, const char * const *argv);
		friend int getIntFromCommandLine(
			const char *specifier, int defaultInt,
			int argc, const char * const *argv);
		friend float getFloatFromCommandLine(
			const char *specifier, float defaultFloat,
			int argc, const char * const *argv);
		friend int getBoolFromCommandLine(
			const char *specifier, int defaultBool,
			int argc, const char * const *argv);
		friend int getStringArrayFromCommandLine(
			const char *startSpecifier, const char *endSpecifier,
			int defaultNum, const char **defaultArray,
			const char ***returnArray,
			int argc, const char * const *argv);

	private:

		/*
		 * _ALLOCATION_INC should be size_t but enum lets me
		 * set value in header.  _MAX_CHARS is int.
		 */
		enum { _ALLOCATION_INC = 1000 };
		enum { _MAX_CHARS      =  255 };
		enum parsingType { input, output };

		int readAsciiFile(FILE *stream);
		int saveAsciiFile(FILE *inStream, FILE *outStream);
		int initLineParsing(parsingType type);
		int parseLineForInput (char *line);
		int parseLineForOutput(char *inputLine, char *outputLine);
		void flushLineParsing();
		void cleanupLineParsing();
		int isComment(char *line);
		static const char *getFromCommandLine(const char *specifier,
			int argc, const char * const *argv);
		static int isOnCommandLine(const char *specifier,
			int argc, const char * const *argv);
		static int getArrayFromCommandLine(const char *startSpecifier,
			const char *endSpecifier, int argc,
			const char * const *argv, const char ***returnArray);
		static int stringToInt  (const char *string, int   *value);
		static int stringToFloat(const char *string, float *value);
		V3dDataLinkedList()
			{ /* private, no access to default constructor */ }
		V3dDataLinkedList(V3dDataLinkedList &)
			{ /* private, no access to copy constructor */ }
		V3dDataLinkedList& operator=(V3dDataLinkedList &p)
			{ /* private, no access to = */ return p; }

		float _degreesZ, _degreesY;
		float _xOffset, _xFactor, _yOffset, _yFactor,
			_zOffset, _zFactor;
		float _xExp, _yExp, _zExp;
		float _nil;
		char *_inputFile ;
		char *_outputFile;
		char *_outFormat;
		V3dFileType _inputFileType;
		int _xCol, _yCol, _zCol;
		int _constructorError;
		int _linesThisVector, _totalLines, _totalVectorLines;
		size_t _allocatedSize;
		float _x, *_xArray;
		float _y, *_yArray;
		float _z, *_zArray;
		int _maxTokens;
		float **_tokenPtr;
		float _xInitMin, _xInitMax, _yInitMin, _yInitMax,
			_zInitMin, _zInitMax;
		class Vect3DData *_current3DData;
		void *_p;
};

#endif
