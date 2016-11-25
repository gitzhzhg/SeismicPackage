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
#ifndef _FGXP_DATA_HH
#define _FGXP_DATA_HH

#include "oprim/base_data.hh"
#include "fgxp/fgxp_constants.hh"

class FgXpData : public BaseData
{
	friend class FgXpDataElement;

	public:

		/*
		 * These methods are used in scaling.
		 */
		void getRange(float *xMin, float *xMax,
			float *yMin, float *yMax,
			float *zMin = (float *) 0,
			float *zMax = (float *) 0);
		int getFirstPnt(float *x, float *y, float *z = (float *) 0);

		/*
		 * Virtual functions from BaseData.
		 */
		virtual int getNumPts        (       long id = defaultId);
		virtual float getX           (int i, long id = defaultId);
		virtual float getY           (int i, long id = defaultId);
		virtual float getZ           (int i, long id = defaultId);
		virtual int getMarkerType    (int i, long id = defaultId);
		virtual int getAltMarkerColor(int i, long id = defaultId);

		/*
		 * These methods are used by picking classes.
		 */
		void replacePoint(int index, float x, float y, float z = 0.0);
		int translateDataIndexToFlag(int index,
			long **srcIndicesRet = (long **) 0,
			int   *numSrcsRet    = (int   *) 0);
		void adjacentDataIndices(int index, int *index1, int *index2);
		int hasSkids()
			{ return (_numSkiddedSrcs || _numSkiddedRcvs); }

		/*
		 * These are used by the FgInform part of FgXp2DPlot.
		 * Get and set indices used when indexed things are
		 * deleted or inserted.  Set should only be used to
		 * index same data, there is no redraw after a set call.
		 */
		long getXIndex() const
			{ return _xIndex; }
		void setXIndex(long xIndex)
			{ _xIndex = xIndex; }
		long getYIndex() const
			{ return _yIndex; }
		void setYIndex(long yIndex)
			{ _yIndex = yIndex; }
		long getZIndex() const
			{ return _zIndex; }
		void setZIndex(long zIndex)
			{ _zIndex = zIndex; }

		/*
		 * These are also used by the FgInform part of FgXp2DPlot.
		 */
		void pointsRemoved         (int index, int num);
		void pointsInserted        (int index, int num);
		void srcRcvChanged         (int index, int num);
		void depSelActChanged      (int index, int num);
		void depSelActFlush        (                  );
		void forgetDepSelActChanged(                  );
		void saveLineNumber        (                  );
		int  restoreLineNumber     (                  );
		void setSkids();

		/*
		 * These methods are for displaying data.
		 */
		int numReceiversAtLine    (     );
		int numSourcesAtLine      (     );
		int numActiveAtLine       (     );
		int numSelectedAtLine     (     );
		void selectPoint          (int i);
		void activatePoint        (int i);

		/*
		 * These methods are for changing flag colors.
		 */
		void  storeMarkerColors();
		void updateMarkerColors();

		int couldHaveSkids();

	private:

		/*
		 * _list is to get FieldGeometry, CardType, and DataType.
		 */
		class FgXpDataLinkedList *_list;

		/*
		 * Index is line index for LdCardType
		 * and gather index for HeaderWordType.
		 */
		long _xIndex, _yIndex, _zIndex;

		int _savingLine;
		long _saveXLine, _saveYLine, _saveZLine;

		int _depSelActChanged, _depSelActIndex, _depSelActNum;
		long _id;

		static int *_markerColors ;
		static int *_markerIndices;
		static int  _arraySizes   ;

		int   _numSkiddedSrcs, _numSkiddedRcvs;
		int  *_skiddedSrcs  ;
		int  *_skiddedSrcGrp;
		int  *_skiddedRcvs  ;
		float _skiddedY, _skiddedZ;

		/*
		 * Private constuctor and destructor, only accessible by
		 * friend FgXpDataLinkedList.
		 */
		/*
		 * Index is line index for LdCardType
		 * and gather index for HeaderWordType.
		 * -1 for yIndex or zIndex indicates same as previous index.
		 */
		FgXpData(class FgXpDataLinkedList *list,
			long xIndex, long yIndex = -1, long zIndex = -1,
			long id = defaultId);

		virtual ~FgXpData();

		int getNumFlags();
		int getXYZNumPts(CardType cardType, long index);
		int numReceiversAtPoint(int i, int priorSkiddedSrcs,
			int priorSkiddedRcvs);
		int numSourcesAtPoint  (int i, int priorSkiddedSrcs,
			int priorSkiddedRcvs);
		int numDependenciesAtPoint(int i);
		int numSelectedAtPoint    (int i);
		int numActiveAtPoint      (int i);
		float getValue(CardType cardType, int dataType,
			long index, int i, long id);
		void  setValue(CardType cardType, int dataType, long index,
			int i, float value);
		void checkArraySizes(int num);
		float getSkiddedValue(CardType cardType, int dataType,
			long index, int i, long id);
		float getValueWithPriorSrcSkids(int priorSkiddedSrcs,
			CardType cardType, int dataType, long index, int i,
			long id);
		float getValueWithPriorRcvSkids(int priorSkiddedRcvs,
			CardType cardType, int dataType, long index, int i,
			long id);
		float getValueWithBothPriorSkids(
			int priorSkiddedSrcs, int priorSkiddedRcvs,
			CardType cardType, int dataType, long index, int i,
			long id);
		float getUnskiddedValueWithPriorSkids(int priorSkids,
			CardType cardType, int dataType,
			long index, int i, long id);
		static int bs(int *array, int num, int value);
		void flagToDataIndex(int flagIndex, int numFlagIndices,
			int *dataIndex, int *numDataIndices);

		FgXpData()
			{ /* private, no access to default constructor */ }
		FgXpData(FgXpData &)
			{ /* private, no access to copy constructor */ }
		FgXpData& operator=(FgXpData &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FGXP_DATA_HH */
