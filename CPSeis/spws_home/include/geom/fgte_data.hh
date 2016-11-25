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
#ifndef _FGTE_DATA_HH
#define _FGTE_DATA_HH

#include "geom/fg_inform.hh"
#include "tredfile_object.h"
#include "cprim.h"

typedef struct {
	float *_heads  ;
	char   _code[2];
} FgTeTredStruct;

typedef struct {
	long _iline;
	long _iflag;
} FgTeFlagStruct;

#ifndef NUM_HEAD
#define NUM_HEAD 64
#endif

class FgTeData : public FgInform
{
	public:
	
		FgTeData(FieldGeometry *fg);
		virtual ~FgTeData();
		TredFile *getTredFile();
		void  preUpdate();
		void postUpdate();
		int  getDeadTraceCode (long trace);
		void getDeadTraceCodes(class FgUserAbort *ua);
		void selectSrcFlags(int itred);
		void selectRcvFlags(int itred);
		void selectFlags(void (FgTeData::*func)(int, FgTeFlagStruct *),
			int itred);
		char *getStatusMessage();
		void setDataLoaded(int value);
		void checkStatus();
		int selectsActive();
		int selectsValid ();
		int canEdit  ();
		int canDelete();
	
	protected:

		/*
		 * FgInform virtual functions.
		 */
		virtual void startingChanges(FieldGeometry *fg);
		virtual void finishedChanges(FieldGeometry *fg);
		virtual void freezingDependentUpdates  (FieldGeometry *fg);
		virtual void dependentValuesOutOfDate  (FieldGeometry *fg);
		virtual void  preResumeDependentUpdates(FieldGeometry *fg);
		virtual void postResumeDependentUpdates(FieldGeometry *fg);
		virtual void  preFlagValuesChanged(FieldGeometry *fg, long ixl,
			int ident, long index, long nrem, long nins);
		virtual void postFlagValuesChanged(FieldGeometry *fg, long ixl,
			int ident, long index, long nrem, long nins);
		virtual void receiverGathersOutOfDate(FieldGeometry *fg);
		virtual void midpointGathersOutOfDate(FieldGeometry *fg);

	private:

		typedef enum { Nothing, Data, Headers, Codes }  LoadState;
		typedef enum { Fast, Slow, Dreadful          } SpeedState;
		LoadState  _load ;
		SpeedState _speed;

		static float (FgTeData::*_headerFunc[NUM_HEAD])
			(int trace_index);
		TredFile *_tredFile;
		char *_doptr  [TRED_TABLE_NMAX];
		char *_codeptr[TRED_TABLE_NMAX];
		FgTeTredStruct *_tred_traces;
		int *_used_heads;
		int _num_used_heads;
		float *_heads;
		class FgTraces *_fg_traces;
		class ZtCards  *_zt_cards ;
		int _inform_may_have_changed_hdrs, _inform_out_of_date;
		int _inform_changed_selects, _selects_valid;
		int _its_me;

		int canDoFast();
		int canDoSlow();
		int canDo(int *headers, int num_headers, int select);
		void fastSetHeaders(class FgUserAbort *ua          );
		void slowSetHeaders(class FgUserAbort *ua, int more);
		float headerxx(int trace_index);
		float header09(int trace_index);
		float header10(int trace_index);
		float header26(int trace_index);
		float header27(int trace_index);
		float header28(int trace_index);
		float header29(int trace_index);
		void compare(int itred,
			int *    effected = (int *) NULL,
			int *num_effected = (int *) NULL);
		static int getHeaderIndex(int header, int *headers,
			int num_headers);
		void srcFunc(int itrace, FgTeFlagStruct *flag);
		void rcvFunc(int itrace, FgTeFlagStruct *flag);
		static int compar(const void *element1, const void *element2);
		void headersChanged();
};
	
#endif /* _FGTE_DATA_HH */
