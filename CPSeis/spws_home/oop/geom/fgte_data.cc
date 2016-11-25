#include "geom/fgte_data.hh"
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
#include "geom/fg_constants.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_informer.hh"
#include "geom/fg_traces.hh"
#include "geom/zt_cards.hh"
#include "geom/fg_group.hh"
#include "geom/field_flag.hh"
#include "geom/seis_line.hh"
#include "geom/fg_user_abort.hh"

#include <stdlib.h>
#include <assert.h>

/*
 * _headerFunc is 0-63 to get headers 1-64
 * To add to headers that can be calculated fast:
 *	1.  Write header calculating method
 *      2.  Insert name of method at appropriate place in _headerFunc
 *      3.  Insert header number in sorted order in ok_headers array in
 *		canDoFast method
 */
float (FgTeData::*FgTeData::_headerFunc[NUM_HEAD])(int trace_index) =
{
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::header09, &FgTeData::header10,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::header26,
	&FgTeData::header27, &FgTeData::header28,
	&FgTeData::header29, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx,
	&FgTeData::headerxx, &FgTeData::headerxx
};

FgTeData::FgTeData(FieldGeometry *fg)
	: FgInform(fg),
	  _num_used_heads(0),
	  _load(Nothing),
	  _speed(Fast),	/* can always do nothing fast */
	  _selects_valid(0),
	  _its_me(0)
{
	_tredFile = tredfile_create();

	_fg_traces = _fg->getFgTraces();
	_zt_cards  = _fg->getZtCards ();

	for (int i = 0; i < TRED_TABLE_NMAX; i++)
	{
		_doptr  [i] = _tredFile->_dos  [i];
		_codeptr[i] = _tredFile->_codes[i];
	}
}

FgTeData::~FgTeData()
{
	tredfile_destroy(_tredFile);

	if (_load > Data)
	{
		delete [] _tred_traces;
		delete [] _heads      ;
	}

	if (_num_used_heads)
		delete [] _used_heads ;
}

TredFile *FgTeData::getTredFile()
{
	return _tredFile;
}

/*
 * From field_geometry.cc
 */
#define CHNG_TRED 10

void FgTeData::preUpdate()
{
	_its_me = 1;

	_fg->notifyDataWillChange(CHNG_TRED);
	_fg->getFgInformer()->preTredValuesChanged();
}

void FgTeData::postUpdate()
{
	checkStatus();

	_fg->getFgInformer()->postTredValuesChanged();
	_fg->notifyDataHasChanged(CHNG_TRED);

	_its_me = 0;
}

int FgTeData::getDeadTraceCode(long trace)
{
   int retval;

   if (_tredFile->_nrecs)
   {
      float head[NUM_HEAD];
      long num_head = NUM_HEAD;
      long del, kill, rev, flag;

      _fg->calculateHeaderWords(trace, (_speed == Dreadful));

      for (int i = 0; i < NUM_HEAD; i++)
         head[i] = (float) _fg->getHeaderWordValue(i + 1);

      asgn_code_to_hdr_spws(
         head, &num_head, &_tredFile->_nrecs, _doptr, _codeptr,
         _tredFile->_hdr_wrd_1, _tredFile->_strt_vlu_1, _tredFile->_end_vlu_1, 
         _tredFile->_hdr_wrd_2, _tredFile->_strt_vlu_2, _tredFile->_end_vlu_2, 
         _tredFile->_hdr_wrd_3, _tredFile->_strt_vlu_3, _tredFile->_end_vlu_3, 
         &del, &kill, &rev, &flag);

      if      (kill)
         retval = ZT_CODE_ZERO;
      else if (rev )
         retval = ZT_CODE_REV ;
      else
         retval = ZT_CODE_LIVE;
   }
   else
   {
      retval = ZT_CODE_LIVE;
   }

   return retval;
}

int FgTeData::canDoFast()
{
	static int     ok_headers[] = { 9, 10, 26, 27, 28, 29 };
	static int num_ok_headers   = sizeof(ok_headers) / sizeof(int);

	return canDo(ok_headers, num_ok_headers, 1);
}

int FgTeData::canDoSlow()
{
	static int     bad_headers[] = { 19, 30, 37, 38 };
	static int num_bad_headers   = sizeof(bad_headers) / sizeof(int);

	return canDo(bad_headers, num_bad_headers, 0);
}

int FgTeData::canDo(int *headers, int num_headers, int select)
{
	/*
	 * If select == 1, can only have from headers.
	 * If select == 0, can not have any of headers.
	 */
	int retval = 1;

	for (int i = 0; i < _num_used_heads; i++)
		if (select != (getHeaderIndex
			(_used_heads[i], headers, num_headers) >= 0))
		{
			retval = 0;
			break;
		}

	return retval;
}

void FgTeData::getDeadTraceCodes(FgUserAbort *ua)
{
	if (!_tredFile->_nrecs)
		return;

	int i;

	if (_load < Headers)
	{
		_tred_traces = new FgTeTredStruct[_fg_traces->_ntraces];

		/*
		 * Allocate one big chunk, then divy it up
		 */
		_heads = new float[(int)_fg_traces->_ntraces * _num_used_heads];

		switch (_speed)
		{
			case Fast:
				fastSetHeaders(ua   );
				break;
			case Slow:
				slowSetHeaders(ua, 0);
				break;
			case Dreadful:
				slowSetHeaders(ua, 1);
				break;
			default:
				assert(0);
		}

		if (ua->aborted())
		{
			delete [] _tred_traces;
			delete [] _heads      ;
			return;
		}

		_load = Headers;
	}

	if (_load < Codes)
	{
		ua->startAbortOption();

		for (i = 0; i < _tredFile->_nrecs; i++)
		{
			if (!(i % 10))
			{
				if (ua->aborted())
				{
					ua->stopAbortOption();
					return;
				}

				char buff[80];
				sprintf(buff, "Applying TRED card %d of %d",
					i+1, _tredFile->_nrecs);
				_fg->getFgInformer()->showMessage(buff);
			}

			compare(i);
		}

		ua->stopAbortOption();

		_load = Codes;
	}

	for (i = 0; i < (int) _fg_traces->_ntraces; i++)
		if      (_tred_traces[i]._code[0])
		{
			_fg_traces->_dead[i] = (char)
				_zt_cards->combineDeadTraceCodes( ZT_CODE_ZERO,
					(int) _fg_traces->_dead[i]);
		}
		else if (_tred_traces[i]._code[1])
		{
			_fg_traces->_dead[i] = (char)
				_zt_cards->combineDeadTraceCodes( ZT_CODE_REV ,
					(int) _fg_traces->_dead[i]);
		}
}

void FgTeData::fastSetHeaders(FgUserAbort *ua)
{
	static char *format =
		"TRED:  Quickly calculating headers for trace %d of %d";
	float *head_ptr;
	int i, j;

	ua->startAbortOption();

	for (head_ptr = _heads, i = 0; i < (int) _fg_traces->_ntraces; i++)
	{
		if (!(i % 100000))
		{
			if (ua->aborted())
			{
				ua->stopAbortOption();
				return;
			}

			char buff[80];
			sprintf(buff, format, i+1, _fg_traces->_ntraces);
			_fg->getFgInformer()->showMessage(buff);
		}

		_tred_traces[i]._heads = head_ptr;
		head_ptr += _num_used_heads;

		for (j = 0; j < _num_used_heads; j++)
			_tred_traces[i]._heads[j] =
				(this->*_headerFunc[_used_heads[j] - 1])(i);

		_tred_traces[i]._code[0] = _tred_traces[i]._code[1] = 0;
	}

	ua->stopAbortOption();
}

void FgTeData::slowSetHeaders(FgUserAbort *ua, int more)
{
	static char *format[2] =
	{
	  "TRED:  Slowly calculating headers for trace %d of %d",
	  "TRED:  Dreadfully slowly calculating headers for trace %d of %d"
	};
	float *head_ptr;
	int  i, j;

	ua->startAbortOption();

	for (head_ptr = _heads, i = 0; i < _fg_traces->_ntraces; i++)
	{
		if (!(i % (10000 - 9000 * more)))
		{
			if (ua->aborted())
			{
				ua->stopAbortOption();
				return;
			}

			char buff[80];
			sprintf(buff, format[more], i+1, _fg_traces->_ntraces);
			_fg->getFgInformer()->showMessage(buff);
		}

		_tred_traces[i]._heads = head_ptr;
		head_ptr += _num_used_heads;

		_fg->calculateHeaderWords((long) (i + 1), more);

		for (j = 0; j < _num_used_heads; j++)
			_tred_traces[i]._heads[j] = (float)
				_fg->getHeaderWordValue(_used_heads[j]);

		_tred_traces[i]._code[0] = _tred_traces[i]._code[1] = 0;
	}

	ua->stopAbortOption();
}

float FgTeData::headerxx(int /*trace_index*/)
{
	assert(0);
	return 0.0F;
}

/*
 * header09 assumes that it is called in order from trace_index 0
 */
float FgTeData::header09(int trace_index)
{
	static int retval     = 0;	/* init only to avoid compile warning */
	static int last_index = 0;	/* init only to avoid compile warning */
	static FgGroup *old_grp = (FgGroup *) NULL;
	       FgGroup *new_grp = _fg_traces->_grps[trace_index];

	assert((trace_index == 0) || (trace_index == last_index+1));
	last_index = trace_index;

	if (new_grp != old_grp)
	{
		old_grp = new_grp;

		if (trace_index == 0)
			retval = 1;
		else
			retval++;
	}

	return (float) retval;
}

/*
 * header10 assumes that it is called in order from trace_index 0
 */
float FgTeData::header10(int trace_index)
{
	static int retval     = 0;	/* init only to avoid compile warning */
	static int last_index = 0;	/* init only to avoid compile warning */
	static FgGroup *old_grp = (FgGroup *) NULL;
	       FgGroup *new_grp = _fg_traces->_grps[trace_index];

	assert((trace_index == 0) || (trace_index == last_index+1));
	last_index = trace_index;

	if ((trace_index == 0) || (new_grp != old_grp))
	{
		old_grp = new_grp;
		retval = 1;
	}
	else
	{
		retval++;
	}

	return (float) retval;
}

float FgTeData::header26(int trace_index)
{
	return (float) _fg->getLineNumber(
		_fg_traces->_grps[trace_index]->getIxlSource());
}

float FgTeData::header27(int trace_index)
{
	return (float) _fg_traces->_rflags[trace_index]
		->getLinePointer()->getLineNumber();
}

float FgTeData::header28(int trace_index)
{
	return _fg_traces->_rflags[trace_index]->getShotpoint();
}

float FgTeData::header29(int trace_index)
{
   return _fg->getShotpoint(_fg_traces->_grps[trace_index]->getIxlSource(),
                            _fg_traces->_grps[trace_index]->getIxfSource());
}

void FgTeData::compare(int itred, int *effected, int *num_effected)
{
	if (effected)	/* both or none */
		assert( num_effected);
	else
		assert(!num_effected);

	int indx = (_tredFile->_codes[itred][0] == 'K') ? 0
	         : (_tredFile->_codes[itred][0] == 'R') ? 1 : 2;
	int hw1 = (int) _tredFile->_hdr_wrd_1[itred];
	int hw2 = (int) _tredFile->_hdr_wrd_2[itred];
	int hw3 = (int) _tredFile->_hdr_wrd_3[itred];

	if (indx < 2 && (hw1 || hw2 || hw3))
	{
		char  mark = (_tredFile->_dos[itred][0] == 'D') ? 1 : 0;
		int   ihw1 = getHeaderIndex(hw1, _used_heads, _num_used_heads);
		int   ihw2 = getHeaderIndex(hw2, _used_heads, _num_used_heads);
		int   ihw3 = getHeaderIndex(hw3, _used_heads, _num_used_heads);
		float min1 = _tredFile->_strt_vlu_1[itred];
		float max1 = _tredFile-> _end_vlu_1[itred];
		float min2 = _tredFile->_strt_vlu_2[itred];
		float max2 = _tredFile-> _end_vlu_2[itred];
		float min3 = _tredFile->_strt_vlu_3[itred];
		float max3 = _tredFile-> _end_vlu_3[itred];

		if (effected)
		{
			long i;
			for (*num_effected = 0, i = 0;
				i < _fg_traces->_ntraces;
				i++)
			{
				if ((!hw1 ||
				      ((_tred_traces[i]._heads[ihw1] >= min1)
				    && (_tred_traces[i]._heads[ihw1] <= max1)))
				 && (!hw2 ||
				      ((_tred_traces[i]._heads[ihw2] >= min2)
				    && (_tred_traces[i]._heads[ihw2] <= max2)))
				 && (!hw3 ||
				      ((_tred_traces[i]._heads[ihw3] >= min3)
				    && (_tred_traces[i]._heads[ihw3] <= max3))))
				{
					effected[(*num_effected)++] = (int) i;
				}
			}
		}
		else
		{
			long i;
			for (i = 0; i < _fg_traces->_ntraces; i++)
			{
				if ((!hw1 ||
				      ((_tred_traces[i]._heads[ihw1] >= min1)
				    && (_tred_traces[i]._heads[ihw1] <= max1)))
				 && (!hw2 ||
				      ((_tred_traces[i]._heads[ihw2] >= min2)
				    && (_tred_traces[i]._heads[ihw2] <= max2)))
				 && (!hw3 ||
				      ((_tred_traces[i]._heads[ihw3] >= min3)
				    && (_tred_traces[i]._heads[ihw3] <= max3))))
				{
					_tred_traces[i]._code[indx] = mark;
				}
			}
		}
	}
	else
	{
		if (effected)
			*num_effected = 0;
	}
}

int FgTeData::getHeaderIndex(int header, int *headers, int num_headers)
{
	int l, m, u;

	for (l = 0, u = num_headers - 1, m = (l + u) / 2;
		u >= l;
		m = (l + u) / 2)
	{
		if      (headers[m] == header)
			return m;
		else if (headers[m] <  header)
			l = m + 1;
		else
			u = m - 1;
	}

	return -1;
}

void FgTeData::selectSrcFlags(int itred)
{
	selectFlags(&FgTeData::srcFunc, itred);
}

void FgTeData::selectRcvFlags(int itred)
{
	selectFlags(&FgTeData::rcvFunc, itred);
}

void FgTeData::selectFlags(void (FgTeData::*func)(int, FgTeFlagStruct *),
	int itred)
{
	assert(_load >= Headers);
	assert(_fg_traces->_grps && _fg_traces->_rflags);

	int num_effected, i;
	FgTeFlagStruct *flags;

	int *effected = new int[_fg_traces->_ntraces];

	compare(itred, effected, &num_effected);

	if (num_effected)
	{
		flags = new FgTeFlagStruct[num_effected];

		for (i = 0; i < num_effected; i++)
			(this->*func)(effected[i], &flags[i]);

		qsort((void *) flags, (size_t) num_effected,
			sizeof(FgTeFlagStruct), compar);

		int unique_i;
		for (unique_i = 0, i = 1; i < num_effected; i++)
			if (compar((void *) &flags[unique_i],
				   (void *) &flags[       i]))
			{
				unique_i++;

				if (unique_i != i)
					memcpy((void*) &flags[unique_i],
					       (void*) &flags[       i],
						sizeof(FgTeFlagStruct));
			}

		num_effected = unique_i + 1;
	}

	delete [] effected;

	_its_me = 1;

	_fg->freezeDependentUpdates();

	for (long line = 0; line < _fg->numLines(); line++)
		_fg->clearFlagSelections(line);

	if (num_effected)
	{
		for (i = 0; i < num_effected; i++)
			_fg->setFlagSelectValue(
				flags[i]._iline, flags[i]._iflag, SELECT_YES);

		delete [] flags;
	}

	_fg->resumeDependentUpdates();

	_selects_valid = 1;

	_its_me = 0;
}

void FgTeData::srcFunc(int itrace, FgTeFlagStruct *flag)
{
	flag->_iline = _fg_traces->_grps[itrace]->getIxlSource();
	flag->_iflag = _fg_traces->_grps[itrace]->getIxfSource();
}

void FgTeData::rcvFunc(int itrace, FgTeFlagStruct *flag)
{
	flag->_iline = _fg_traces->_rflags[itrace]->getLinePointer()
		->getLineIndex();
	flag->_iflag = _fg_traces->_rflags[itrace]->getFlagIndex  ();
}

int FgTeData::compar(const void *element1, const void *element2)
{
	int retval;

	FgTeFlagStruct *ptr1 = (FgTeFlagStruct *) element1;
	FgTeFlagStruct *ptr2 = (FgTeFlagStruct *) element2;

	if      (ptr1->_iline < ptr2->_iline)
		retval = -1;
	else if (ptr1->_iline > ptr2->_iline)
		retval =  1;
	else if (ptr1->_iflag < ptr2->_iflag)
		retval = -1;
	else if (ptr1->_iflag > ptr2->_iflag)
		retval =  1;
	else
		retval =  0;

	return retval;
}

char *FgTeData::getStatusMessage()
{
	static char *message[] =
	{
		"TRED data not loaded\nTRED data not loaded",
		"Can do TRED calculations fast because only headers 9, 10, 26, 27, 28, and 29 are used\nMust make CMP gathers to enable TRED card selection",
		"Must do TRED calculations slowly because headers other than 9, 10, 26, 27, 28, and 29 are used\nMust make CMP gathers to enable TRED card selection",
		"Must do TRED calculations dreadfully slowly because header 19, 30, 37, or 38 is used\nMust make CMP gathers to enable TRED card selection", 
		"Have done TRED headers fast because only headers 9, 10, 26, 27, 28, and 29 are used\nTRED card selections enabled",
		"Have done TRED headers slowly because headers other than 9, 10, 26, 27, 28, and 29 are used\nTRED card selections enabled",
		"Have done TRED headers dreadfully slowly because header 19, 30, 37, or 38 is used\nTRED card selections enabled", 
		"Have done TRED headers and codes fast because only headers 9, 10, 26, 27, 28, and 29 are used\nTRED card selections enabled",
		"Have done TRED headers and codes slowly because headers other than 9, 10, 26, 27, 28, and 29 are used\nTRED card selections enabled",
		"Have done TRED headers and codes dreadfully slowly because header 19, 30, 37, or 38 is used\nTRED card selections enabled", 
	};

	switch (3 * _load + _speed)
	{
		case 0:
			return message[0];
		case 3:
		case 4:
		case 5:
		case 6:
		case 7:
		case 8:
		case 9:
		case 10:
		case 11:
			return message[3 * _load + _speed - 2];
		case 1:
		case 2:
		default:
			assert(0);
			return (char *) NULL;	/* silence compiler warning */
	}
}

void FgTeData::setDataLoaded(int value)
{
	/*
	 * Note _load is an enum and value is an int.
	 * If you just read a file (value = 1), _load must be 0 (Nothing)
	 * and will be set to 1 (Data).
	 * If you just closed a file (value = 0), _load must be > 0
	 * (Data, Headers, or Codes) and will be set to 0 (Nothing).
	 */
	assert((int) _load * value == 0);
	_load = (LoadState) value;

	if (_load)	/* opened */
	{
		assert(_speed == Fast);

		/*
		 * No HW 25 allowed
		 */
		for (long i = 0; i < _tredFile->_nrecs; i++)
		{
			if (_tredFile->_hdr_wrd_1[i] == 25)
				_tredFile->_hdr_wrd_1[i] = 0;

			if (_tredFile->_hdr_wrd_2[i] == 25)
				_tredFile->_hdr_wrd_2[i] = 0;

			if (_tredFile->_hdr_wrd_3[i] == 25)
				_tredFile->_hdr_wrd_3[i] = 0;
		}
	}
	else		/* closed */
	{
		/*
		 * You can always do nothing fast.
		 */
		_speed = Fast;
	}
}

void FgTeData::checkStatus()
{
	if (_load == Nothing)
		return;

	int all_heads[NUM_HEAD+1], used_heads[NUM_HEAD];
	int num_used_heads, i, j;

	/*
	 * Mark the used headers
	 */
	for (i = 1; i <= NUM_HEAD; i++)
		all_heads[i] = 0;

	for (i = 0; i < _tredFile->_nrecs; i++)
	{
		all_heads[_tredFile->_hdr_wrd_1[i]] = 1;
		all_heads[_tredFile->_hdr_wrd_2[i]] = 1;
		all_heads[_tredFile->_hdr_wrd_3[i]] = 1;
	}

	/*
	 * We put the marks into all_heads[0], but only
	 * look from 1.
	 */
	for (num_used_heads = 0, i = 1; i <= NUM_HEAD; i++)
		if (all_heads[i])
			used_heads[num_used_heads++] = i;

	if (num_used_heads == _num_used_heads)
	{
		for (i = 0; i < _num_used_heads; i++)
			if ( used_heads[i] != _used_heads[i])
			{
				for (j = i; j < _num_used_heads; j++)
					_used_heads[j] = used_heads[j];

				headersChanged();

				break;
			}

		if (_load > Headers)
			_load = Headers;
	}
	else
	{
		if (num_used_heads > _num_used_heads)
		{
			if (_num_used_heads)
				delete [] _used_heads;

			_used_heads = new int[num_used_heads];
		}
		else if (num_used_heads == 0)
		{
			delete [] _used_heads;
		}

		_num_used_heads = num_used_heads;

		for (i = 0; i < _num_used_heads; i++)
			_used_heads[i] = used_heads[i];

		headersChanged();
	}

	_selects_valid = 0;
}

void FgTeData::headersChanged()
{
	if      (canDoFast())
		_speed = Fast;
	else if (canDoSlow())
		_speed = Slow;
	else
		_speed = Dreadful;

	if (_load > Data)
	{
		delete [] _tred_traces;
		delete [] _heads      ;
		_load = Data;
	}
}

int FgTeData::selectsActive()
{
	return (_load > Data);
}

int FgTeData::selectsValid()
{
	return _selects_valid;
}

void FgTeData::startingChanges(FieldGeometry * /*fg*/)
{
	if (_its_me)
		return;

	_inform_may_have_changed_hdrs = _inform_changed_selects = 0;
}

void FgTeData::finishedChanges(FieldGeometry * /*fg*/)
{
	if (_its_me)
		return;

	if (_inform_may_have_changed_hdrs && (_load > Data))
	{
		delete [] _tred_traces;
		delete [] _heads      ;
		_load = Data;

		_selects_valid = 0;
	}
	else if (_inform_changed_selects)
	{
		_selects_valid = 0;
	}
}

void FgTeData::freezingDependentUpdates(FieldGeometry * /*fg*/)
{
	if (_its_me)
		return;

	_inform_out_of_date = 0;
}

void FgTeData::dependentValuesOutOfDate(FieldGeometry * /*fg*/)
{
	if (_its_me)
		return;

	_inform_out_of_date = 1;
}

void FgTeData::preResumeDependentUpdates(FieldGeometry * /*fg*/)
{
	if (_its_me)
		return;

	/* do nothing */
}

void FgTeData::postResumeDependentUpdates(FieldGeometry * /*fg*/)
{
	if (_its_me)
		return;

	if (_inform_out_of_date)
		_inform_may_have_changed_hdrs = 1;

	/*
	 * Do not know about selects, so assume the worst.
	 */
	_inform_changed_selects = 1;
}

void FgTeData::preFlagValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
	int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
	if (_its_me)
		return;

	/* do nothing */
}

void FgTeData::postFlagValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
	int ident, long /*index*/, long /*nrem*/, long /*nins*/)
{
	if (_its_me)
		return;

	if (ident == FG_SEL)
		_inform_changed_selects = 1;
}

void FgTeData::receiverGathersOutOfDate(FieldGeometry * /*fg*/)
{
	if (_its_me)
		return;

	_inform_may_have_changed_hdrs = 1;
}

void FgTeData::midpointGathersOutOfDate(FieldGeometry * /*fg*/)
{
	if (_its_me)
		return;

	if (_speed != Fast)
		_inform_may_have_changed_hdrs = 1;
}

int FgTeData::canEdit()
{
	return !_fg->isLocked(LOCK_S_R_CMP);
}

int FgTeData::canDelete()
{
	return !_fg->isLocked(LOCK_DEL);
}
