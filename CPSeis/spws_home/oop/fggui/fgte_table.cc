#include "fggui/fgte_table.hh"
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
#include "geom/fgte_data.hh"
#include "fggui/fgte_pop.hh"

#include <string.h>

enum {
	SEL_SRC = 1,
	SEL_RCV ,
	NOTHING ,
	DO      ,
	CODE    ,
	HDRWRD1 ,
	STRTVLU1,
	ENDVLU1 ,
	HDRWRD2 ,
	STRTVLU2,
	ENDVLU2 ,
	HDRWRD3 ,
	STRTVLU3,
	ENDVLU3
};

FgTeTable::FgTeTable(SLDelay *slparent, char *name, FgTeData *data,
	FgTePop *pop)
	: SLDatabox(slparent, name, (void *) NULL, 4),
	  _data(data), _pop(pop), _one(-1), _two(-2),
	  _editable(0), _selected_col( -1), _selected_row(-1)
{
	assert(slparent == pop);
}

FgTeTable::~FgTeTable()
{
	/* do nothing */
}

void FgTeTable::makeHelper()
{
	TredFile *tredFile = _data->getTredFile();

	regArrays(staticNupdate, staticMaxupdate, 0, 0, 3, 30);

	static long zero    =   0;
	static long nothing = -99;

	regCarray   (SEL_SRC, "S" , &zero, &_two, 0, 1);
	updateCvar  (SEL_SRC, staticSupdate);
	trapCvar    (SEL_SRC, staticStrap  );
	updateSwitch(SEL_SRC, staticSswitch);

	regCarray   (SEL_RCV, "R" , &zero, &_two, 0, 1);
	updateCvar  (SEL_RCV, staticSupdate);
	trapCvar    (SEL_RCV, staticStrap  );
	updateSwitch(SEL_RCV, staticSswitch);

	regCarray(NOTHING, " " , &zero, &nothing, 0, 3);

	regCarray (DO, " Do" , &zero, &_two, 0, TRED_CODES_SIZE);
	updateCvar(DO, staticCupdate);
	trapCvar  (DO, staticCtrap  );

	regCarray (CODE, "Code", &zero, &_two, 0, TRED_CODES_SIZE);
	updateCvar(CODE, staticCupdate);
	trapCvar  (CODE, staticCtrap  );

	regIarray (HDRWRD1, "HW 1"  , &zero, &_one, 0, 4   );
	updateIvar(HDRWRD1, staticIupdate);
	trapIvar  (HDRWRD1, staticItrap  );

	regFarray (STRTVLU1, " Start", &zero, &_one, 0, 7, 2);
	updateFvar(STRTVLU1, staticFupdate);
	trapFvar  (STRTVLU1, staticFtrap  );

	regFarray (ENDVLU1, "  End" , &zero, &_one, 0, 7, 2);
	updateFvar(ENDVLU1, staticFupdate);
	trapFvar  (ENDVLU1, staticFtrap  );

	regIarray (HDRWRD2, "HW 2"  , &zero, &_one, 0, 4   );
	updateIvar(HDRWRD2, staticIupdate);
	trapIvar  (HDRWRD2, staticItrap  );

	regFarray (STRTVLU2, " Start", &zero, &_one, 0, 7, 2);
	updateFvar(STRTVLU2, staticFupdate);
	trapFvar  (STRTVLU2, staticFtrap  );

	regFarray (ENDVLU2, "  End" , &zero, &_one, 0, 7, 2);
	updateFvar(ENDVLU2, staticFupdate);
	trapFvar  (ENDVLU2, staticFtrap  );

	regIarray (HDRWRD3, "HW 3"  , &zero, &_one, 0, 4   );
	updateIvar(HDRWRD3, staticIupdate);
	trapIvar  (HDRWRD3, staticItrap  );

	regFarray (STRTVLU3, " Start", &zero, &_one, 0, 7, 2);
	updateFvar(STRTVLU3, staticFupdate);
	trapFvar  (STRTVLU3, staticFtrap  );

	regFarray (ENDVLU3, "  End" , &zero, &_one, 0, 7, 2);
	updateFvar(ENDVLU3, staticFupdate);
	trapFvar  (ENDVLU3, staticFtrap  );
}

long FgTeTable::staticNupdate(void *data)
{
	return ((FgTeTable *) data)->_data->getTredFile()->_nrecs;
}

long FgTeTable::staticMaxupdate(void * /*data*/)
{
	return (long) TRED_TABLE_NMAX;
}

long FgTeTable::staticIupdate(void *data, long ident, long index)
{
	long retval;
	TredFile *tredFile = ((FgTeTable *) data)->_data->getTredFile();

	switch (ident)
	{
		case HDRWRD1:
			retval = tredFile->_hdr_wrd_1[index];
			break;
		case HDRWRD2:
			retval = tredFile->_hdr_wrd_2[index];
			break;
		case HDRWRD3:
			retval = tredFile->_hdr_wrd_3[index];
			break;
		default:
			assert(0);
	}

	return retval;
}

float FgTeTable::staticFupdate(void *data, long ident, long index)
{
	float retval;
	TredFile *tredFile = ((FgTeTable *) data)->_data->getTredFile();

	switch (ident)
	{
		case STRTVLU1:
			retval = tredFile->_strt_vlu_1[index];
			break;
		case ENDVLU1 :
			retval = tredFile-> _end_vlu_1[index];
			break;
		case STRTVLU2:
			retval = tredFile->_strt_vlu_2[index];
			break;
		case ENDVLU2 :
			retval = tredFile-> _end_vlu_2[index];
			break;
		case STRTVLU3:
			retval = tredFile->_strt_vlu_3[index];
			break;
		case ENDVLU3 :
			retval = tredFile-> _end_vlu_3[index];
			break;
		default:
			assert(0);
	}

	return retval;
}

char *FgTeTable::staticCupdate(void *data, long ident, long index)
{
	char *retval;
	TredFile *tredFile = ((FgTeTable *) data)->_data->getTredFile();

	switch (ident)
	{
		case DO     :
			retval = tredFile->_dos  [index];
			break;
		case CODE   :
			retval = tredFile->_codes[index];
			break;
		default:
			assert(0);
	}

	return retval;
}

char *FgTeTable::staticSupdate(void* /*data*/, long  ident, long  /*index*/)
{
	char *retval;
	static char blank[2] = " ";

	switch (ident)
	{
		case SEL_SRC:
		case SEL_RCV:
			retval = blank;
			break;
		default:
			assert(0);
	}

	return retval;
}

void FgTeTable::staticItrap(void *data, long ident, long index,
	long  ivar, long nread, char *endkey)
{
	if (nread)
	{
		((FgTeTable *) data)->objItrap(ident, index, ivar);
	}
	else
	{
		KeyType key = getKeyType(endkey);

		switch (key)
		{
			case INSERT:
				((FgTeTable *) data)->insertRow(index);
				break;
			case REMOVE:
				((FgTeTable *) data)->removeRow(index);
				break;
			case RETURN:
			case WHO_CARES:
				/* do nothing */
				break;
			default:
				assert(0);
		}
	}
}

void FgTeTable::staticFtrap(void *data, long ident, long index,
	float fvar, long nread, char *endkey)
{
	if (nread)
	{
		((FgTeTable *) data)->objFtrap(ident, index, fvar);
	}
	else
	{
		KeyType key = getKeyType(endkey);

		switch (key)
		{
			case INSERT:
				((FgTeTable *) data)->insertRow(index);
				break;
			case REMOVE:
				((FgTeTable *) data)->removeRow(index);
				break;
			case RETURN:
			case WHO_CARES:
				/* do nothing */
				break;
			default:
				assert(0);
		}
	}
}

void FgTeTable::staticCtrap(void *data, long ident, long index,
	char *cvar, long /*nread*/, char *endkey)
{
	KeyType key = getKeyType(endkey);

	switch (key)
	{
		case RETURN:
			((FgTeTable *) data)->objCtrap(ident, index, cvar);
			break;
		case INSERT:
			((FgTeTable *) data)->insertRow(index);
			break;
		case REMOVE:
			((FgTeTable *) data)->removeRow(index);
			break;
		case WHO_CARES:
			/* do nothing */
			break;
		default:
			assert(0);
	}
}

void FgTeTable::staticStrap(void *data, long ident, long index,
	char *cvar, long /*nread*/, char *endkey)
{
	KeyType key = getKeyType(endkey);

	switch (key)
	{
		case RETURN:
			((FgTeTable *) data)->objStrap(ident, index, cvar);
			break;
		case INSERT:
		case REMOVE:
		case WHO_CARES:
			/* do nothing */
			break;
		default:
			assert(0);
	}
}

void FgTeTable::objItrap(long ident, long index, long ivar)
{
	if (!_data->canEdit())
		return;

	_data->preUpdate();

	TredFile *tredFile = _data->getTredFile();

	if (index == tredFile->_nrecs)
	{
		tredFileInitRow(tredFile, index);
		tredFile->_nrecs++;
	}

	if      (ivar  <  0)
		ivar =  0;
	else if (ivar ==  1)
		ivar =  2;
	else if (ivar == 25)
		ivar =  0;
	//else if (ivar  > 64)
	//	ivar = 64;

	switch (ident)
	{
		case HDRWRD1:
			tredFile->_hdr_wrd_1[index] = ivar;
			break;
		case HDRWRD2:
			tredFile->_hdr_wrd_2[index] = ivar;
			break;
		case HDRWRD3:
			tredFile->_hdr_wrd_3[index] = ivar;
			break;
		default:
			assert(0);
	}

	_pop ->updateFile();
	_data->postUpdate();
}

void FgTeTable::objFtrap(long ident, long index, float fvar)
{
	if (!_data->canEdit())
		return;

	_data->preUpdate();

	TredFile *tredFile = _data->getTredFile();

	if (index == tredFile->_nrecs)
	{
		tredFileInitRow(tredFile, index);
		tredFile->_nrecs++;
	}

	switch (ident)
	{
		case STRTVLU1:
			tredFile->_strt_vlu_1[index] = fvar;
			break;
		case ENDVLU1:
			tredFile-> _end_vlu_1[index] = fvar;
			break;
		case STRTVLU2:
			tredFile->_strt_vlu_2[index] = fvar;
			break;
		case ENDVLU2:
			tredFile-> _end_vlu_2[index] = fvar;
			break;
		case STRTVLU3:
			tredFile->_strt_vlu_3[index] = fvar;
			break;
		case ENDVLU3:
			tredFile-> _end_vlu_3[index] = fvar;
			break;
		default:
			assert(0);
	}

	_pop ->updateFile();
	_data->postUpdate();
}

void FgTeTable::objCtrap(long ident, long index, char * /*cvar*/)
{
	if (!_data->canEdit())
		return;

	_data->preUpdate();

	TredFile *tf = _data->getTredFile();

	if (index == tf->_nrecs)
	{
		switch (ident)
		{
			case DO:
			case CODE:
				tredFileInitRow(tf, index);
				tf->_nrecs++;
				break;
			default:
				assert(0);
		}
	}
	else
	{
		switch (ident)
		{
			case DO:
				switch (tf->_dos[index][0])
				{
					case 'D':
						memcpy((void *) tf->_dos[index],
						  (void *) "UNDO", 
						  (size_t) TRED_CODES_SIZE);
						break;
					case 'U':
						memcpy((void *) tf->_dos[index],
						  (void *) "DO  ", 
						  (size_t) TRED_CODES_SIZE);
						break;
					default:
						assert(0);
				}
				break;
			case CODE:
				switch (tf->_codes[index][0])
				{
					case 'K':
						memcpy((void*)tf->_codes[index],
						  (void *) "REV ", 
						  (size_t) TRED_CODES_SIZE);
						break;
					case 'R':
						memcpy((void*)tf->_codes[index],
						  (void *) "FLAG", 
						  (size_t) TRED_CODES_SIZE);
						break;
					case 'F':
						memcpy((void*)tf->_codes[index],
						  (void *) "DEL ", 
						  (size_t) TRED_CODES_SIZE);
						break;
					case 'D':
						memcpy((void*)tf->_codes[index],
						  (void *) "KILL", 
						  (size_t) TRED_CODES_SIZE);
						break;
					default:
						assert(0);
				}
				break;
			default:
				assert(0);
		}
	}

	_pop ->updateFile();
	_data->postUpdate();
}

void FgTeTable::objStrap(long ident, long index, char * /*cvar*/)
{
	TredFile *tf = _data->getTredFile();

	if (index < tf->_nrecs)
	{
		switch (ident)
		{
			case SEL_SRC:
				_data->selectSrcFlags((int) index);
				break;
			case SEL_RCV:
				_data->selectRcvFlags((int) index);
				break;
			default:
				assert(0);
		}

		doSelect((int) ident, (int) index);
	}
}

long FgTeTable::staticSswitch(void *data, long ident, long index)
{
	return ((FgTeTable *) data)->objSswitch(ident, index);
}

long FgTeTable::objSswitch(long ident, long index)
{
	long retval;

	if ( _editable
	 && (index < _data->getTredFile()->_nrecs)
	 && (_data->selectsActive()))
	{
		retval = isSelected((int) ident, (int) index) ? 6 : 2;
	}
	else
	{
		retval = -2;
	}

	return retval;
}

void FgTeTable::setEditable(int flag)
{
	if (flag)
	{
		_one =  1;
		_two =  2;
	}
	else
	{
		_one = -1;
		_two = -2;
		_selected_col = _selected_row = -1;
	}

	_editable = flag;
}

void FgTeTable::insertRow(long index)
{
	if (!_data->canEdit())
		return;

	TredFile *tredFile = _data->getTredFile();

	if (tredFile->_nrecs < TRED_TABLE_NMAX)
	{
		_data->preUpdate();

		for (long i = tredFile->_nrecs - 1; i >= index; i--)
			tredFileCopyRow(tredFile, i, i + 1);

		tredFileInitRow(tredFile, index);

		tredFile->_nrecs++;

		_pop ->updateFile();
		_data->postUpdate();
	}
	else
	{
		showMessage("Can not insert, TRED file is max. size.");
	}
}

void FgTeTable::removeRow(long index)
{
	if (!_data->canDelete())
		return;

	TredFile *tredFile = _data->getTredFile();

	if (tredFile->_nrecs)
	{
		_data->preUpdate();

		for (long i = index + 1; i < tredFile->_nrecs; i++)
			tredFileCopyRow(tredFile, i, i - 1);

		if (index < tredFile->_nrecs)
			tredFile->_nrecs--;

		_pop ->updateFile();
		_data->postUpdate();
	}
	else
	{
		showMessage("Can not remove, TRED file is empty.");
	}
}

FgTeTable::KeyType FgTeTable::getKeyType(char *endkey)
{
	KeyType retval;

	if      (!strcmp(endkey, "RETURN"))
		retval = RETURN;
	else if (!strcmp(endkey, "INSERT"))
		retval = INSERT;
	else if (!strcmp(endkey, "REMOVE"))
		retval = REMOVE;
	else
		retval = WHO_CARES;

	return retval;
}

void FgTeTable::tredFileCopyRow(TredFile *tf, long from, long to)
{
	memcpy((void *) tf->_dos  [to], (void *) tf->_dos  [from],
		(size_t) TRED_CODES_SIZE);
	memcpy((void *) tf->_codes[to], (void *) tf->_codes[from],
		(size_t) TRED_CODES_SIZE);

	tf-> _hdr_wrd_1[to] = tf-> _hdr_wrd_1[from];
	tf->_strt_vlu_1[to] = tf->_strt_vlu_1[from];
	tf-> _end_vlu_1[to] = tf-> _end_vlu_1[from];

	tf-> _hdr_wrd_2[to] = tf-> _hdr_wrd_2[from];
	tf->_strt_vlu_2[to] = tf->_strt_vlu_2[from];
	tf-> _end_vlu_2[to] = tf-> _end_vlu_2[from];

	tf-> _hdr_wrd_3[to] = tf-> _hdr_wrd_3[from];
	tf->_strt_vlu_3[to] = tf->_strt_vlu_3[from];
	tf-> _end_vlu_3[to] = tf-> _end_vlu_3[from];
}

void FgTeTable::tredFileInitRow(TredFile *tf, long row)
{
	memcpy((void *) tf->_dos  [row], (void *) "DO  ", 
		(size_t) TRED_CODES_SIZE);
	memcpy((void *) tf->_codes[row], (void *) "KILL",
		(size_t) TRED_CODES_SIZE);

	tf-> _hdr_wrd_1[row] = 0   ;
	tf->_strt_vlu_1[row] = 0.0F;
	tf-> _end_vlu_1[row] = 0.0F;

	tf-> _hdr_wrd_2[row] = 0   ;
	tf->_strt_vlu_2[row] = 0.0F;
	tf-> _end_vlu_2[row] = 0.0F;

	tf-> _hdr_wrd_3[row] = 0   ;
	tf->_strt_vlu_3[row] = 0.0F;
	tf-> _end_vlu_3[row] = 0.0F;
}

void FgTeTable::doSelect(int col, int row)
{
	_selected_col = col;
	_selected_row = row;
}

int FgTeTable::isSelected(int col, int row)
{
	return (_data->selectsValid()
		&& (col == _selected_col) && (row == _selected_row));
}
