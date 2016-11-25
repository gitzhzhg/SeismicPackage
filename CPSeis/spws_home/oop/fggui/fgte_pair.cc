#include "fggui/fgte_pair.hh"
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
#include "inquire.h"

#include <string.h>
#include <assert.h>

FgTePair::FgTePair(SLDelay *parent, char *name, HelpCtx hctx,
	FgTeData *data, FgTePop *pop)
	: SLFilePairPlus(parent, name, hctx, True, "TRED file", "tred",
	  False, False),
	  _data(data), _pop(pop)
{
	assert(parent == pop);
}

FgTePair::~FgTePair()
{
	/* do nothing */
}


void FgTePair::doValidate(const char *filename1, const char *filename2,
	long *valid1, long *valid2, char *info1, char *info2,
	long * /*same_datasets*/)
{
	TredFile *temp = tredfile_create();

	*valid1 = (tredfile_get(filename1, temp, info1) == 0)
                                                         ? INQUIRE_VALID_YES 
                                                         : INQUIRE_VALID_NO;

	if ((*valid1 == INQUIRE_VALID_YES) && got25(temp))
		strcpy(info1, " contains HW 25 --- will be ignored");

	*valid2 = (tredfile_get(filename2, temp, info2) == 0)
                                                         ? INQUIRE_VALID_YES 
                                                         : INQUIRE_VALID_NO;

	tredfile_destroy(temp);

	_pop->setValidity((*valid1 == INQUIRE_VALID_YES)
		|| ((*valid2 == INQUIRE_VALID_NO) && strlen(filename2)));
}

int FgTePair::doOpen(long status,
	const char *filename1, const char *filename2,
	Boolean /*required1*/, Boolean /*required2*/,
	FppWorkingMessage * /*working_message_trap*/,
	void * /*working_message_data*/,
	char *msg)
{
	int retval;

	TredFile *tredFile = _data->getTredFile();
	assert(0 == tredFile->_nrecs);

	switch (status)
	{
		case FILE_READ_ONLY:
		case FILE_UPDATE:
			_data->preUpdate();
			retval = tredfile_get(filename1, tredFile, msg);
			if (retval)
				tredfile_clear(tredFile);
			else
				_data->setDataLoaded(1);
			_data->postUpdate();
			break;
		case FILE_COPY:
			_data->preUpdate();
			retval = tredfile_get(filename1, tredFile, msg);
			if (0 == retval)
			{
				retval = tredfile_put(filename2, tredFile, msg);
				if (retval)
					tredfile_clear(tredFile);
				else
					_data->setDataLoaded(1);
			}
			else
			{
				tredfile_clear(tredFile);
			}
			_data->postUpdate();
			break;
		case FILE_CREATE:
			_data->setDataLoaded(1);
			_data->checkStatus  ( );
			retval = 0;
			break;
		default:
			assert(0);
	}

	return retval;
}

void FgTePair::doClose()
{
	if (_data->getTredFile()->_nrecs)
	{
		_data-> preUpdate();
		tredfile_clear(_data->getTredFile());
		_data->postUpdate();
	}

	_data->setDataLoaded(0);
}

int FgTePair::got25(TredFile *tf)
{
	int retval = 0;

	for (long i = 0; i < tf->_nrecs; i++)
		if ((tf->_hdr_wrd_1[i] == 25)
		 || (tf->_hdr_wrd_2[i] == 25)
		 || (tf->_hdr_wrd_3[i] == 25))
		{
			retval = 1;
			break;
		}

	return retval;
}
