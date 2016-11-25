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
#include "pick/fk_pair.hh"
#include "pick/ll_fk_data.hh"
#include "cprim.h"
#include "inquire.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>

FkPair::FkPair(SLDelay *parent, char *name, HelpCtx hctx,
	FkDataLinkedList *data)
	: SLFilePairPlus(parent, name, hctx, True, "FK pick file", "fkp",
	  False, False),
	  _data(data)
{
	/* just initializers */
}

FkPair::~FkPair()
{
	/* do nothing */
}


void FkPair::doValidate(const char *filename1, const char *filename2,
	long *valid1, long *valid2, char *info1, char *info2,
	long * /*same_datasets*/)
{
	FkDataLinkedList *list = new FkDataLinkedList;

	*valid1 = (list->readFile(filename1, info1) == 0)
                                       ? INQUIRE_VALID_YES : INQUIRE_VALID_NO;

	list->clear();

	*valid2 = (list->readFile(filename2, info2) == 0)
                                       ? INQUIRE_VALID_YES : INQUIRE_VALID_NO;

	delete list;
}

int FkPair::doOpen(long status,
	const char *filename1, const char *filename2,
	Boolean /*required1*/, Boolean /*required2*/,
	FppWorkingMessage * /*working_message_trap*/,
	void * /*working_message_data*/,
	char *msg)
{
	int retval;

	switch (status)
	{
		case FILE_READ_ONLY:
		case FILE_UPDATE:
			retval = _data->readFile(filename1, msg);
			break;
		case FILE_COPY:
			retval = _data->readFile(filename1, msg);
			if (0 == retval)
				retval = _data->writeFile(filename2, msg);
			break;
		case FILE_CREATE:
			retval = 0;
			break;
		default:
			assert(0);
	}

	return retval;
}

void FkPair::doClose()
{
	/* do nothing */
}
