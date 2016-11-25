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
#ifndef _FGTE_PAIR_HH
#define _FGTE_PAIR_HH

#include "sl/sl_file_pair_plus.hh"
#include "cprim.h"

class FgTePair : public SLFilePairPlus
{
	public:

		FgTePair(SLDelay *parent, char *name, HelpCtx hctx,
			class FgTeData *data, class FgTePop *pop);
		virtual ~FgTePair();

	protected:

		virtual void doValidate(const char *filename1,
			const char *filename2,
			long *valid1, long *valid2,
			char *info1, char *info2,
			long *same_datasets);

		virtual int doOpen(long status,
			const char *filename1,
			const char *filename2,
			Boolean required1, Boolean required2,
			FppWorkingMessage *working_message_trap,
			void              *working_message_data,
			char *msg);

		virtual void doClose();

	private:

		class FgTeData *_data;
		class FgTePop  *_pop ;

		static int got25(TredFile *tf);

		FgTePair()
			: SLFilePairPlus((SLDelay *) NULL, (char *) NULL,
			  (HelpCtx) NULL, False, (const char *) NULL,
			  (const char *) NULL, False, False)
			{ /* private, no access to default constructor */ }
		FgTePair(FgTePair &)
			: SLFilePairPlus((SLDelay *) NULL, (char *) NULL,
			  (HelpCtx) NULL, False, (const char *) NULL,
			  (const char *) NULL, False, False)
			{ /* private, no access to copy constructor */ }
		FgTePair& operator=(FgTePair &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FGTE_PAIR_HH */
