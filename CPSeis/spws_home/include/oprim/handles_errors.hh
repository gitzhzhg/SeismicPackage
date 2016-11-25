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
#ifndef HANDLES_ERRORS_H
#define HANDLES_ERRORS_H

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

class HandlesErrors
{
	private:

		HandlesErrors *_whoHasMe;
		HandlesErrors *_whoToNotify;
		int _error;

		virtual void errorHandler(HandlesErrors *, int)
			{ /* do nothing */ }
		virtual void errorNotify(int) //No this ptr, might be deleted.
			{ /* do nothing */ }

	protected:

		HandlesErrors()
			{
				_whoHasMe    = (HandlesErrors *) 0;
				_whoToNotify = (HandlesErrors *) 0;
				_error       = _ERROR_NONE;
			}
		virtual ~HandlesErrors()	//Virtual to silence g++ warning
			{ /* do nothing */ }
		void handleError(int error)
			{ handleError(0, error); }
		void handleError(HandlesErrors *, int);
		void notifyError(int error)
			{
				_error = error;
				_whoToNotify ? _whoToNotify->notifyError(error)
					: errorNotify(error);
			}

	public:

		enum
		{
			_ERROR_NONE,
			_ERROR_PARTNER_DIED,
			_ERROR_OUT_OF_COLORS,
			_ERROR_OUT_OF_PIXMAP_MEMORY,
			_ERROR_BAD_NODE_NAME,
			_ERROR_SLAVE_DPY_NOT_FOUND,
			_ERROR_SLAVE_DPY_NOT_EXEC,
			_ERROR_VMS_SPWS_BIN_NOT_SET,
			_ERROR_COUNT
		};

		static char *errorString[_ERROR_COUNT];

		void setErrorHandler(HandlesErrors *whoHasMe)
			{
				assert(!_whoToNotify);	// One or the other.
				_whoHasMe = whoHasMe;
			}
		void setErrorNotify(HandlesErrors *whoToNotify)
			{
				assert(!_whoHasMe);	// One or the other.
				_whoToNotify = whoToNotify;
			}
		int checkError()
			{ return(_error); }
		void clearError()
			{ _error = _ERROR_NONE; }

};

#endif
