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
#ifndef _FGTE_TABLE_HH
#define _FGTE_TABLE_HH

#include "sl/sl_databox.hh"

#include <cprim.h>

class FgTeTable : public SLDatabox
{
	public:

		enum KeyType {
			RETURN   ,
			INSERT   ,
			REMOVE   ,
			WHO_CARES
		};
			
		FgTeTable(SLDelay *slparent, char *name, class FgTeData *data,
			class FgTePop *pop);
		virtual ~FgTeTable();
		void setEditable(int flag);

	protected:

		virtual void makeHelper();

	private:

		class FgTeData *_data;
		class FgTePop  *_pop ;
		long _one, _two;
		int _editable;
		int _selected_col, _selected_row;

		static long staticNupdate  (void *data);
		static long staticMaxupdate(void *data);

		static long  staticIupdate(void *data, long ident, long index);
		static float staticFupdate(void *data, long ident, long index);
		static char *staticCupdate(void *data, long ident, long index);
		static char *staticSupdate(void *data, long ident, long index);

		static void staticItrap(void *data, long ident,
			long index, long  ivar, long nread, char *endkey);
		static void staticFtrap(void *data, long ident,
			long index, float fvar, long nread, char *endkey);
		static void staticCtrap(void *data, long ident,
			long index, char *cvar, long nread, char *endkey);
		static void staticStrap(void *data, long ident,
			long index, char *cvar, long nread, char *endkey);

		void objItrap(long ident, long index, long  ivar);
		void objFtrap(long ident, long index, float fvar);
		void objCtrap(long ident, long index, char *cvar);
		void objStrap(long ident, long index, char *cvar);

		static long staticSswitch(void *data, long ident, long index);

		long objSswitch(long ident, long index);

		void insertRow(long index);
		void removeRow(long index);

		static FgTeTable::KeyType getKeyType(char *endkey);
		static void tredFileCopyRow(TredFile *tf, long from, long to);
		static void tredFileInitRow(TredFile *tf, long row);

		void doSelect  (int col, int row);
		int  isSelected(int col, int row);
};

#endif	/* _FGTE_TABLE_HH */
