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
#include "sp/seis_inform.hh"


SeisInform::SeisInform(SeisPlot *sp) 
{ 
   if (sp) addSeisPlot(sp);
} 

SeisInform::~SeisInform() 
{ 
   for(SeisPlot *sp= top(); (sp); sp= top()) 
          delSeisPlot(sp);
}

void SeisInform::addSeisPlot(SeisPlot *sp, void *user_data) 
{
   if (sp && !find(sp) ) {
       sp->addInformer(this);  // adds "this" SeisInform to list of informers
							   // for this SeisPlot
       add(sp,user_data);  // adds SeisPlot sp to linked list in class SPList.
						   // sp becomes a SeisPlot this SeisInform knows about.
						   // SeisInform::add() comes from parent class SPList.
   }
}

void SeisInform::delSeisPlot(SeisPlot *sp) 
{
   if (sp && find(sp)) {
      sp->delInformer(this); 
      remove(sp);
   }
}
