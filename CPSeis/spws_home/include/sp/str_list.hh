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
#ifndef STR_LIST_H
#define STR_LIST_H


#include "oprim/element.hh"
#include "oprim/ll_base.hh"



class StrElement : public Element
{
  friend class StrList;
  protected:
     char *_str;

     StrElement(char *str) : _str(str) {}
     ~StrElement() {}
     int operator ==(void * const str) const
                        { return((char*) str == _str); }
     virtual void print() const {}
};



class StrList : public BaseLinkedList
{
  public:
        void add(char *str);
        void remove(char *str);
        char *top(void **ptr = (void **) 0);
        char *find(char *str, void **ptr = (void **) 0);
        char *bottom(void **ptr = (void **) 0);
        char *next(void **ptr = (void **) 0);
        char *prev(void **ptr = (void **) 0);
        char *current(void **ptr = (void **) 0);


};


#endif
