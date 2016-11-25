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
#ifndef _savecvm_
#define _savecvm_


typedef struct _savecvm
 { char file[96];
   int control;
   int style;
   int can_write;
   Widget shell,form,fcw;
   Widget cvm,gws,goc,sierra;
   Widget ok,cancel;
   void *cvmapp;
 } savecvm;

#define DO_WRITE 1
#define NO_WRITE  0

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
Widget SaveGUI(int , void *cvmapp ,HelpCtx );
int savecvm_out(void *cvmapp,char *fileo,long *otyp,char *msg);
#ifdef __cplusplus
}                   // for C++
#endif

#endif
