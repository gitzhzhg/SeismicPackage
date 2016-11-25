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
#ifndef SEIS_SELECT_IPC_POP_HH
#define SEIS_SELECT_IPC_POP_HH

#include "sp/seis_select_pop.hh"
#include "ipc/ipc_inform.hh"


class SeisSelectIpcPop :  public SeisSelectPop, public IpcInform
{
public:
  SeisSelectIpcPop
    (Widget p,
     char *name,
     HelpCtx hctx,
     SeisPlot *sp,
     IpcIO *ipc_io,
     SeisPlot *info_sp = NULL,
     Boolean allow_selector = False);

  ~SeisSelectIpcPop ();

private:
  virtual void beforeIpc ();

  virtual void afterIpc ();

  void displayGather
    (int cmp,
     int line);

  void displayGathers
    (int line);

  virtual void resetDataSelectionParameters
    (long ntot,
     long iskp,
     long ndo,
     long nskp,
     long tdec,
     float tmin,
     float tmax);

  int learnFileOrganization
    (int hdr_cmp,
     int hdr_line);

  int iskpToGather
    (int cmp,
     int line);

  int iskpToBeginningOfLine
    (int line);

  void getLinePattern
    (long *ntot,
     long *ndo,
     long *nskp);

  char
    *_data,
    *_active_file;

  int
    _istat,
    _file_regularized,
    _gather_size,
    _num_gathers,
    _line_size,
    _num_lines,
    _first_gather,
    _last_gather,
    _first_line,
    _last_line;

};

#endif
