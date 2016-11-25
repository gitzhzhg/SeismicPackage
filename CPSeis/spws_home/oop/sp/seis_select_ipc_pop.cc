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
#include "sp/seis_select_ipc_pop.hh"
#include "ipc/ipc_constants.hh"
#include "ipc/ipc_io.hh"
#include "sl/sl_text_box.hh"

#include "trciof77.h"

SeisSelectIpcPop::SeisSelectIpcPop (Widget p, char *name, HelpCtx hctx,
  SeisPlot *sp, IpcIO *ipc_io, SeisPlot *info_sp, Boolean allow_selector) :
  SeisSelectPop (p, name, hctx, sp, info_sp, allow_selector),
  IpcInform (ipc_io),
  _data              (0),
  _file_regularized  (0),
  _gather_size       (0),
  _num_gathers       (0),
  _line_size         (0),
  _num_lines         (0),
  _first_gather      (0),
  _last_gather       (0),
  _first_line        (0),
  _last_line         (0),
  _active_file       (0)
{
  _data = (char *)malloc (IPC::MAX_SOCKET_BUF*sizeof(char));
}

SeisSelectIpcPop::~SeisSelectIpcPop ()
{
  free (_data);
}

void SeisSelectIpcPop::beforeIpc ()
{
  if (IO()->data()) {
    strcpy (_data, IO()->data());
  }
  else {
    strcpy (_data, " ");
  }
}

void SeisSelectIpcPop::afterIpc ()
{
  if (IO()->data() && strcmp(_data,IO()->data())) {
    // a change occured, read the data
    strcpy (_data, IO()->data());
    // this is just test code.  will need more flexibility handling data
    int count;
    float z, amp, cmp, line;
    count = sscanf (_data, "Z=%f,A=%f,H7=%f,H8=%f", &z, &amp, &cmp, &line);
    if (count == 4) {
      displayGather ((int)(cmp+0.5), (int)(line+0.5));
//    displayGathers ((int)(line+0.5));
    }
  }
}

void SeisSelectIpcPop::displayGather (int cmp, int line)
{
  if (made() && _sp && (_file_regularized =
    learnFileOrganization(IPC::HDR_MIDPOINT_XGRID,IPC::HDR_MIDPOINT_YGRID))) {
    long iskp = iskpToGather (cmp, line);
    if (iskp >= 0) {
      long ntot = _gather_size;
      long ndo = 1;
      long nskp = 0;
      long tdec = 1;
      resetDataSelectionParameters (ntot, iskp, ndo, nskp, tdec, _sp->tmin(),
        _sp->tmax());
      DoAction ();
    }
    else {
      fprintf (stderr, 
        "Failed to display gather at CMP=%d, LINE=%d on\n%s\n"
         "bad coordinates\n",
        cmp, line, _sp->filename());
    }
  }
  else {
    fprintf (stderr,
      "Failed to display gather at CMP=%d, LINE=%d\n%s\nnot regularized\n",
      cmp, line, _sp->filename());
  }
}

void SeisSelectIpcPop::displayGathers (int line)
{
  if (made() && _sp && (_file_regularized =
    learnFileOrganization(IPC::HDR_MIDPOINT_XGRID,IPC::HDR_MIDPOINT_YGRID))) {
    long iskp = iskpToBeginningOfLine (line);
    if (iskp >= 0) {
      long ntot, ndo, nskp;
      getLinePattern (&ntot, &ndo, &nskp);
      long tdec = 1;
      resetDataSelectionParameters (ntot, iskp, ndo, nskp, tdec, _sp->tmin(),
        _sp->tmax());
      DoAction ();
    }
    else {
      fprintf (stderr, 
        "Failed to display gathers on LINE=%d on\n%s\n"
         "bad coordinates\n",
        line, _sp->filename());
    }
  }
  else {
    fprintf (stderr,
      "Failed to display gathers on LINE=%d\n%s\nnot regularized\n",
      line, _sp->filename());
  }
}

// dont simply use the SeisSelect::resetDataSelectionParameters.  it was
//   written for geopress when using 3D data sets where the user is being
//   informed of the size of a single line in the 3D data set.  For the
//   general cbyt when gathers are being plotted, the user needs to be kept
//   aware of the size of the entire data set.
void SeisSelectIpcPop::resetDataSelectionParameters (long  ntot, long  iskp,
  long  ndo,  long  nskp, long  tdec, float tmin, float tmax)
{
  _data_sel1->SetValue(NTOT,   ntot );
  _data_sel1->SetValue(ISKP,   iskp );
  _data_sel1->SetValue(NDO,    ndo  );
  _data_sel1->SetValue(NSKP,   nskp );
  _data_sel2->SetValue(TDEC,   tdec );
  _data_sel2->SetValue(TMIN,   tmin );
  _data_sel2->SetValue(TMAX,   tmax );
  wprocVAShowMsg(_information,
       "Total Nplt: %d,    Tmin: %4.3f,   Tmax: %4.3f,   Srval %4.4f",
      _sp->totalTraces(),      tmin,          tmax,    _sp->srval());
 
}

int SeisSelectIpcPop::learnFileOrganization (int hdr_cmp, int hdr_line)
{
  int retval;

  if (!_active_file || strcmp(_sp->filename(),_active_file)) {
    // this is the same SeisPlot as last time
    TF_Global *globals = workstation_globals_get_globals (_sp->filename(),
      &_istat);

    if (globals) {
      int open = 1;
      int nhwd = workstation_globals_get_nhdwd (globals);
      double *dhds0 = (double *)malloc ((size_t)nhwd*sizeof(double));
      double *dhds1 = (double *)malloc ((size_t)nhwd*sizeof(double));
      float trace[1];
      int lun;
      int ns = 1;
      long nt = _sp->totalTraces ();
      int k2, have_gather, have_line, tnum;
      have_gather = _gather_size = _num_gathers = 0;
      have_line = _line_size = _num_lines = 0;
      int cmp_index = hdr_cmp - 1;
      int line_index = hdr_line - 1;
      for (k2 = 0; !_istat && k2 < nt;) {
	tnum = k2 + 1;
	trciof77wrapper_get_trace_ (_sp->filename(), &open, dhds0, trace,
        &tnum, &lun, &_istat, &nhwd, &ns);
	if (!_istat) {
	  if (k2 == 0) {
	    _first_gather = (int)dhds0[cmp_index];
	    _last_gather  = (int)dhds0[cmp_index];
	    _gather_size++;
	    _first_line = (int)dhds0[line_index];
	    _last_line  = (int)dhds0[line_index];
	    _line_size++;
	    open = 0;
	    k2++;
	  }
	  else {
	    if (!have_gather) {
	      if (_first_gather == (int)dhds0[cmp_index]) {
		_gather_size++;
		k2++;
	      }
	      else {
		have_gather = 1;
		// look at the last trace to see if this is a single line
		tnum = nt;
		trciof77wrapper_get_trace_ (_sp->filename(), &open, dhds1,
                  trace, &tnum, &lun, &_istat, &nhwd, &ns);
		if (!_istat                          && 
		  _last_line == (int)dhds1[line_index]   ) {
		  // file is a single line
		  _last_gather = (int)dhds1[cmp_index];
		  _num_gathers = nt / _gather_size;
		  k2++;
		  if (_num_gathers*_gather_size == nt) {
		    // the length of the single line is predictable
		    _line_size = nt;
		    have_line = 1;
		    _num_lines = 1;
		    retval = 1; // its good!!!!
		    k2 = nt;
		  }
		}
	      }
	    }
	    if (!have_line) {
	      if (_first_line == (int)dhds0[line_index]) {
		_last_gather = (int)dhds0[cmp_index];
		if (!have_gather) {
		  _line_size++;
		}
		else {
		  _line_size += _gather_size;
		  k2 += _gather_size;
		}
	      }
	      else {
		k2 = nt;
	      }
	    }
	  }
	}
      }
      if (_istat) {
	// error opening trace file
	retval = 0;
      }
      else if (!have_line) {
	_num_gathers = _line_size / _gather_size;
	if (_num_gathers*_gather_size == _line_size) {
	  // the first line is predictable
	  _num_lines = nt / _line_size;
	  if (_num_lines * _line_size == nt) {
	    // look at the last trace to verify the findings
	    tnum = nt;
	    trciof77wrapper_get_trace_ (_sp->filename(), &open, dhds0, trace,
              &tnum, &lun, &_istat, &nhwd, &ns);
	    if (!_istat                            && 
		_last_gather == (int)dhds0[cmp_index]   ) {
	      _last_line = (int)dhds0[line_index];
	      retval = 1; // its good!!!!
	    }
	    else {
	      // prestack data is not regularized
	      retval = 0;
	    }
	  }
	  else {
	    // prestack data is not regularized
	    _num_lines = 0;
	    retval = 0;
	  }
	}
	else {
	  // prestack data is not regularized
	  _num_gathers = 0;
	  retval = 0;
	}
      }
      int istat; // preserve _istat
      trciof77wrapper_close_file_ (&lun, &istat);
      free (dhds0);
      free (dhds1);
    }
    else {
      // error getting globals
      retval = 0;
    }
    if (_active_file) {
      free (_active_file);
      _active_file = 0;
    }
    _active_file = (char *)malloc ((strlen(_sp->filename())+1)*sizeof(char));
    strcpy (_active_file, _sp->filename());
  }
  else {
    // previously did the work
    retval = _file_regularized;
  }
  return retval;
}

int SeisSelectIpcPop::iskpToGather (int cmp, int line)
{
  int retval;

  if (_file_regularized) {

    float x;
    int line_count;
    if (_num_lines > 1) {
      x = (float)(line - _first_line)
	/ (float)(_last_line - _first_line);
      line_count = (int)(x * (float)(_num_lines - 1) + 0.5);
    }
    else {
      line_count = 0;
    }

    int cmp_count;
    if (_num_gathers > 1) {
      x = (float)(cmp - _first_gather)
        / (float)(_last_gather - _first_gather);
      cmp_count = (int)(x * (float)(_num_gathers - 1) + 0.5);
    }
    else {
      cmp_count = 0;
    }

    if (line_count < 0 || line_count > _num_lines-1   ||
	cmp_count < 0 ||  cmp_count > _num_gathers-1   ) {
      // cmp,line don't work
      retval = -1;
    }
    else {
      retval = line_count * _line_size + cmp_count * _gather_size;
    }
  }
  else {
    retval -1;
  }
  return retval;
}

int SeisSelectIpcPop::iskpToBeginningOfLine (int line)
{
  int retval;

  if (_file_regularized) {

    float x;
    int line_count;
    if (_num_lines > 1) {
      x = (float)(line - _first_line)
	/ (float)(_last_line - _first_line);
      line_count = (int)(x * (float)(_num_lines - 1) + 0.5);
    }
    else {
      line_count = 0;
    }

    if (line_count < 0 || line_count > _num_lines-1) {
      // cmp,line don't work
      retval = -1;
    }
    else {
      retval = line_count * _line_size;
    }
  }
  else {
    retval -1;
  }
  return retval;
}

void SeisSelectIpcPop::getLinePattern (long *ntot, long *ndo, long *nskp)
{
  if (_file_regularized) {
    // make sure that the current NPLT attempts to span one line and nothing
    //   more or less
    long num_of_gathers = _sp->nplt () / _gather_size;
    if (num_of_gathers < 2) {
      // must have at least two, perhaps the first and last
      num_of_gathers = 2;
    }
    if (num_of_gathers > _line_size/_gather_size) {
      // cannot have more than are there
      num_of_gathers = _line_size / _gather_size;
    }

    // make sure that current NDO does at least one gather
    if (num_of_gathers == _line_size/_gather_size) {
      // every prestack trace is used
      *ndo = 1;
    }
    else if (_sp->ndo()  > _gather_size) {
      // multiple gathers can be done, but must be a whole number of them
      *ndo = (int)(_sp->ndo() / _gather_size) * _gather_size;
    }
    else {
      // force at least one full gather is used
      *ndo = _gather_size;
    }

    // make sure that current NSKP skips a whole number of gathers
    long intervals = num_of_gathers * _gather_size / *ndo - 1;
    if (intervals < 1) intervals = 1;
    if (*ndo == 1) {
      // every prestack trace is used
      *nskp = 0;
    }
    else {
      long num_gathers_to_do = *ndo / _gather_size;
      long num_gathers_not_done = _line_size / _gather_size - num_of_gathers;
      long num_gathers_skipped = num_gathers_not_done / intervals;
      *nskp = num_gathers_skipped * _gather_size;
    }

    *ntot = num_of_gathers * _gather_size;
    if (intervals*((*ndo)+(*nskp))+(*ndo) > _line_size) {
      *ndo  = 1;
      *nskp = 0;
    }
  }
  else {
    *ntot = _sp->nplt ();
    *ndo  = _sp->ndo  ();
    *nskp = _sp->nskp ();
  }
}
