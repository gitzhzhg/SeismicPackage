// fgqc_trace_counter.cc
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

#include "fgqc/fgqc_trace_counter.hh"
#include "fgqc/fgqc_pop.hh"
#include "fgqc/fgqc_plot_constants.hh"
#include "geom/field_geometry.hh"
#include "fgqc/fgqc_plot.hh"
#include "named_constants.h"

FgQcTraceCounter::FgQcTraceCounter (long data_loc, FgQcPop *pop):
  _data_loc        (data_loc),
  _fg              (pop->fg()),
  _coord_system    (pop->getCoordinateSystem())
{
  _xmin = MinimumValue(pop->getUserLeft(),pop->getUserRight());
  _xmax = MaximumValue(pop->getUserLeft(),pop->getUserRight());
  _ymin = MinimumValue(pop->getUserTop(),pop->getUserBottom());
  _ymax = MaximumValue(pop->getUserTop(),pop->getUserBottom());
}

FgQcTraceCounter::~FgQcTraceCounter ()
{
}

char *FgQcTraceCounter::questionText ()
{
  char num_trc_str[12];
  static char string[200];
  long num_traces = computation ();
  DataLocation data_loc = (DataLocation)_data_loc;

  sprintf(string,"%s","");//default

  switch(data_loc)
    {
    case CMP_DISTRIBUTION:
      if(num_traces > 4000)
        {
        sprintf (num_trc_str, "%d", num_traces);
        strcpy (string, "The number of bins you requested is ");
        strcat (string, num_trc_str);
        strcat (string, " which may take awhile.\n");
        strcat (string, "----Do you want to proceed?---");
        }
    break;

    default:
      if (num_traces > 5000) 
        {
        sprintf (num_trc_str, "%d", num_traces);
        strcpy (string, "The process will require ");
        strcat (string, num_trc_str);
        strcat (string, " trace headers to be computed.\n");
        strcat (string, "----\nComputing just 5000 is quick.\n");
        strcat (string, "----\nDo you want to proceed?");
        }
    break;
    }

  return string;

}

//============================================================================
//========================= Find number of traces ============================
//============================================================================
long FgQcTraceCounter::computation ()
{
long num_traces = 0;
long i, j, k, l, trace;
float xloc, yloc;
double dxloc, dyloc;
DataLocation data_loc = (DataLocation)_data_loc;
long num_gathers = _fg->numCmpGathers ();
long fold;
long num_lines = _fg->numLines ();
long num_flags, num_sources, group, num_channels, num_receivers;
double xgrid, ygrid;




  switch(data_loc)
    {
    case PLOT_AT_CMP:
      for (i = 0; i < num_gathers; i++) {
        fold = _fg->foldOfStack (i);
        for (j = 0; j < fold; j++) {
          if (_coord_system == SURVEYSYSTEM) {
            _fg->getCmpTraceLoc (i, j, &dxloc, &dyloc);
          }
          else /*if (_coord_system == GRIDSYSTEM)*/ {
            _fg->getCmpTraceGrid (i, j, &dxloc, &dyloc);
          }
          xloc = (float)dxloc;
          yloc = (float)dyloc;
          if (xloc >= _xmin && xloc <= _xmax &&
              yloc >= _ymin && yloc <= _ymax   ) num_traces++;
          }
        }
      break;  


    case CMP_DISTRIBUTION:
      for(i=0; i<_fg->numCmpGathers(); i++)
        {
        if(_coord_system == SURVEYSYSTEM)
          _fg->getCmpLocBinCenter(i, &dxloc , &dyloc );
        else
          _fg->getCmpGridBinCenter(i, &dxloc , &dyloc );
        if(dxloc >= _xmin && dxloc <= _xmax && 
           dyloc >= _ymin && dyloc <= _ymax)
          {
          for(j = 0; j < _fg->foldOfStack(i); j++) ++num_traces;
          }
        }
      break;


    default:
      for (i = 0; i < num_lines; i++) {
        num_flags = _fg->numFlagsOnLine (i);
        for (j = 0; j < num_flags; j++) {
          if (data_loc == PLOT_AT_SOURCE) {
            num_sources = _fg->numSourcesAtFlag (i, j);
            for (k = 0; k < num_sources; k++) {
              group = _fg->sourceGroupNumber (i, j, k);
              if (group > 0) {
                num_channels = _fg->findNumChannelsInGroup (group);
                _fg->getSkiddedSourceCoords (group, &dxloc, &dyloc);
                if (_coord_system == GRIDSYSTEM) {
                  xgrid = _fg->getXgridCoord (dxloc, dyloc);
                  ygrid = _fg->getYgridCoord (dxloc, dyloc);
                  dxloc = xgrid;
                  dyloc = ygrid;
                }
                xloc = (float)dxloc;
                yloc = (float)dyloc;
                if (xloc >= _xmin && xloc <= _xmax &&
                    yloc >= _ymin && yloc <= _ymax   ) {
                  for (l = 0; l < num_channels; l++) {
                    trace = _fg->findTraceNumber (group, l);
                    if (trace > 0) num_traces++;
                  }
                }
              }
	    }
          }
          else /*if (data_loc == PLOT_AT_RECEIVER)*/ {
            num_receivers = _fg->numReceiversAtFlag (i, j);
            for (k = 0; k < num_receivers; k++) {
              trace = _fg->receiverTraceNumber (i, j, k);
              if (trace > 0) {
                _fg->getSkiddedTraceCoords (i, j, k, &dxloc, &dyloc);
                if (_coord_system == GRIDSYSTEM) {
                  xgrid = _fg->getXgridCoord (dxloc, dyloc);
                  ygrid = _fg->getYgridCoord (dxloc, dyloc);
                  dxloc = xgrid;
                  dyloc = ygrid;
                }
                xloc = (float)dxloc;
                yloc = (float)dyloc;
                if (xloc >= _xmin && xloc <= _xmax &&
                    yloc >= _ymin && yloc <= _ymax   ) num_traces++;
  	      }
            } 
	  }
        }
      }
      break;
    }//end switch   

  return num_traces;
}
