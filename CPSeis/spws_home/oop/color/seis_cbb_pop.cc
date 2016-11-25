// class that creates the color bar builder menu
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
#include "color/seis_cbb_pop.hh"
#include "color/cbb_rgb_set.hh"
#include "color/cbb_col_fio_gui.hh"
#include "color/cbb_col_set_gui.hh"
#include "color/cbb_cps_amp_proc.hh"
#include "color/cbb_levels_gui.hh"
#include "color/seis_color_file_io.hh"
#include "sp/seis_plot.hh"
#include "sp/sp_list.hh"
#include "sp/seis_color.hh"
#include "sp/seis_ctype.hh"
#include <stdio.h>
#include <stdlib.h>

SeisCBBPop::SeisCBBPop (Widget parent, char *name, HelpCtx hctx,
  SeisPlot *sp, int max_levels, SeisColorFileIO *fio, SeisColor *scr) :
  ColorBarBuilderPop (parent, name, hctx, max_levels, fio),
  SeisInform (sp),
  _sp   (sp),
  _scr  (scr)
{
  init ();
}

SeisCBBPop::SeisCBBPop (SLDelay *container, char *name, HelpCtx hctx,
  SeisPlot *sp, int max_levels, SeisColorFileIO *fio, SeisColor *scr) :
  ColorBarBuilderPop (container, name, hctx, max_levels, fio),
  SeisInform (sp),
  _sp   (sp),
  _scr  (scr)
{
  init ();
}

SeisCBBPop::~SeisCBBPop ()
{
  if (_list) delete _list, _list = 0;
  if (_amp)  delete _amp,  _amp = 0;
}

Widget SeisCBBPop::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  ColorBarBuilderPop::make (parent);

  return topWidget ();
    
}

void SeisCBBPop::addSP (SeisPlot *sp)
{
  if (_list && sp) {
    if (_list->top()) sp->shareColorsWith (_list->top());
    _list->add (sp);
  }
}

void SeisCBBPop::notCurrentInWindow (SeisPlot *sp)
{
  if (_sp && sp && _list) {
    if (_sp == sp) {
      _sp = sp->currentSPInWindow ();
      addSeisPlot (_sp);
      if (!_list->find(_sp)) addSP (_sp);
    }
  }
}

void SeisCBBPop::destroyed (SeisPlot *sp)
{
  if (_list) {
    if (_list->find(sp)) _list->remove (sp);
  }
}

void SeisCBBPop::newPlot (SeisPlot *sp)
{
  if (_sp && sp) {
    if (_sp == sp) {
      if (ampsChanged() || _col_set_gui->RGBChanged()) {
        getAmps ();
        _col_set_gui->changeRGB ();
      }
    }
  }
}

float *SeisCBBPop::getLUT ()
{
// user must free the array produced after calling this function
  if (!_scr) return 0;

  int num_colors = _scr->getNumColors ();
  float *rgbz = 0;
  rgbz = (float *)malloc ((size_t)(4*num_colors)*sizeof(float));
  if (!rgbz) return (float *)0;

  int k2, k3 = 0;
  for (k2 = 0; k2 < num_colors; k2++) {
    _scr->getAColor (k2, &rgbz[k3], &rgbz[k3+1], &rgbz[k3+2],
      &rgbz[k3+3]);
    k3 += 4;
  }

  return rgbz;
}

int SeisCBBPop::getLUTSize ()
{
  if (!_scr) return (int)0;

  int num_colors = _scr->getNumColors ();

  return num_colors;
}

Boolean SeisCBBPop::LUTChanged (CBBRGBSet *rgb)
{
  if (!rgb || !_scr) return False;

  int num_colors = _scr->getNumColors ();
  if (rgb->getSize() != num_colors) return True;

  float scr_red, scr_green, scr_blue, scr_attribute;
  float rgb_red, rgb_green, rgb_blue, rgb_attribute;
  int k2;
  for (k2 = 0; k2 < num_colors; k2++) {
    _scr->getAColor (k2, &scr_red, &scr_green, &scr_blue, &scr_attribute);
    rgb->getOne (k2, &rgb_red, &rgb_green, &rgb_blue, &rgb_attribute);
    if (scr_red != rgb_red || scr_green != rgb_green ||
        scr_blue != rgb_blue) return True;
  }

  return False;
}

void SeisCBBPop::getAmps ()
{
  float a_min, a_max;
  if (_sp) {
    a_min = _sp->minColorAmp ();
    a_max = _sp->maxColorAmp ();
  }
  else {
    a_min = 0;
    a_max = 1;
  }
  if (_amp) delete _amp, _amp = 0;
  _amp = new CBBCPSAmpProc ();
  _amp->setMinimum (a_min);
  _amp->setMaximum (a_max);
  _amp->setLevels (_max_levels);
  _amp->setCellsPerLevel (2);
}

Boolean SeisCBBPop::ampsChanged ()
{
  if (!_amp || !_sp) return False;
  if (_amp->minimum() != _sp->minColorAmp()) return True;
  if (_amp->maximum() != _sp->maxColorAmp()) return True;
  return False;
}

Boolean SeisCBBPop::LUTNotEqualToFile (char *filename, CBBRGBSet *rgb)
{
  Boolean not_equal = True;

  FILE *file_in;
  int len = strlen (filename);
  if (!len) return not_equal;

  if (!rgb) return not_equal;

  file_in = fopen (filename, "r");
  if (file_in == 0) return not_equal;

// dummy read of cps hardcoded 999 at first of file
  float val;
  int stat = fscanf (file_in, "%10f", &val);
  if (stat < 1) {
    fclose (file_in);
    return not_equal;
  }

// allocate the RGBZ array
  float fred, fgreen, fblue, attribute;
  float lred, lgreen, lblue;

// read to compare
  int rgb_size = rgb->getSize ();
  int k2;
  not_equal = False;
  for (k2 = 0; k2 < _max_levels; k2++) {
    stat = fscanf (file_in, " %f %f %f %f ", &fred, &fgreen, &fblue,
      &attribute);
    if (stat == EOF) {
      if (k2 != rgb_size) not_equal = True;
      k2 = _max_levels;
    }
    else if (stat != 4 || k2 > rgb_size-1) {
      not_equal = True;
      k2 = _max_levels;
    }
    else {
      rgb->getOne (k2, &lred, &lgreen, &lblue, &attribute);
      if (fred != lred || fgreen != lgreen || fblue != lblue) {
        not_equal = True;
        k2 = _max_levels;
      }
    }
  }
  fclose (file_in);

  return not_equal;
}

void SeisCBBPop::init ()
{
  getAmps ();

  _list = new SPList ();

  addSP (_sp);

}
