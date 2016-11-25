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
// class that creates the color bar builder menu
#ifndef COLOR_BAR_BUILDER_POP_HH
#define COLOR_BAR_BUILDER_POP_HH

#include "sl/sl_form_pop.hh"

class CBBLevelsGui;
class CBBColSysGui;
class CBBAttrRngGui;
class CBBColFillGui;
class CBBColExtGui;
class CBBColROGui;
class CBBColFIOGui;
class CBBColSetGui;
class CBBColFillSettings;
class CBBColExtSettings;
class CBBRGBSet;
class CBBCPSAmpProc;
class ColorSelectorPop;
class SLCenteringForm;
class ColorFileIO;

class ColorBarBuilderPop :  public SLFPopSep {

friend class CBBLevelsGui;
friend class CBBAttrRngGui;
friend class CBBColSetGui;
friend class CBBColROGui;
friend class CBBColSysGui;

public:
  enum ColorCoordinate {
    CC0,					// Color coordinate 0
    CC1,					// Color coordinate 1
    CC2						// Color coordinate 2
  };

  ColorBarBuilderPop				// constructor
    (Widget parent,				//   parent widget
     char *name,				//   name
     HelpCtx hctx,				//   context sensitive help
     int max_levels,				//   maximum color levels
     ColorFileIO *fio = NULL);			//   color file I/O object

  ColorBarBuilderPop				// constructor
    (SLDelay *container,			//   container
     char *name,				//   name
     HelpCtx hctx,				//   context sensitive help
     int max_levels,				//   maximum color levels
     ColorFileIO *fio = NULL);			//   color file I/O object

  virtual ~ColorBarBuilderPop ();		// destructor

  virtual Widget make				// make function
    (Widget parent = 0);			//   parent widget

  virtual void DoAction ();			// do when OK or APPLY pushed

  virtual void UndoInput ();			// do when CANCEL pushed

  CBBColSysGui *colSysGui ()			// return color system GUI
    { return _col_sys_gui; }

  CBBColFillSettings *colFillSetter ();		// rtn col fill setting data

  CBBColExtSettings *colExtSetter ();		// rtn col extrap setting data

  CBBLevelsGui *levelsGui ()			// rtn color levels Gui object
    { return _levels_gui; }

  CBBColSetGui *colSetGui ()			// rtn color set Gui object
    { return _col_set_gui; }

  ColorFileIO *fileIO ()			// rtn color file I/O object
    { return _fio; }

  int maxLevels ()				// rtn max color levels
    { return _max_levels; }

  virtual float *getLUT () = 0;			// return the RGBZ LUT to use

  virtual int getLUTSize () = 0;		// get the color LUT size

  virtual Boolean LUTChanged			// True if color LUT chnged
    (CBBRGBSet *rgb) = 0;			//   givn the obj to put it in

  virtual Boolean LUTNotEqualToFile		// True if user chnged input
    (char *filename,				//   given the file to chk
     CBBRGBSet *rgb) = 0;			//   givn the LUT to check

  virtual void getAmps ();			// define the amplitude funct

  virtual Boolean ampsChanged ()		// True if amplitudes chnged
    { return False; }

  virtual void managing ();			// called when this is managed

  virtual Boolean setFileIn			// announces current input
    (char *filename);				//   given file name

  void changeRGB ();				// change to nonfile RGB

  int readColorFile 				// read a color file
    (char *filename);				//   given file name

protected:
  virtual void init ();				// constructor helper

  int writeColorFile 				// writes output file
    (char *filename,				//   given file name
     CBBRGBSet *rgb);				//   a set of RGB's

  virtual void setFileOut 			// communicates current output
    (char * /* filename */) {}

  ColorFileIO
    *_fio;					// color file I/O object

  CBBLevelsGui
    *_levels_gui;				// GUI for color levels & prms

  CBBColSysGui
    *_col_sys_gui;				// GUI for sel color system

  CBBAttrRngGui
    *_attr_rng_gui;				// GUI for sel attribute range

  CBBColFillGui
    *_col_fill_gui;				// GUI for sel color fill mthd

  CBBColExtGui
    *_col_ext_gui;				// GUI for sel clr extrap mthd

  CBBColFIOGui
    *_col_fio_gui;				// GUI for color bar file I/O

  CBBColROGui
    *_col_ro_gui;				// GUI for color readout

  CBBColSetGui
    *_col_set_gui;				// GUI for color set

  CBBColFillSettings
    *_rgb_fill_settings,			// RGB interpolation setttings
    *_bhs_fill_settings;			// BHS interpolation setttings

  CBBColExtSettings
    *_rgb_ext_settings,				// RGB extrapolation setttings
    *_bhs_ext_settings;				// BHS extrapolation setttings

  CBBCPSAmpProc
    *_amp;					// CPS style amplitude procssr

  ColorSelectorPop
    *_col_sel_pop;				// color selector popup

  SLCenteringForm
    *_cbt,					// holds title
    *_cfg,					// holds sel color fill GUI
    *_ceg,					// holds sel color extr GUI
    *_crg,					// holds color readout GUI
    *_csg;					// holds color set GUI

  char
    *_name;					// given name

  int
    _max_levels,				// max levels of color allowed
    _status;					// status flag

};

#endif
