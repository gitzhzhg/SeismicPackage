#include <math.h>
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
#include "plot/pick_base.hh"
#include "sp/seis_plot.hh"
#include "fgmap/fg_map_pick.hh"
#include "fgmap/fg_loc_out.hh"
#include "fgmap/fg_map_to_flag.hh"
#include "fgmap/fg_pick_action.hh"
#include "geom/field_geometry.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/fgxp_2d_plot.hh"
#include "fgxp/fgxp_data.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"
#include "sl/prim_support.hh"
#include "sl/error_handler.hh"

static char *mode_str = "Mode: Picking";
static const char * const info_token = "MAPINFO";
static const char * const sel_token  = "SELECTFLAGS";
static const char * const dist_token = "DISTANCE";
static const char * const close_token ="CLOSEST";
static const char * const showrec_token ="SHOWREC";
static const char * const interpolate_token ="INTERPOLATE";

static const char * const mapinfo_fallback = 
 "mouse*MAPINFO:  BTN#1: Select Active Flag,  BTN#2: Select Active Line,\
  BTN#3: Popup Menu \\nShift BTN#1: Information on Flag  \
Cntl BTN#1: Select Line  \
Cntl BTN#2: UnSelect Line";

static const char * const showrec_fallback = 
 "mouse*SHOWREC:  BTN#1: Select Receivers,  BTN#2: Select Sources,\
  BTN#3: Popup Menu \\n";

static const char * const selectflags_fallback = 
 "mouse*SELECTFLAGS:  BTN#1: Select Flags Area,  BTN#2: Unselect Flag Area,  \
  BTN#3: Popup Menu \\n\
Shift BTN#1: Select Flags On Line  \
Shift BTN#2: UnSelect Flags On Line \
Cntl BTN#1: Select Line   \
Cntl BTN#2: UnSelect Line";

static const char * const distance_fallback = 
 "mouse*DISTANCE:  BTN#1: Distance,  BTN#2: none,  BTN#3: Popup Menu";

static const char * const closest_fallback = 
 "mouse*CLOSEST:  BTN#1: Closest Line,  BTN#2: Closest Line,  \
BTN#3: Popup Menu";

static const char * const interpolate_fallback = 
 "mouse*INTERPOLATE:  BTN#1: Interpolate Area,  Shift BTN#1: Interpolate Line";




FgMapPick::FgMapPick( SeisPlot           *sp, 
                      FieldGeometry      *fg, 
                      FgXpPlotLinkedList *plot_list,
                      int                 mode,
                      FgLocOut           *fg_loc_out) :
          PickBase(sp, mode_str, info_token, mapinfo_fallback, 
	           XC_tcross, beep, True, &PrimSupport::updateEverything),
                   _fg(fg), _plot_list(plot_list), 
                   _mode(mode), _working_vect(NULL), _fg_loc_out(fg_loc_out),
                   _xlat(NULL), _is_grid(False), _only_selected(False)
{
 assert(_mode!=ClosestLine);
 init(sp);
}


FgMapPick::FgMapPick( SeisPlot           *sp, 
                      FieldGeometry      *fg, 
                      FgMapToFlag        *xlat,
                      Boolean             is_grid) :
          PickBase(sp, mode_str, info_token, mapinfo_fallback, 
	           XC_tcross, beep, True, &PrimSupport::updateEverything),
                   _fg(fg), _plot_list(NULL), _mode(ClosestLine), 
                   _working_vect(NULL), _fg_loc_out(NULL), _xlat(xlat),
                   _is_grid(is_grid), _only_selected(False)
{
 assert(_mode==ClosestLine);
 init(sp);
}


void FgMapPick::init(SeisPlot *sp)
{
 changeHelp();
 _vlist= new SeisVectLinkedList();
 _vlist->addPlot(sp);
 _action= new FgPickAction(sp,_fg,_plot_list);
}




FgMapPick::~FgMapPick()
{
  delete _vlist;
  delete _action;
}

void FgMapPick::setMode(int mode)
{
 _mode= mode;
 changeHelp();
}

void FgMapPick::changeHelp()
{
  switch (_mode) {
         case SelectFlags:  changeHelpToken(sel_token, selectflags_fallback); 
                            break;
         case FlagInfo:     changeHelpToken(info_token, mapinfo_fallback); 
                            break;
         case Distance:     changeHelpToken(dist_token, distance_fallback); 
                            break;
         case ShowRec:      changeHelpToken(showrec_token, showrec_fallback); 
         case ShowShot:     break;
         case ClosestLine:  changeHelpToken(close_token, closest_fallback); 
                            break;
         case Interpolate:  changeHelpToken(interpolate_token, 
                                            interpolate_fallback); 
                            break;
         default:           assert(0); break;
   } // End Switch
}

Vector* FgMapPick::getCurrentIndex( int   x, 
                                    int   y, 
                                    long *line_index, 
                                    long *flag_index,
                                    long *vect_index,
                                    long *source_index)
{
  Vector *vect;
  FgXpData *xpdata;
  int i, numsrcs;
  long *srcary= NULL;

  vect= _plot_list->getClosest(x,y,getPlot());
  if (vect) {
      i= vect->closestIndex(x,y,getPlot());
      xpdata= (FgXpData*)vect->getData();   // yes, a down cast
      *flag_index=  xpdata->translateDataIndexToFlag(i,&srcary,&numsrcs);
      *line_index= xpdata->getXIndex();
      if (vect_index) *vect_index= i;
      if (source_index) {
           if (numsrcs > 0) {
                 *source_index= srcary[0];
                 free(srcary);
           }
           else 
                 *source_index= 0;
      } // end if
  } // end if vect
  return vect;
}

void FgMapPick::selectLine(int x, int y, Boolean select)
{
  long flag_index;
  long line_index;
  long source_index;
  char select_char;
  if (select)
        select_char= SELECT_YES;
  else
        select_char= SELECT_MAYBE;
  if (getCurrentIndex( x, y, &line_index, &flag_index, NULL, &source_index))
        _fg->setLineSelectValue(line_index, select_char);
}


void FgMapPick::selectActiveFlag(int x, int y)
{
  long flag_index;
  long line_index;
  long source_index;
  if (getCurrentIndex( x, y, &line_index, &flag_index, NULL, &source_index))
        _fg->setActiveSourceIndices( line_index, flag_index, source_index);
}


void FgMapPick::selectActiveLine(int x, int y)
{
  long flag_index;
  long line_index;
  if (getCurrentIndex( x, y, &line_index, &flag_index))
        _fg->setActiveLineIndex(line_index);
}


void FgMapPick::setClosestLineActive(int x, int y)
{
  long flag_index;
  long line_index;
  long shot_index;
  FgMapToFlag::SkidType skid_type;
  if ( _xlat->findClosest( (SeisPlot*)getPlot(), x,y, &line_index, &flag_index,
                           &skid_type, &shot_index, _is_grid,_only_selected)) {
      if (shot_index == -1) shot_index= 0;
      _fg->setActiveSourceIndices( line_index, flag_index, shot_index);
  }
}

void FgMapPick::showFlags(int show_type, int x, int y)
{
  static char *s_ood= "Source gathers are out of date.";
  static char *r_ood= "Receiver gathers are out of date.";
  long flag_index;
  long line_index;
  long source_index;
  if (getCurrentIndex( x, y, &line_index, &flag_index, NULL, &source_index)) {
     _fg->setActiveSourceIndices( line_index, flag_index, source_index);
     if (show_type == ShowRec) {
          if (_fg->sourceGathersOutOfDate()) {
              ErrorHandler eh(getPlot()->getWidget());
              eh.deliverError(s_ood);
          }
          else {
              _plot_list->clearSelections();
              _plot_list->selectRcvsFromSrc(x,y,getPlot(), 'Y');
          }
     } // end if show_type == ShowRec
     else {
          if (_fg->receiverGathersOutOfDate()) {
              ErrorHandler eh(getPlot()->getWidget());
              eh.deliverError(r_ood);
          }
          else {
              _plot_list->clearSelections();
              _plot_list->selectSrcsFromRcv(x,y,getPlot(), 'Y');
          }
     } // end else
  } // end if getCurrentIndex


}

void FgMapPick::findFlag(int x, int y)
{
  long flag_index;
  long line_index;
  long source_index;
  long line;
  float flag;
  char outstr[600];
  char grp_num_str[40]= "Don't Know";
  char has_rec[10], has_source[10];
  ErrorHandler eh( getPlot()->getWidget(), "Flag Information", False);
  static char * const FLGINFO = 
        "Line Number:          %d\n\
         Shot Point Number:    %4.3f\n\
         Has Source            %s\n\
         Source Group Number:  %s\n\
         Has Receiver          %s\n\
         X Location            %4.3f\n\
         Y Location            %4.3f\n\
         X Grid                %4.3f\n\
         Y Grid                %4.3f\n\
         Elevation             %4.3f";

  if (!getCurrentIndex( x, y, &line_index, &flag_index, NULL, &source_index))
       return;
  line= _fg->getLineNumber( line_index);
  flag= _fg->getShotpoint( line_index, flag_index);
  
  if (!_fg->sourceGathersOutOfDate()) {
       if (_fg->flagHasSource(line_index, flag_index)) {
            strcpy(has_source, "Yes");
            sprintf(grp_num_str, "%d", 
                 _fg->sourceGroupNumber(line_index,flag_index, source_index));
              
       } // end if
       else {
            strcpy(has_source, "No");
            strcpy(grp_num_str, "No Sources");
       }
  } // end if
  else
       strcpy(has_source, "Don't Know");

  if (!_fg->receiverGathersOutOfDate()) {
       if (_fg->flagHasReceiver(line_index, flag_index))
             strcpy(has_rec, "Yes");
       else
             strcpy(has_rec, "No");
  } // end if
  else
       strcpy(has_rec, "Don't Know");

 
  /*
   * Do something with this information besides just print it out.
   */
  sprintf(outstr, FLGINFO, line, flag, has_source, grp_num_str, has_rec,
                           _fg->getXloc(line_index, flag_index),
                           _fg->getYloc(line_index, flag_index),
                           _fg->getXgrid(line_index, flag_index),
                           _fg->getYgrid(line_index, flag_index),
                           _fg->getElevation(line_index, flag_index)
                        );
  eh.deliverInformation(outstr, "Flag Information");
}




void FgMapPick::startDistanceSelection(int x, int y)
{
   _drag_in_progress= False;
   _xsqr[1]= _xsqr[0]= getPlot()->xWC(x);
   _ysqr[1]= _ysqr[0]= getPlot()->yWC(y);
   VectData *dist_data= new VectData(2, _xsqr, _ysqr);
   _working_vect=  _vlist->add(dist_data, "green", 2, True);
   if (_fg_loc_out) {
        _fg_loc_out->setAttr1(APP_CONTROL);
        _fg_loc_out->setAttr2(APP_CONTROL);
   }
}

void FgMapPick::dragDistanceSelection(int x, int y)
{
   _drag_in_progress= True;
   _xsqr[1]= getPlot()->xWC(x);
   _ysqr[1]= getPlot()->yWC(y);

   // here goes the downcast again...
   ((VectData*)_working_vect->getData())->replace(1, 1, _xsqr+1, _ysqr+1);

   float dist= sqrt( ((_xsqr[1]- _xsqr[0])*(_xsqr[1]- _xsqr[0])) +
                     ((_ysqr[1]- _ysqr[0])*(_ysqr[1]- _ysqr[0])) );

   if (_fg_loc_out) {
        char wkstr[30];
        sprintf(wkstr, "D: %4.23f\n", dist);
        _fg_loc_out->setAttr1Field("DISTANCE");
        _fg_loc_out->setAttr2Field(wkstr);
   }
   else
        printf("Distance: %3.2f\n", dist);
}

void FgMapPick::endDistanceSelection(int, int)
{
   delete _working_vect->getData();
   _vlist->remove(_working_vect);
   _drag_in_progress= False;
   if (_fg_loc_out) {
        _fg_loc_out->resetLastAttr1();
        _fg_loc_out->resetLastAttr2();
   }
}


void FgMapPick::setPlotIsGrid(Boolean g)
{
  _is_grid= g;
}
void FgMapPick::setPlotIsSelected(Boolean g)
{
  _only_selected= g;
}


void FgMapPick::noModButtonOnePress(int x, int y)
{
    switch (_mode) {
         case FlagInfo:     break;
         case SelectFlags:  _action->startGroupSelection(x,y, True);
                            break;
         case Distance:     startDistanceSelection(x,y); 
                            break;
         case ShowRec:      break;
         case ShowShot:     break;
         case ClosestLine:  break;
         case Interpolate:  _action->startGroupInterpolation(x,y, 
                                               FG_XLOC, FG_YLOC); 
                            break;
         default:        assert(0); break;
    } // End Switch
}

void FgMapPick::noModButtonOneMotion(int, int x2, int, int y2)
{
   switch (_mode) {
         case SelectFlags:  _action->dragGroup(x2,y2);
                            break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case Distance:     dragDistanceSelection(x2,y2); 
                            break;
         case ShowShot:     break;
         case ClosestLine:  break;
         case Interpolate:  _action->dragGroup(x2,y2);
                            break;
         default:           assert(0); break;
   } // End Switch
}


void FgMapPick::noModButtonOneRelease(int x1, int x2, int y1, int y2)
{
   switch (_mode) {
         case SelectFlags:  _action->endGroup(x1, y1, x2, y2);
                            break;
         case FlagInfo:     selectActiveFlag(x1,y1); break;
         case ShowRec:      showFlags(ShowRec,x1,y1);
                            break;
         case ShowShot:     break;
         case Distance:     endDistanceSelection(x2,y2); 
                            break;
         case ClosestLine:  setClosestLineActive(x1,y1);
                            break;
         case Interpolate:  _action->endGroup(x1, y1, x2, y2); 
                            break;
         default:           assert(0); break;
   } // End Switch
 }




void FgMapPick::shiftButtonTwoPress(int x, int y)
{
   switch (_mode) {
//         case SelectFlags:  selectActiveLine(x,y); break;
         case SelectFlags:  
                            _action->startLineSnapSelection(x,y, False);
                            break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  break;
         default:           assert(0); break;
   } // End Switch
}


void FgMapPick::shiftButtonTwoMotion(int , int x2, int , int y2)
{
   switch (_mode) {
         case SelectFlags:  _action->dragLineSnap(x2,y2); break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  break;
         default:           assert(0); break;
   } // End Switch
}


void FgMapPick::shiftButtonTwoRelease(int x1, int x2, int y1, int y2)
{
   switch (_mode) {
         case SelectFlags:  _action->endLineSnap(x1,y1,x2,y2); break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  break;
         default:           assert(0); break;
   } // End Switch
}



void FgMapPick::cntlButtonTwoPress(int, int)
{
   switch (_mode) {
         case SelectFlags:  
                            break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate: 
                            break;
         default:           assert(0); break;
   } // End Switch
}

void FgMapPick::cntlButtonTwoMotion(int , int, int , int)
{
   switch (_mode) {
         case SelectFlags:  break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  break;
         default:           assert(0); break;
   } // End Switch
}


void FgMapPick::cntlButtonTwoRelease(int x1, int, int y1, int)
{
   switch (_mode) {
         case SelectFlags:  selectLine(x1, y1, False); break;
         case FlagInfo:     selectLine(x1, y1, False); break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  break;
         default:           assert(0); break;
   } // End Switch
}




void FgMapPick::shiftButtonOnePress(int x, int y)
{
   switch (_mode) {
         case SelectFlags:  _action->startLineSnapSelection(x,y, True);
                            break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  _action->startLineSnapInterpolation(x,y, 
                                               FG_XLOC, FG_YLOC); 
                            break;
         default:           assert(0); break;
   } // End Switch
}

void FgMapPick::shiftButtonOneMotion(int , int x2, int , int y2)
{
   switch (_mode) {
         case SelectFlags:  _action->dragLineSnap(x2,y2); break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  _action->dragLineSnap(x2,y2); break;
         default:           assert(0); break;
   } // End Switch
}


void FgMapPick::shiftButtonOneRelease(int x1, int x2, int y1, int y2)
{
   switch (_mode) {
         case SelectFlags:  _action->endLineSnap(x1,y1,x2,y2); break;
         case FlagInfo:     findFlag(x1,y1); break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  _action->endLineSnap(x1,y1,x2,y2); break;
         default:           assert(0); break;
   } // End Switch
}


void FgMapPick::cntlButtonOnePress(int, int)
{
   switch (_mode) {
         case SelectFlags:  
                            break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate: 
                            break;
         default:           assert(0); break;
   } // End Switch
}

void FgMapPick::cntlButtonOneMotion(int , int, int , int)
{
   switch (_mode) {
         case SelectFlags:  break;
         case FlagInfo:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  break;
         default:           assert(0); break;
   } // End Switch
}


void FgMapPick::cntlButtonOneRelease(int x1, int, int y1, int)
{
   switch (_mode) {
         case SelectFlags:  selectLine(x1, y1, True);  break;
         case FlagInfo:     selectLine(x1, y1, True); break;
         case ShowRec:      break;
         case ShowShot:     break;
         case Distance:     break;
         case ClosestLine:  break;
         case Interpolate:  break;
         default:           assert(0); break;
   } // End Switch
}




void FgMapPick::noModButtonTwoPress(int x, int y) 
{ 
    switch (_mode) {
         case FlagInfo:     break;
         case Distance:     break;
         case ShowRec:      break;
         case ShowShot:     break;

         case SelectFlags:  _action->startGroupSelection(x,y, False);
                            break;
         case ClosestLine:  break;
         case Interpolate:  break;
         default:        assert(0); break;
    } // End Switch
}

void FgMapPick::noModButtonTwoMotion(int, int x2, int, int y2)
{
   switch (_mode) {
         case SelectFlags:  _action->dragGroup(x2,y2);
                            break;
         case FlagInfo:     break;
         case Distance:     break;
         case ShowRec:      break;
         case ShowShot:     break;
         case ClosestLine:  break;
         case Interpolate:  break;
         default:           assert(0); break;
   } // End Switch
}



void FgMapPick::noModButtonTwoRelease(int x1, int x2, int y1, int y2)
{
   switch (_mode) {
         case SelectFlags:  _action->endGroup(x1, y1, x2, y2);
                            break;
         case FlagInfo:     selectActiveLine(x1,y1); break;
         case Distance:     break;
         case ShowRec:      showFlags(ShowShot,x1,y1);
                            break;
         case ShowShot:     break;
         case ClosestLine:  setClosestLineActive(x1,y1); break;
         case Interpolate:  break;
         default:           assert(0); break;
   } // End Switch
 }
