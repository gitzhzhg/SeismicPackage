#include "fgmap/fg_loc_out.hh"
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
#include "fgmap/fg_map_to_flag.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"
#include "sl/sl_pull_pop.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/fgxp_2d_plot.hh"
#include "fgxp/fgxp_data.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "wproc.h"
#include <stdio.h>
#include <stdlib.h>
#include <Xm/TextF.h>
#include <assert.h>


static String  defres[]= {
           "*XmTextField.editable:     False",
           "*XmTextField.traversalOn:  False",
           "*XmTextField.cursorPositionVisible : False",
           "*vout.rightOffset:    5",
           "*vout.bottomOffset:   5",
           "*xloc.columns:        12",
           "*yloc.columns:        12",
           "*line.columns:        12",
           "*flag.columns:        12",
           "*attr1.columns:       12",
           "*attr2.columns:       12",
           "*XmTextField.marginHeight: 1",
           "*XmTextField.marginWidth:  1",
           ".numColumns:               3",
           ".packing:             PACK_COLUMN",
           NULL };





FgLocOut::FgLocOut(Widget   p,
                   char     *name,
                   HelpCtx  hctx,
                   FieldGeometry *fg,
                   SeisPlot *sp,
                   FgXpPlotLinkedList *plot_list) :

               SLRowColumn(p, name, hctx, True, True, True, 
                           SLRowColumn::_VERTICAL), 
                           SeisInform(), _fg(fg), _trans_type_grid(False),
                           _xlat(NULL), _select_lines_only(False)
{
   assert(sp);
   setDefaultResources( XtDisplay(p), name, defres);
   addSeisPlot(sp, (void*) plot_list); 

   XtVaSetValues( topWidget(), XmNpacking,    XmPACK_COLUMN,
                               XmNnumColumns, 3,       NULL);

   setDefaultResources( XtDisplay(p), name, defres);

   _line= XtVaCreateManagedWidget("line", xmTextFieldWidgetClass, topWidget(),
                                  NULL);
   _flag= XtVaCreateManagedWidget("flag", xmTextFieldWidgetClass, topWidget(),
                                  NULL);

   _xloc= XtVaCreateManagedWidget("xloc", xmTextFieldWidgetClass, topWidget(),
                                  NULL);

   _yloc= XtVaCreateManagedWidget("yloc", xmTextFieldWidgetClass, topWidget(),
                                  NULL);

   _attr1= XtVaCreateManagedWidget("attr1", xmTextFieldWidgetClass, 
                                   topWidget(), NULL);

   _attr2= XtVaCreateManagedWidget("attr2", xmTextFieldWidgetClass, 
                                   topWidget(), NULL);

   _attr1_pop= new SLPullPop("attr1_pop", _attr1, hctx);
   _attr2_pop= new SLPullPop("attr2_pop", _attr2, hctx);
   _attr1_pop->setTitle("Display Attribute 1");
   _attr2_pop->setTitle("Display Attribute 2");

   addPopButtons(_attr1_pop, FG_ELEV);
   _attr1_pop->addRadio( "Plot Z Output",  SP_Z_OUTPUT);
   addPopButtons(_attr2_pop, FG_ELEV);
   _attr1_type= FG_ELEV;
   _attr2_type= FG_ELEV;

   if (sp) {
      sp->setLocationOutput(_xloc, _yloc, NULL);
   }

}


FgLocOut::~FgLocOut()
{
 SeisPlot *sp;
 for(sp= top(); (sp); sp= next() )  {
           removeControl(sp);
 }
 delete _attr1_pop;
 delete _attr2_pop;
}

void FgLocOut::addPopButtons(SLPullPop *pop, int curr_val)
{
   pop->setComplexNotify(this);
   pop->addRadio( "Distance from Prev flag", FG_DIST);
   pop->addRadio( "X Location",              FG_XLOC);
   pop->addRadio( "Y Location",              FG_YLOC);
   pop->addRadio( "Elevation",               FG_ELEV);
   pop->addRadio( "X Grid",                  FG_XGRID);
   pop->addRadio( "Y Grid",                  FG_YGRID);
   pop->addRadio( "Hole Depth",              FG_HD);
   pop->addRadio( "Time up Hole",            FG_TUH);
   pop->addRadio( "Source Static",           FG_SSTAT);
   pop->addRadio( "Receiver Static",         FG_RSTAT);
   pop->addRadio( "Inline Src. Skid",        IL_SRC_SKID);
   pop->addRadio( "Crossline Src. Skid",     XL_SRC_SKID);
   pop->addRadio( "Elevation Skid",          FG_ESKID); 
   pop->addRadio( "Cumulative Horiz Dist",   FG_CUM);
   pop->addRadio( "Azimuth",                 FG_AZIM);
   pop->addRadio( "Skid Type",               FG_SKID_TYPE);
   pop->addRadio( "Source Group Number",     SHOT_GRP_NO);
   pop->setRadioValue(curr_val);
}



void FgLocOut::addControl(SeisPlot *sp, FgXpPlotLinkedList *plot_list)
{
   addSeisPlot(sp, (void*)plot_list );
   sp->setLocationOutput(_xloc, _yloc, NULL);
}

void FgLocOut::removeControl(SeisPlot *sp)
{
   delSeisPlot(sp);
   sp->removeLocationOutput();
}




void FgLocOut::postZoomSeparateWindow(SeisPlot *, SeisPlot *zoomsp)
{
   addControl(zoomsp, NULL);  
}


void FgLocOut::mouseOutputUpdate(SeisPlot* sp, float x, float y)
{
  FgXpPlotLinkedList *plot_list= NULL;
  if (find(sp)) {
       plot_list= (FgXpPlotLinkedList*)currentUserData();
  }
  outputInfo(sp, plot_list, (int)x, (int)y);
}


void FgLocOut::blankIt()
{
  wprocShowMsg(_line, "Line:");
  wprocShowMsg(_flag, "Flag:");
  blankAttribute(_attr1_type, _attr1);
  blankAttribute(_attr2_type, _attr2);
}



void FgLocOut::outputInfo(SeisPlot           *sp, 
                          FgXpPlotLinkedList *plot_list,
                          int                x,
                          int                y )
{
 Boolean data_found= False;
 long line_index= 0;
 long flag_index= 0;
 long shot_index= 0;
 FgMapToFlag::SkidType skid_type= FgMapToFlag::NoSkid;


 if ( (x==-1)&&(y==-1) )  {
      data_found= False;
 } // end if
 else if ( (plot_list) && (sp) ) {
     data_found= outputByVectors(sp, plot_list,x,y, &line_index, &flag_index, 
                                                  &shot_index, &skid_type);

 } // end else if
 else if ( (_xlat) && (sp) ) {
     data_found= outputByXlat(sp, x,y, &line_index, &flag_index,
                                     &shot_index, &skid_type);
 } // end else if
 else {
     data_found= outputByOther(sp, x,y, &line_index, &flag_index,
                                     &shot_index, &skid_type);
 } // end else if


 if (data_found) outputToFields(line_index, flag_index, skid_type, shot_index);
 else            blankIt();

}

/*
 *  Use the translator class to get the line & flag index from the X & Y
 */
Boolean FgLocOut::outputByXlat(SeisPlot              *sp, 
                               int                    x,
                               int                    y,
                               long                  *line_index,
                               long                  *flag_index,
                               long                  *shot_index,
                               FgMapToFlag::SkidType *skid_type)
{
  Boolean close_enough;
  close_enough= _xlat->translate(sp, x, y, line_index, flag_index, 
                                 skid_type, shot_index, _trans_type_grid,
                                 _select_lines_only);
  return close_enough;
}

/*
 *  Use the plotted vectors to get the line & flag index from the X & Y
 */
Boolean FgLocOut::outputByVectors(SeisPlot              *sp, 
                                  FgXpPlotLinkedList    *plot_list,
                                  int                    x,
                                  int                    y,
                                  long                  *line_index,
                                  long                  *flag_index,
                                  long                  *shot_index,
                                  FgMapToFlag::SkidType *skid_type)
{

  Vector *vect;
  FgXpData *xpdata;
  int i, numsrcs;
  long *srcary= NULL;
  Boolean data_found= False;
 
  vect= plot_list->getClosest(x,y,sp);
  if (vect) {
     if ( vect->howClose(x,y,sp) < 6 ) {
           i= vect->closestIndex(x,y,sp);
           xpdata= (FgXpData*)vect->getData();   // yes, a down cast
           *flag_index= xpdata->translateDataIndexToFlag(i,&srcary,&numsrcs);
           *line_index= xpdata->getXIndex();
           data_found= True;
           if (numsrcs > 0) {
                 *shot_index= srcary[0];
                 free(srcary);
                 if (_fg->sourceIsSkidded(*line_index,*flag_index, *shot_index))
                         *skid_type= FgMapToFlag::ShotSkid;
           }
           else  {
                 if (_fg->flagHasReceiver(*line_index,*flag_index))
                        if (_fg->receiverIsSkidded(*line_index,*flag_index))
                                  *skid_type= FgMapToFlag::RecSkid;
                 *shot_index= -1;
           }
     } // end if howClose
     else {
           data_found= False; //write blanks to the output field
     }
  }  // end if vect
  else {
     data_found= False;
  }
  return data_found;
}

Boolean FgLocOut::outputByOther(SeisPlot*, 
                                int      ,
                                int      ,
                                long    *,
                                long    *,
                                long    *,
                                FgMapToFlag::SkidType *)
{
  return False;
}


void FgLocOut::outputToFields(long                  line_index, 
                              long                  flag_index, 
                              FgMapToFlag::SkidType skid_type,
                              long                  shot_index)
{
  long line_no;
  float flag_no;
  line_no= _fg->getLineNumber( line_index);
  flag_no= _fg->getShotpoint( line_index, flag_index);
  wprocVAShowMsg(_line, "Line: %d", line_no);
  wprocVAShowMsg(_flag, "Flag: %5.3f", flag_no);
  showAttribute(_attr1_type, line_index, flag_index, 
                skid_type, shot_index, _attr1);
  showAttribute(_attr2_type, line_index, flag_index, 
                skid_type, shot_index, _attr2);
}




void FgLocOut::blankAttribute(int type, Widget w)
{
  switch (type) {
          case FG_DIST:      wprocShowMsg(w, "Dist:");     break;
          case FG_XLOC:      wprocShowMsg(w, "Xloc:");     break;
          case FG_YLOC:      wprocShowMsg(w, "Yloc:");     break;
          case FG_ELEV:      wprocShowMsg(w, "Elev:");     break;
          case FG_XGRID:     wprocShowMsg(w, "XGrid:");    break;
          case FG_YGRID:     wprocShowMsg(w, "YGrid:");    break;
          case FG_HD:        wprocShowMsg(w, "HD:");       break;
          case FG_TUH:       wprocShowMsg(w, "TUH:");      break;
          case FG_RSTAT:     wprocShowMsg(w, "Rstat:");    break;
          case FG_SSTAT:     wprocShowMsg(w, "Sstat:");    break;
          case IL_SRC_SKID:  wprocShowMsg(w, "ILS:");   break;
          case XL_SRC_SKID:  wprocShowMsg(w, "XLS:");   break;
          case FG_ESKID:     wprocShowMsg(w, "Eskid:");    break;
          case FG_CUM:       wprocShowMsg(w, "Cum Dist:"); break;
          case FG_AZIM:      wprocShowMsg(w, "Azim:");     break;
          case FG_SKID_TYPE: wprocShowMsg(w, "");          break;
          case SHOT_GRP_NO:  wprocShowMsg(w, "SGrp:");     break;
          case SP_Z_OUTPUT:  break;
          case APP_CONTROL:  wprocShowMsg(w, "");          break;
          default: assert(0); break;

  } // end switch
}

void FgLocOut::showAttribute(int type, 
                             long ixl, 
                             long ixf, 
                             FgMapToFlag::SkidType skid_type,
                             long shot_index,
                             Widget w)
{
  float fval, dummy;
  double dval;

  switch (type) {
          case FG_DIST: 
                        dval= _fg->getIncrDistance(ixl, ixf);
                        wprocVAShowMsg(w, "Dist: %5.3f", dval);
                        break;
          case FG_XLOC: 
                        dval= _fg->getXloc(ixl, ixf);
                        wprocVAShowMsg(w, "Xloc: %5.3f", dval);
                        break;
          case FG_YLOC: 
                        dval= _fg->getYloc(ixl, ixf);
                        wprocVAShowMsg(w, "Yloc: %5.3f", dval);
                        break;
          case FG_ELEV: 
                        fval= _fg->getElevation( ixl, ixf); 
                        wprocVAShowMsg(w, "Elev: %5.3f", fval);
                        break;
          case FG_XGRID: 
                        dval= _fg->getXgrid(ixl, ixf);
                        wprocVAShowMsg(w, "XGrid: %5.3f", dval);
                        break;
          case FG_YGRID: 
                        dval= _fg->getYgrid(ixl, ixf);
                        wprocVAShowMsg(w, "YGrid: %5.3f", dval);
                        break;
          case FG_HD: 
                        fval= _fg->getHoleDepth( ixl, ixf); 
                        wprocVAShowMsg(w, "HD: %5.3f", fval);
                        break;
          case FG_TUH: 
                        fval= _fg->getUpholeTime( ixl, ixf); 
                        wprocVAShowMsg(w, "TUH: %5.3f", fval);
                        break;
          case FG_RSTAT: 
                        fval= _fg->getReceiverStatic( ixl, ixf); 
                        wprocVAShowMsg(w, "Rstat: %5.3f", fval);
                        break;
          case FG_SSTAT: 
                        fval= _fg->getSourceStatic( ixl, ixf); 
                        wprocVAShowMsg(w, "Sstat: %5.3f", fval);
                        break;
          case IL_SRC_SKID: 
                        if (!_fg->sourceGathersOutOfDate() &&
                            skid_type ==FgMapToFlag::ShotSkid) {
                               _fg->getSourceSkids( ixl, ixf, shot_index,
                                                    &fval, &dummy); 
                               wprocVAShowMsg(w, "ILS: %4.2f", fval);
                        } // end if
                        else
                               wprocShowMsg(w, "ILS: NONE");
                        break;
          case XL_SRC_SKID: 
                        if (!_fg->sourceGathersOutOfDate() &&
                            skid_type ==FgMapToFlag::ShotSkid) {
                               _fg->getSourceSkids( ixl, ixf, shot_index,
                                                    &dummy, &fval); 
                               wprocVAShowMsg(w, "XLS: %4.2f", fval);
                        } // end if
                        else
                               wprocShowMsg(w, "XLS: NONE");
                        break;
          case FG_ESKID: 
                        fval= _fg->getReceiverEskid( ixl, ixf); 
                        wprocVAShowMsg(w, "Eskid: %5.3f", fval);
                        break;
          case FG_CUM:
                        dval= _fg->getCumDistance( ixl, ixf); 
                        wprocVAShowMsg(w, "Cum Dist: %5.3f", dval);
                        break;
          case FG_AZIM:
                        dval= _fg->getAzimuth( ixl, ixf); 
                        wprocVAShowMsg(w, "Azim: %5.3f", dval);
                        break;
          case FG_SKID_TYPE: 
                            if (skid_type == FgMapToFlag::NoSkid)
                                   wprocShowMsg(w, "Flag");
                            else if (skid_type ==FgMapToFlag::RecSkid)
                                   wprocShowMsg(w, "Rec. Skid");
                            else if (skid_type ==FgMapToFlag::ShotSkid)
                                   wprocShowMsg(w, "Shot Skid");
                            break;
          case SHOT_GRP_NO: 
                            if (shot_index > -1) {
                                   wprocVAShowMsg(w, "SGrp: %d", 
                                                 _fg->sourceGroupNumber(
                                                 ixl, ixf, shot_index) );
                            }
                            else
                                   wprocShowMsg(w, "SGrp: NONE");
                            break;
          case SP_Z_OUTPUT: break;
          case APP_CONTROL: break;
          default:          assert(0); break;
  } // end switch
}

Boolean FgLocOut::notifyComplex(SLDelay *obj, int ident)
{
  if (obj == _attr1_pop) {
        if (ident == SP_Z_OUTPUT) {
            for(SeisPlot* sp= top(); (sp); sp= next() )  {
                 sp->setLocationOutputType(PlotImage::MOUSE_VEL, "", 0.0);
                 sp->setLocationOutput(_xloc, _yloc, _attr1);
            }
            wprocShowMsg(_attr1, "");
        }
        else {
            for(SeisPlot* sp= top(); (sp); sp= next() )  {
                 sp->setLocationOutput(_xloc, _yloc, NULL);
            }
        }
        _previous_attr1_type= _attr1_type;
        _attr1_type= ident;
        blankAttribute(_attr1_type, _attr1);
  }
  else if (obj == _attr2_pop) {
        _previous_attr2_type= _attr2_type;
        _attr2_type= ident;
        blankAttribute(_attr2_type, _attr2);
  }
  return True;
}


void FgLocOut::setAttr1(int attr)
{
  notifyComplex(_attr1_pop, attr);
  _attr1_pop->setRadioValue(attr);
}

void FgLocOut::setAttr2(int attr)
{
  notifyComplex(_attr2_pop, attr);
  _attr2_pop->setRadioValue(attr);
}

int FgLocOut::attr1() { return _attr1_type; }
int FgLocOut::attr2() { return _attr2_type; }



void FgLocOut::resetLastAttr1()
{
 setAttr1(_previous_attr1_type);
}

void FgLocOut::resetLastAttr2()
{
 setAttr2(_previous_attr2_type);
}

void FgLocOut::setAttr1Field(char *str)
{
  wprocShowMsg(_attr1, str);
}
void FgLocOut::setAttr2Field(char *str)
{
  wprocShowMsg(_attr2, str);
}

void FgLocOut::drawGrid(SeisPlot *sp, Boolean by_grid)
{
  _xlat->drawGrid(sp,by_grid);
}

void FgLocOut::setSelectedLinesOnly(Boolean s) 
{
  _select_lines_only= s;
}
