#include <stdio.h>
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
#include "fgmap/fg_seis_plot.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "fgmap/fg_plot_color_pop.hh"
#include "sl/sl_pull_pop.hh"
#include "sp/seis_zoomop_pop.hh"
#include "sp/seis_grid_pop.hh"
#include "sp/seis_zoomop_pop.hh"
#include "geom/fg_inform.hh"
#include "geom/fg_constants.hh"
#include "geom/field_geometry.hh"
#include "fggui/fg_control_pop.hh"
#include "fggui/transform_pop.hh"
#include "fggui/fg_status_gui.hh"
#include "hardcopy/hardcopy_pop.hh"
#include "sl/slp_file_data.hh"
#include "vu/seis_label.hh"



class LockInformer : public FgInform {
     private:
         FgSeisPlot *_fgsp;
     public:
         LockInformer(FieldGeometry *fg, FgSeisPlot *fgsp) :
                 FgInform(fg), _fgsp(fgsp) {}
         virtual void postChangeDataLock(FieldGeometry *fg);
};


enum { ZUP=44545, ZSEP, ZDOWN, ZORGIN, ZOP, ADDLABEL, DELALLLAB, UNLOCK };


FgSeisPlot::FgSeisPlot(Widget          p,       
                       char           *name,
                       FgSeisPlotList *list,
                       Boolean         add_grid_pop,
                       Boolean         backing_store_sugested) :
                  SLForm(p, name, list->helpCtx()), SeisInform(), SeisPlot(),
                  _grid_pop(NULL), _list(list),
                  _first_plot(True), _xp_list(NULL),
                  _suggest_backing_store(backing_store_sugested)
{
  FgControlPop *dcp;
  TransformPop *tp;
  constructor(topWidget(), "sp");
  addSeisPlot(this);
  assert(list);
  
  _lock_informer= new LockInformer(list->fieldGeometry(), this);
  _status= new FgStatusGui(this, "toms_stat", _list->fieldGeometry(), 
                           list->getAllPops(), NULL, True);
  showMessageArea();
  _popup= new SLPullPop("popup", SeisPlot::W(), list->helpCtx());
  _label= new SeisLabel(this, list->helpCtx());
  if (add_grid_pop) {
     _grid_pop= new SeisGridPop(topWidget(), "gridpop", this, list->helpCtx());
  }

  _hard_data = new SLpFileData ("hardcopy_file", (long)0, "Plot File:",
				"Plot File", "cgm", SLpFile::_OUTPUT);
  _hard_pop= new HardCopyPop(topWidget(), "hardpop", list->helpCtx(), 
			     this, _hard_data, HardCopyPop::SYMETRICAL, False,
			     1.0);

  _popup->setComplexNotify(this);
  _popup->addPush( "Zoom Up",                 ZUP);
  _popup->addPush( "Zoom Up Separate Window", ZSEP);
  _popup->addPush( "Zoom Down",               ZDOWN);
  _popup->addPush( "Original Size",           ZORGIN);
  _popup->addPushUp("Zoom Options...",        _list->zoomOpPop());
  _popup->addSep();
  _popup->addPush("Add A Label",    ADDLABEL);
  _popup->addPush("Delete All Lables", DELALLLAB);
  _popup->addSep();
  _popup->addPush("Unlock All Data",  UNLOCK);
  _popup->addSep();
  if (add_grid_pop)  {
      _popup->addPushUp("Grid Setup...",      _grid_pop);
  }
  _popup->addPushUp("Hard Copy Plot...",     _hard_pop);
  dcp= _list->getDCP(); 
  tp= _list->getTP(); 
  if (dcp) _popup->addPushUp("Data control panel...",  dcp);
  if (tp)  _popup->addPushUp("Grid transformation...", tp);
  _popup->addPushUp("Line/Flag Color Options...",  _list->getPlotColorPop());

  setGridColor(_list->plotBgPixel());

  _list->add(this);

  if (_list->fieldGeometry()->getDataLock() != LOCK_NONE) {
        _popup->sensitive(True, UNLOCK, -1);
  } // end if
  else {
        _popup->sensitive(False, UNLOCK, -1);
  } // end if
  setTimingLines( (double)1.0, (double)1.0);

}


FgSeisPlot::~FgSeisPlot()
{
 _list->remove(this);
 delete _status;
 delete _popup;
 delete _label;
 delete _lock_informer; 
 delete _hard_pop;
 delete _hard_data;
 if (_grid_pop) delete _grid_pop;
}



SeisPlot& FgSeisPlot::operator=(SeisPlot& sp)
{
 SeisPlot::operator=(sp);
 return *this;
}


Boolean FgSeisPlot::notifyComplex(SLDelay *obj, int ident)
{
  if (obj == _popup) {
    switch (ident) {
      case ZUP:       zoomUp();            break;
      case ZSEP:      zoomUpSeparateWin(); break;
      case ZDOWN:     zoomDown();          break;
      case ZORGIN:    originalSize();      break;
      case ADDLABEL:  _label->insertMoveOneLabel();  break;
      case DELALLLAB: _label->deleteAllLabels();    break;
      case UNLOCK:    _list->fieldGeometry()->setDataLock(LOCK_NONE);  break;
    } // end switch
  } // end if
  return True;
}

void FgSeisPlot::prePlot(SeisPlot *)
{
  if (_first_plot) {
    switch (_list->backingStoreMode()) {
          case FgSeisPlotList::ALWAYS:  backingStore(True); 
                                        break;
          case FgSeisPlotList::NEVER:   backingStore(False); 
                                        break;
          case FgSeisPlotList::FOLLOW_SUGGESTION:  
                                        if (_suggest_backing_store)
                                                backingStore(True); 
                                        else 
                                                backingStore(False); 
                                        break;
          default:                      assert(0);
                                        break;
    } // end switch
    _first_plot= False;
  }
}


void FgSeisPlot::showMessageArea()
{
  _status->manage();
  XtVaSetValues(_status->W(),  XmNleftAttachment,   XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNtopAttachment,    XmATTACH_FORM,
                               NULL);
  XtVaSetValues(SeisPlot::W(), XmNleftAttachment,   XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _status->W(),
                               XmNbottomAttachment, XmATTACH_FORM,
                               NULL);
}

void FgSeisPlot::hideMessageArea()
{
  _status->unmanage();
  XtVaSetValues(SeisPlot::W(), XmNleftAttachment,   XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNtopAttachment,    XmATTACH_FORM,
                               XmNbottomAttachment, XmATTACH_FORM,
                               NULL);
}

void LockInformer::postChangeDataLock(FieldGeometry *fg)
{
  if (fg->getDataLock() != LOCK_NONE) {
        _fgsp->_popup->sensitive(True, UNLOCK, -1);
  } // end if
  else {
        _fgsp->_popup->sensitive(False, UNLOCK, -1);
  } // end if

}
