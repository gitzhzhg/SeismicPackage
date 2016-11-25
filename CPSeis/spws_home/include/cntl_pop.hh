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
#include "sl/sl_form_pop.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_prim.hh"
#include "sl/sl_scale.hh"
#include "sl/psuedo_widget.hh"
#include "va.h"
#include "wproc.h"
#include "image.h"
#include <Xm/Xm.h>
class CntlPop;


class SpecificScale : public SLScale {

  protected:
             virtual void ScaleAction(int value);
             CntlPop      *_cntlpop;
  public:
          SpecificScale ( PsuedoWidget    *pw,
                          char      *name,
                          HelpCtx   hctx,
                          int        *valptr,
                          CntlPop   *cntlpop) :
             SLScale( pw, name, hctx, valptr ), _cntlpop(cntlpop) {};
};

class CntlTog : public SLTogBox {

  protected:
             CntlPop      *_cntlpop;
  public:
             CntlTog( PsuedoWidget     *pw,
                      char       *name,
                      HelpCtx    hctx,
                      SLTogAry   togary,
                      unsigned int arycnt,
                      Boolean    dotitle,
                      CntlPop    *cntlpop ) :
          SLTogBox(pw,name,hctx,togary,arycnt,True, dotitle), 
          _cntlpop(cntlpop) {};
};

class OverRadio : public SLRadioBox {

  protected:
             virtual void ChoiceAction( long button);
             CntlPop      *_cntlpop;
  public:
             OverRadio( PsuedoWidget       *pw,
                        char         *name,
                        HelpCtx      hctx,
                        SLRadioAry   togary,
                        unsigned int arycnt,
                        CntlPop      *cntlpop) :
               SLRadioBox(pw,name,hctx,togary,arycnt,NULL,True) ,
                  _cntlpop(cntlpop){};
};

class ActionTog : public CntlTog  {
  protected:
             virtual void TogAction( long ident, Boolean set );

  public:
             ActionTog( PsuedoWidget     *pw,
                        char       *name,
                        HelpCtx    hctx,
                        SLTogAry   togary,
                        unsigned int arycnt,
                        Boolean    dotitle,
                        CntlPop    *cntlpop ) :
              CntlTog(pw,name,hctx,togary,arycnt,dotitle,cntlpop) {};
};

class OverTog : public CntlTog  {
  protected:
             virtual void TogAction( long ident, Boolean set );

  public:
             OverTog(   PsuedoWidget     *pw,
                        char       *name,
                        HelpCtx    hctx,
                        SLTogAry   togary,
                        unsigned int arycnt,
                        Boolean    dotitle,
                        CntlPop    *cntlpop ) :
              CntlTog(pw,name,hctx,togary,arycnt,dotitle,cntlpop) {};
};


class DoppText : public SLTextBox {

  protected:
             virtual void TextAction( long ident);
             CntlPop      *_cntlpop;
  public:
           DoppText( PsuedoWidget     *pw,
                     char             *name,
                     HelpCtx          hctx,
                     SLTextAry        textary,
                     unsigned int     arycnt,
                     CntlPop          *cntlpop ) :
          SLTextBox(pw,name,hctx,textary,arycnt,True,1,True,False), 
           _cntlpop(cntlpop) {};
};


//------------------------------------------
//----------------------------- CntlPop
//------------------------------------------
class CntlPop :  public SLFPopSep {

   private:
       static void pushAction( void *info, long wconst);
       static void textAction( void *obj,  long wconst);
   protected:
       void push(long wconst);
       SLTogBox      *_actions;
       SLTogBox      *_which_over;
       SLRadioBox    *_over_type;
       SLTextBox     *_nmc_opts;
       SLScale       *_spec_scale;
       SLPushBox     *_action_push;
       SLPushBox     *_over_push;
       Widget        _vel_id;
       float         _doppval;
       float         _old_doppval;
       VelStruct     *_vel;
       Boolean       _hidepicks;

   public:
       CntlPop( Widget            p,
                char              *name,
                VelStruct         *vel);
       virtual void manage();
       virtual Widget make(Widget p);
       void    numFunctions(long n);
       void    updateCntl();
       void    zoom( Widget w, VelStruct *_vel);
       void    applynmo(VelStruct *_vel);
       void    removenmo(VelStruct *_vel);
       void    refreshvel(VelStruct *_vel);
       void    hideshow(VelStruct *_vel);
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);
       friend  DoppText;
       friend  OverTog;
       friend  ActionTog;
       friend  OverRadio;
       friend  SpecificScale;
};
