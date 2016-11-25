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
#ifndef SEISCTYPE_H
#define SEISCTYPE_H

#include "sl/sl_form.hh"
#include "sp/seis_cbar.hh"

class Cscale;
class SeisPlot;
class SLScale;
class SLRadioBox;
class SeisColorFileIO;
class SeisCBBPop;
class SLPushBox;

class SeisCtype : public SLForm {

  private:
      static void cfilecb (void *data, long ident, char *oldvar, char *newvar);
      static void doCtog  (SeisCtype*, long);
      Boolean okToReadColorFile ();

  protected:
      void unmade_filesetup();

      SeisCbar        *_cbar;
      class SLpFile   *_cfileo;
      SLScale         *_cnum;
      SLRadioBox      *_c_choice;
      SeisPlot        *_sp;
      char             _cfile[200];
      Boolean          _new_color;
      SeisColorFileIO *_fio;
      SeisCBBPop      *_cbldr;
      SLPushBox       *_but;

      char          *_cfile_failstr;
      Boolean        _cfile_valid;
      Boolean        _first_time;

  public:
      enum {CBAR,USEDEF};
      friend class Cscale;
      SeisCtype( Widget   p,
                 char     *name,
                 HelpCtx  hctx,
                 SeisPlot *sp,
                 SeisCbar *cbar,
                 Boolean  make_now  =False);

      SeisCtype( SLDelay  *contain,
                 char     *name,
                 HelpCtx  hctx,
                 SeisPlot *sp,
                 SeisCbar *cbar,
                 Boolean  make_if_can  =False);

      ~SeisCtype ();

       void init();


       virtual Widget make(Widget p);
       //virtual void reloadDefaults(Boolean do_method= True);
       //virtual void reloadSystemDefaults(Boolean do_method =True);
       void setColorType(int which);
       void setFileName(char *fname); 
       virtual void newColorBar();
       char *getFileName();
       void setPredef(int which);
       int  getPredef();
       virtual void typeChange(int /*which*/) {};
       int  whichSelected();
       Widget getColorScaleWidget();
       void pushAction (long ident);
       Boolean notifyComplex (SLDelay *obj, int ident);
       long readColorFile (char *filename);
       SLRadioBox *getChoiceBox(){return _c_choice;}

};
#endif
