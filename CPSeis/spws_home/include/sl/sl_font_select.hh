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
#ifndef SLFONTSELECT_HH
#define SLFONTSELECT_HH

#include "sl/sl_form.hh"

class SLOptionMenu;


class SLFontSelect : public SLForm {
  private:
     //void addPt( const int pt);
     void addFam( const char *fam);
     static void ptChange(void*,long);
     static void famChange(void*,long);
     void scalablePtSizes();
     void changePtList();
     void init(const Display *dpy);

  protected:
     enum {PT_MAX=100};
     char         *_selected_font; 
     SLOptionMenu *_pt_option;
     SLOptionMenu *_fam_option;
     int          _pt_list[PT_MAX];
     int          _curr_pt_list[PT_MAX];
     char         _fam_list[100][80];
     int          _pt_cnt;
     int          _curr_pt_cnt;
     int          _fam_cnt;
     Widget       _famlab;
     Widget       _ptlab;


  public:
     SLFontSelect(const Widget  p,
                  const char    *name,
                  const HelpCtx hctx,
                  const Boolean doframe =False,
                  const Boolean make_now=True );

     SLFontSelect(SLDelay *contain,
                  const char    *name,
                  const Boolean doframe     =False,
                  const Boolean make_if_can =True );
     ~SLFontSelect();
     virtual Widget make(Widget p =NULL);
     void setFontPattern(char *fontstr);
     int  pointSize() const;
     const char *familyName() const;
     char *selectedFont() const;
};



#endif
