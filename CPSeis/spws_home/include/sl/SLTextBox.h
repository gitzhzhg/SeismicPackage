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
#ifndef SLTEXTBOX_H
#define SLTEXTBOX_H

#include "sl/sl_delay.hh"
#include "wproc.h"
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>

typedef union {
       unsigned char cval;
       short         sval;
       int           ival;
       float         fval;
       long          lval;
       double        dval;
     } Vtype;



typedef struct _SLText {
                            char   *name;
                            char   *range;
                            void   *target;
                            long   type;
                            int    ident;
                           } SLText, *SLTextAry;


typedef struct _SLTextW {
                            Widget tw;
                            Widget lw;
                            char   *name;
                            char   *range;
                            void   *target;
                            long   type;
                            int    ident;
                            Boolean value_set;
                            Vtype  orgin_value;  // not used
                            Vtype  value;        // not used
                           } SLTextW, *SLTextAryW;

typedef void (*SLTextfunc)(void*,long);

// must match wproc.h
#define SLType_int      1
#define SLType_float    2
#define SLType_sint     6
#define SLType_gfloat   9
#define SLType_uchar    10
#define SLType_long     11
#define SLType_double   12
#define SLType_gdouble  13


class SLTextBox : public SLDelay {
      public:
          enum TextReason { TextNone, GainingFocus, LosingFocus, Activate };
      private:
          static void okCallback(Widget, XtPointer, XtPointer);
          static void DoTextCallback(Widget, XtPointer,
                                     XmTextVerifyCallbackStruct*);
          static void highlightHandler(Widget w, XtPointer CBdata, 
                                       XEvent *event);
      protected:
          void init( const Display *dpy, SLTextAry textary);
          void get_targets(Boolean =False);
          virtual void DoText(Widget, XtPointer, XmTextVerifyCallbackStruct*);
          virtual void DoTextFocus(Widget, XtPointer, XmAnyCallbackStruct*);
          void doHighlight(Widget w, XtPointer CBdata, XEvent *event);
          virtual void TextActionFocus( long ) {};
          virtual void TextAction( long ) {};
          void    sendHighlight(long i);
          /*
           *  work routines
           */
          int   retidx( int  ident);
          int   retidx( char *name);
          void   wkSetSensitive( int  ident, Boolean set);
          void   wkSetValue( int i, double);
          void   wkSetValue( int i, long);
          long   wkGetInt( int i);
          double wkGetFloat( int i);

          void wkSetRange (int ident, String range);
          void wkSetRange (int i, void *low, void *high, void *def,
			   long precision = same);

          SLTextAryW    _textary;
          unsigned int  _arycnt;
          int           _curr_text;
          Widget        _popbox;
          long          _last_valid;
          SLTextfunc    _altFocusAction;
          void          *_altFocusData;
          SLTextfunc    _altLosingAction;
          void          *_altLosingData;
          Boolean       _DoLabel;
          Boolean       _DoTitles;
          long          _number_col;
          int           _last_idx;
          TextReason    _last_reason;
          Dimension     *_external_width;



      public:
          enum {
#ifdef IA64
            same = -999999999
#else
            same = -999999
#endif
	  };

          SLTextBox( Widget     p,
                     char       *name,
                     HelpCtx    hctx,
                     SLTextAry  textary,
                     unsigned int arycnt,
                     Boolean    DoLabel    =True,
                     long       number_col =1,
                     Boolean    doframe    =False,
                     Boolean    DoTitles   =False,
                     Boolean    make_now   =True);

          SLTextBox( SLDelay    *contain,
                     char       *name,
                     HelpCtx    hctx,
                     SLTextAry  textary,
                     unsigned int arycnt,
                     Boolean    DoLabel    =True,
                     long       number_col =1,
                     Boolean    doframe    =False,
                     Boolean    DoTitles   =False,
                     Boolean    make_if_can=True);

          SLTextBox( PsuedoWidget *pw,
                     char          *name,
                     HelpCtx       hctx,
                     SLTextAry     textary,
                     unsigned int  arycnt,
                     Boolean       DoLabel    =True,
                     long          number_col =1,
                     Boolean       doframe    =False,
                     Boolean       DoTitles   =False);

          ~SLTextBox();
          Boolean validate();
          void    load();
          void    popError(char *);
          void setAltFocusAction( SLTextfunc action, void *data)
                                        { _altFocusAction= action;
                                          _altFocusData  = data; };
          void setAltLosingAction( SLTextfunc action, void *data)
                                        { _altLosingAction= action;
                                          _altLosingData  = data; };
          TextReason lastNotifyReason();

          void setWidth (Dimension external_width);
          


          /*
           *  required virtual functions
           */
          virtual Widget make(Widget p =NULL);
          virtual WidgetClass topClass() { return(xmRowColumnWidgetClass); };
          virtual void reloadDefaults(Boolean do_method= True);

          /*
           *  this is the interface using the ident style
           */
          void   clear( int  ident);
          void   highlight(int  ident);
          void   SetValue( int  ident, double);
          void   SetValue( int  ident, float);
          void   SetValue( int  ident, int);
          void   SetValue( int  ident, long);
          long   GetInt( int  ident);
          double GetFloat( int  ident);
          Widget TxtW( int  ident );
          Widget LabW( int  ident );
          void   SetSensitive( int  ident, Boolean set);
          void   SetRange( int   ident, String range );
          void   SetRange( int  ident, double low = same, double high = same, 
                           double def = same, long precision = same);
          void   SetRange( int  ident, long low = same, long high = same, 
                           long def = same);
          void   SetRange( int  ident, float low = same, float high = same, 
                           float def = same, int precision = same);

          /*
           * 
           */
          int operator[](int idx);

          void operator=(int value);
          void operator=(float value);
          //friend void operator=(float& value, SLTextBox *obj);
          //friend void operator=(int& value, SLTextBox *obj);



          /*
           *  this is the interface using the name style
           */
          void   clear( char *name);
          void   highlight(char *name);
          void   SetValue( char *name, double);
          void   SetValue( char *name, float);
          void   SetValue( char *name, int);
          void   SetValue( char *name, long);
          long   GetInt( char *name);
          double GetFloat( char *name);
          Widget TxtW( char *name );
          Widget LabW( char *name );
          void   SetSensitive( char *name, Boolean set);
          void   SetRange( char *name, String range );
          void   SetRange( char *name, double low = same, double high = same, 
                           double def = same, long precision = same);
          void   SetRange( char *name, long low = same, long high = same,  
                            long def = same);
          void   SetRange( char *name, float low = same, float high = same, 
                           float def = same, int precision = same);
};
/*
 * ================== For Getting Values ==========================
 */

/*
inline void operator=(float& value, SLTextBox *obj)
                                 { value= GetFloat(obj->_last_idx); }
inline void operator=(int& value, SLTextBox *obj)
                                 { value= GetInt(obj->_last_idx); }
*/

#endif
