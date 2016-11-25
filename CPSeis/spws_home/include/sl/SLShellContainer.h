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
#ifndef SHELLCONTAINER_H 
#define SHELLCONTAINER_H 

#include <Xm/Form.h>
#include "wproc.h"
#include "sl/sl_delay.hh"
#include "sl/container_list.hh"
#include "sl/wlist.hh"
#include "sl/display_list.hh"
#include "sl/delay_list.hh"

#ifdef FP_OK
#undef FP_OK
#endif
#ifdef FP_CANCEL
#undef FP_CANCEL
#endif
#ifdef FP_APPLY
#undef FP_APPLY
#endif
#ifdef FP_HELP
#undef FP_HELP
#endif


#define FP_OK         0
#define FP_CANCEL     1
#define FP_APPLY      2
#define FP_HELP       3
#define FP_REMOVE     4
#define FP_OTHER      5
#define FP_NONE       6


class CharPtrLinkedList;

enum { UseResource=-2, UseParentScreen=-1, MayNotIcon=0, MayIcon=1 };

class SLShellContainer : public SLDelay {

   public:
         // these enums may be used by some someclasses that do some
         // action such as picking
        typedef enum _ActivityStatus { NoActivityStatus,
                                       PopUpNoActivity,
                                       PopUpWithActivity,
                                       PopDownNoActivity,
                                       PopDownEndingActivity,
                                       PopDownWithActivity,
                                       StartingActivity,
                                       EndingActivity   } ActivityStatus;
   private:
        static void windowClosingCallback(Widget,XtPointer,XtPointer);
        void windowClosing(Widget,XtPointer,XtPointer);
        static void raiseDialogEvent(Widget,XtPointer,XEvent *ev);
        enum SortofBoolean { DONT_KNOW=-1, NO_BOOL=0, YES_BOOL=1};

   protected:
        static ContainerList _pop_list; // every child made in under this class
        static DisplayList   _dpy_list; // every display that we are using
        static Wlist         _msg_list; // message widget from application
        static Wlist         _mode_list; // mode widget from application
        int                  _num_test_colors;
        int                  _screen_number;
        Widget               _shell;              
        Colormap             _cmap;
        Boolean              _cmap_installed;
        int                  _may_icon;
        SortofBoolean        _screen_different;
        ActivityStatus       _current_activity_status;

        void setColormap(Colormap cmap) { _cmap= cmap;}
        Colormap colormap()             { return _cmap;}
        Boolean canIAlloc(int number, int num_planes, Widget w);
        int computeScreen(Widget p);
        void addDialogRaiser();

        // these methods may be used by some someclasses that do some
        // action such as picking
        void activityNotify(ActivityStatus status);

   public:
        
        SLShellContainer(const Widget  p, 
                         char         *name, 
                         const HelpCtx hctx            =NULL,
                         const int     num_colors      =0,
                         const int     may_icon        =UseResource,
                         const int     screen_number   =UseResource);
        SLShellContainer(char         *name, 
                         const HelpCtx hctx    = NULL,
                         const int     may_icon= MayNotIcon);
        SLShellContainer(SLDelay       *contain, 
                         char          *name, 
                         const HelpCtx  hctx= NULL, 
                         const int     num_colors      =0,
                         const int     may_icon        =UseResource,
                         const int     screen_number   =UseResource);

        void raiseDialog(Widget shell);

        virtual ~SLShellContainer();
        virtual void    managing();

        // these methods may be used by some someclasses that do some
        // action such as picking
        ActivityStatus getActivityStatus();
        virtual void stopActivity() {}

        void setScreenNumber(int scrno);
        void setMayIcon();
        void setNumberTestColors(int num);
        int  numberTestColors();

        virtual Widget make(Widget p =NULL);
        virtual WidgetClass topClass() { return(xmFormWidgetClass); };
        virtual Boolean isDialog()    { return True; };
        virtual Boolean isContainer() { return True; };
        virtual WidgetClass dialogShellType();

        enum ModalType { PrimAppModal, FullAppModal, Modeless};
        void setModal( ModalType mode);
        
        void setTopWidget(Widget w);
        Boolean isPrivateCmap();
        virtual void closing()= 0;
        void closeAlert(Widget w);
        Widget creDialog(const Widget  p, const WidgetClass wclass);
        void setTitle(char* title, Boolean set_to_icon= True);
        void setIcon(char* title, Pixmap icon_pix= 0, Pixmap icon_mask= 0);


    /* 
     *   ------------------ Static Functions -------------------
     *  Used to operate on all the static linked list that this class 
     *  maintains 
     */
/*todo*/static void addErrorWidget(Widget w);
        static void setClocks(Display *_dpy, Cursor watch);
        static void resetClocks(Display *_dpy);
        static char *setMsgWidget(Widget shell, char *str);
        static void resetMsgWidget(Widget shell, char *str);
        static void setAllMsgWidgets(CharPtrLinkedList *list, char *str);
        static void resetAllMsgWidgets(CharPtrLinkedList *list);
        static char *setModeWidget(Widget shell, char *str);
        static void resetModeWidget(Widget shell, char *str);
        static void refreshModeWidget(Widget shell, char *str);
        static SLShellContainer *anyContainer();
        static Widget getMsgArea(Widget shell);
        static void addMsgArea(Widget w)     {_msg_list.add(w);}
        static void removeMsgArea(Widget w)  {_msg_list.remove(w);}
        static void addModeArea(Widget w)    {_mode_list.add(w);}
        static void removeModeArea(Widget w) {_mode_list.remove(w);}
        static Display *topDpy(void **x =(void **)0){return _dpy_list.top(x);}
        static Display *nextDpy(void **x=(void **)0){return _dpy_list.next(x);}
        static HelpCtx getShellHelpCtx(Widget shell);
/*
        static void SLShellContainer::iconifyAll();
*/
        static void iconifyAll();


};

#endif
