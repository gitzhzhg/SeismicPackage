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
/*
 * Widget Name : DumbManager
 * File        : DumbManage.c
 * Author      : Trey Roby
 * Date        : 12/1/92
 *
 *      This widget is a manager that will allow any child resize request
 *      without resizing itself.
 *
 * CHANGES:
 */

 
#include <X11/IntrinsicP.h> 
#include <X11/StringDefs.h>
#include <Xm/XmP.h>
#include <Xm/BulletinBP.h>
#include "dumb_manag_p.h"
#include "wproc.h"

#include <stdio.h>
#include <string.h>

/*
 * declare some constants
 */

#define DM_DEBUG 0



/*
 * ======= DECLARATIONS - Methods, Private & Public FUNCTIONS SECTION =======
 */

/* 
 * Declaration of methods
 */
static void Resize();
static void ChangeManaged();
/* static XtGeometryResult QueryGeometry(); */   /* Not Used */
static XtGeometryResult GeometryManager();



/* 
 * Declaration of private functions 
 */
static void dolayout( Widget w);

/*
 *=======================  CLASS REC INIT SECTION =============================
 */

#define DUMB_CLASS_REC { \
    { \
    /* core_class fields */ \
        /* superclass                   */ (WidgetClass) &xmManagerClassRec, \
        /* class_name                   */ "DumbManager", \
        /* widget_size                  */ sizeof(DumbManagerRec), \
        /* class_initialize             */ NULL, \
        /* class_part_initialize        */ NULL, \
        /* class_inited                 */ False, \
        /* initialize                   */ NULL, \
        /* initialize_hook              */ NULL, \
        /* realize                      */ XtInheritRealize, \
        /* actions                      */ NULL,   /* actions */ \
        /* num_actions                  */ 0,       /* XtNumber(actions), */ \
        /* resources                    */ NULL, \
        /* num_resources                */ 0, \
        /* xrm_class                    */ NULLQUARK, \
        /* compress_motion              */ True, \
        /* compress_exposure            */ XtExposeCompressMultiple, \
        /* compress_enterleave          */ True, \
        /* visible_interest             */ False, \
        /* destroy                      */ NULL, \
        /* resize                       */ Resize, \
        /* expose                       */ NULL,       /* Redisplay, */ \
        /* set_values                   */ NULL, \
        /* set_values_hook              */ NULL, \
        /* set_values_almost            */ XtInheritSetValuesAlmost, \
        /* get_values_hook              */ NULL, \
        /* accept_focus                 */ NULL, \
        /* version                      */ XtVersion, \
        /* callback_private             */ NULL, \
        /* tm_table                     */ NULL,  /* defaultTranslations, */ \
        /* query_geometry               */ NULL, \
        /* display_accelerator          */ NULL, \
        /* extension                    */ NULL \
    }, \
 \
    { \
    /* composite_class fields */ \
        /* geometry_manager             */ GeometryManager, \
        /* change_managed               */ ChangeManaged, \
        /* insert_child                 */ XtInheritInsertChild, \
        /* delete_child                 */ XtInheritDeleteChild, \
        /* extension                    */ NULL, \
    }, \
    { \
    /* constraint_class fields */ \
        /* resources                    */ NULL, \
        /* num_resources                */ 0, \
        /* constraint_size              */ 0, \
        /* initialize                   */ NULL, \
        /* destroy                      */ NULL, \
        /* set_values                   */ NULL, \
        /* extension                    */ NULL \
    }, \
    { \
    /* manager_class fields */ \
        /* translations                 */  XtInheritTranslations, \
        /* syn_resources                */  NULL, \
        /* num_syn_resources            */  0, \
        /* syn_constraint_resources     */  NULL, \
        /* num_syn_constraint_resources */  0, \
        /* parent_process               */  XmInheritParentProcess, \
        /* extension                    */  NULL \
    }, \
   { \
    /* DumbManager class fields */ \
        /* make_compiler_happy          */ 1 \
   } \
}

/*
 * declare widget class
 */
#ifndef VMS
DumbManagerClassRec dumbManagerClassRec = DUMB_CLASS_REC;
WidgetClass dumbManagerWidgetClass = (WidgetClass)&dumbManagerClassRec;
#else
#ifdef __GNUC__
GLOBALDEF(DumbManagerClassRec, dumbManagerClassRec, DUMB_CLASS_REC);
GLOBALDEF(WidgetClass, dumbManagerWidgetClass, 
                                   (WidgetClass)&dumbManagerClassRec);
#else
externaldef(dumbmanagerclassrec) DumbManagerClassRec dumbManagerClassRec = 
                                DUMB_CLASS_REC;
externaldef(dumbmanagerwidgetclass) WidgetClass dumbManagerWidgetClass
                              = (WidgetClass)&dumbManagerClassRec;
#endif /* __GNUC__ */
#endif /* VMS */





/*
 * ========================================================================== 
 * ================================== METHODS ===============================
 * ========================================================================== 
 */ 


#if 0
/*
 * ============================ QueryGeometry METHOD ======================== 
 */ 
static XtGeometryResult QueryGeometry( Widget w,
                                       XtWidgetGeometry *request, 
                                       XtWidgetGeometry *reply_return)
{ 
}
#endif

/*
 * ============================ ChangeManaged METHOD ======================== 
 */
static void ChangeManaged(Widget w)

{
#if (DM_DEBUG)
 puts( "ChangeManaged called");
#endif
 dolayout( w);
}


/*
 * ============================ Resize METHOD ======================== 
 */
static void Resize(Widget w)

{
#if (DM_DEBUG)
 puts( "resize called");
#endif
 dolayout( w);
}


/*
 * ============================ GeometryManager METHOD ======================== 
 */
static XtGeometryResult GeometryManager( Widget           w,
                                         XtWidgetGeometry *request,
                                         XtWidgetGeometry *reply )


{ 
  Position x,y;
  Dimension  width, height, bwidth;
  XtGeometryMask mask= request->request_mode;

#if (DM_DEBUG)
puts( "geometry manager called"); 
#endif

 


 x =      (mask & CWX)      ? request->x : w->core.x; 
 y =      (mask & CWY)      ? request->y : w->core.y; 
 width =  (mask & CWWidth)  ? request->width  : w->core.width; 
 height = (mask & CWHeight) ? request->height : w->core.height; 
 bwidth = (mask & CWBorderWidth) ? request->border_width : w->core.border_width; 
 XtConfigureWidget( w, x, y, width, height, bwidth);

 return XtGeometryDone;

}


/*
 * ================== FUNCTION dolayout ===============================
 */
static void dolayout( Widget w)

{
}
