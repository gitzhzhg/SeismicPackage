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
#include "cenv.h"
#include "wproc.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Separator.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/CascadeBG.h>





static void put_help_on_the_widget(  wunion wu[], 
                                     long    i,
                                     HelpCtx helpctx)

{
switch (wu[i].any.wclass) {
     case PUSH_BUTTON   :
     case ARROW_BUTTON  :
     case TEXT          :
     case TOGGLE_BUTTON :
     case SCALEW        :
     case SCROLL_BAR    :
              if ( (helpctx)&&(wu[i].w) ) {
                      add_HELP(wu[i].w,helper,helpctx);
              } /* End if */
              break;
     case CAS_PULL    :
              if ( (helpctx)&&(wu[i].w) ) {
                      add_HELP(wu[i].cpw.cascade,helper,helpctx);
              } /* End if */
              break;
     case OPTION_MENU   :
              if ( (helpctx)&&(wu[i].w) ) {
                      add_HELP(wu[i].opm.menuw,helper,helpctx);
              } /* End if */
              break;
} /* End switch */
}



void create_prim_widgets( wunion wu[],
                          long   cnt,
                          HelpCtx helpctx)
{
   int i;

   for (i= 0; (i<cnt); i++ ) {

     if (!wu[i].w) {
           switch (wu[i].any.wclass) {
               case SEPARATOR     : create_separator( &wu[i] ); break;
               case PUSH_BUTTON   : create_push( &wu[i] ); break;
               case ARROW_BUTTON  : create_push( &wu[i] ); break;
               case TEXT          : create_text( (struct TextW   *)&wu[i] ); 
                                    break;
               case TOGGLE_BUTTON : create_toggle( &wu[i] ); break;
               case SCALEW        : create_scale( &wu[i] ); break;
               case SCROLL_BAR    : create_sb( &wu[i] ); break;
          ENDswitch
     ENDif
     put_help_on_the_widget( wu, i, helpctx );
   ENDloop

}




void wprocPrimHelp( wunion wu[],
                    long   cnt,
                    HelpCtx helpctx)
{
   int i;

   for (i= 0; (i<cnt); i++ )  put_help_on_the_widget( wu,i, helpctx );
}
