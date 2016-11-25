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
#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/Intrinsic.h>


void setDefRes( Display *dpy,
                char    *name,
                String  *resourceSpec )
{
   char buf[1000];
   XrmDatabase system_db, new_res_db;
   int i, j;


   /*
    *  Add the Component resources, prepending the name of the component
    */
#if (XlibSpecificationRelease>=5)
    /*
     * if the release is 5 or later then when must support multiple
     * screens.
     */
   for(i=0; (i<ScreenCount(dpy)); i++) {
       new_res_db = XrmGetStringDatabase ( "" );  /* make empty res database*/
       for(j=0; ( resourceSpec[j] != NULL ); j++) {
           sprintf(buf, "*%s%s", name, resourceSpec[j]);
           XrmPutLineResource( &new_res_db, buf );
       }
       /*
        * Merge them into the Xt database, with lowest precendence
        */
       if (new_res_db) {
           system_db = XtScreenDatabase(ScreenOfDisplay(dpy,i));
           XrmCombineDatabase(new_res_db, &system_db, FALSE);
       }
    } /* end loop */
#else
   /*
    * if the release is before 5 then just do the one database.
    */
   new_res_db = XrmGetStringDatabase ( "" );  /* make empty res database */
   for(i=0; ( resourceSpec[i] != NULL ); i++)
   {
       sprintf(buf, "*%s%s", name, resourceSpec[i]);
       XrmPutLineResource( &new_res_db, buf );
   }
   /*
    * Merge them into the Xt database, with lowest precendence
    */
   if (new_res_db) {
         XrmMergeDatabases ( dpy->db, &new_res_db );
         ((Display*)dpy)->db = new_res_db;
   } /* End if */
#endif
}

void setDefResWithClass(
                Display *dpy,
                char    *class_name,
                char    *name,
                String  *resourceSpec )
{
   char buf[1000];
   XrmDatabase system_db, new_res_db;
   int i, j;


   /*
    *  Add the Component resources, prepending the name of the component
    */
#if (XlibSpecificationRelease>=5)
    /*
     * if the release is 5 or later then when must support multiple
     * screens.
     */
   for(i=0; (i<ScreenCount(dpy)); i++) {
       new_res_db = XrmGetStringDatabase ( "" );  /* make empty res database*/
       for(j=0; ( resourceSpec[j] != NULL ); j++) {
           sprintf(buf, "%s*%s%s", class_name, name, resourceSpec[j]);
           XrmPutLineResource( &new_res_db, buf );
       }
       /*
        * Merge them into the Xt database, with lowest precendence
        */
       if (new_res_db) {
           system_db = XtScreenDatabase(ScreenOfDisplay(dpy,i));
           XrmCombineDatabase(new_res_db, &system_db, FALSE);
       }
    } /* end loop */
#else
   /*
    * if the release is before 5 then just do the one database.
    */
   new_res_db = XrmGetStringDatabase ( "" );  /* make empty res database */
   for(i=0; ( resourceSpec[i] != NULL ); i++)
   {
       sprintf(buf, "%s*%s%s", class_name, name, resourceSpec[i]);
       XrmPutLineResource( &new_res_db, buf );
   }
   /*
    * Merge them into the Xt database, with lowest precendence
    */
   if (new_res_db) {
         XrmMergeDatabases ( dpy->db, &new_res_db );
         ((Display*)dpy)->db = new_res_db;
   } /* End if */
#endif
}
