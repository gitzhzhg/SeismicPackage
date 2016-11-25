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
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>



#define PMAX 256


main( int argc, char *argv[] )

{
 Display *dpy;
 Screen  *scr;
 Window  wind;
 Colormap cmap;

 long pixels[PMAX];
 unsigned long pmsk[20];
 long i, stat;
 long copy_colors= 50;
 XtAppContext appctx;
 Visual *vis;
 XVisualInfo *vinfo, vtempl;
 long num, cnt;
 XStandardColormap *smap;
 Atom prop;
 char aa[10];



  if (argc > 1) copy_colors= atol(argv[1]);
  printf("number of color to allocate - %d\n",copy_colors);

  XtToolkitInitialize();
  appctx = XtCreateApplicationContext();
  dpy = XtOpenDisplay(appctx, NULL, "test", "Test", 0, 0, &argc, argv);
  if (!dpy) {
      puts("Cannot open display.");
      exit (0);
  }

  scr= DefaultScreenOfDisplay(dpy);
  cmap= DefaultColormapOfScreen(scr);
  wind= RootWindowOfScreen(scr);


  stat= XAllocColorCells( dpy, cmap, True, pmsk, 0, pixels, copy_colors);
  printf("stat after alloc= %d\n",stat);

  cmap=XCreateColormap( dpy, wind, DefaultVisualOfScreen(scr), AllocNone );
  if (cmap==DefaultColormapOfScreen(scr))
      puts("was not able to allocate private colormap");
  else
      puts("private colormap allocated");

  vis= DefaultVisual( dpy, 0);

  vinfo= XGetVisualInfo( dpy, 0, &vtempl, &num);

  prop= XInternAtom( dpy, "RGB_DEFAULT_MAP", True);

  stat= XGetRGBColormaps( dpy, wind, &smap, &cnt, prop);

/**/
  gets(aa); 



/**/


  XtCloseDisplay( dpy );
}
