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
#include <unistd.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <X11/cursorfont.h>
#include <Xm/DrawingA.h>
#include <Xm/ScrolledW.h>
#include <limits.h>
#include "plot/pick_watch.hh"
#include "pixmap_set.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_zoomer.hh"
#include "sp/seis_panner.hh"
#include "sp/seis_movie.hh"
#include "sp/inform_list.hh"
#include "sp/seis_inform.hh"
#include "sp/seis_color.hh"
#include "sp/sp_transform.hh"
#include "sp/do_abort.hh"
#include "sp/seis_scrwin.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_hard_copy.hh"
#include "sp/seis_plot_zoom_under.hh"
#include "sl/shell_watch.hh"
#include "sl/shell_mode.hh"
#include "sl/error_handler.hh"
#include "sl/sl_error_pop.hh"
#include "sl/put_widget_msg.hh"
#include "sl/colorset_collection.hh"
#include "sl/color_info_set.hh"
#include "hardcopy/paper_plot.hh"
#include "hardcopy/hardcopy_plot.hh"
#include "hardcopy/hardcopy_trace_plot.hh"
#include "hardcopy/hardcopy_annotate.hh"
#include "hardcopy/hardcopy_raster.hh"
#include "hardcopy/hardcopy_contour.hh"
#include "pick/horizons_picks.hh"
#include "oprim/horizons_manager.hh"
#include "oprim/horizons_vect_style.hh"
#include "net/net_env.hh"
#include "ipc/ll_sd.hh"
#include "trace_selector.hh"
#include "cprim.h"
#include "tfdefs.h"
#include "tf_global.h"
#include "minimax.h"


static const char *DefGraphFont= "-adobe-*-bold-r-*-*-*-120-*-*-m-*-*-*";
static const char *DefSmallGraphFont= "fixed";
#define BoldFontClass    "SPBoldFont"
#define BoldFontInstance "spboldfont"
#define FontClass        "SPFont"
#define FontInstance     "spfont"

static const int DEFGS= 50;
static const int DEFLOADABLE= 34;
static const int DEFCONTOUR= 20;

SPList SeisPlot::_sp_list;

static char *getResource(Widget w, char *, char *, char *);
//static void clean_image(PlotImage *image, ImageInput *user);

class InformSeisZoomer : public SeisZoomer {
 public:
  InformSeisZoomer( SeisPlot *sp,
		    float zoom_factor = 2.0,
		    int   zoom_type   = SpPointer) :
		    SeisZoomer(sp,NULL,zoom_factor,zoom_type) {}
  virtual void zoomComplete( SeisPlot *sp, SeisPlot *,
			     SeisPlot::ZoomDir direction, int );
};

void InformSeisZoomer::zoomComplete( SeisPlot *sp,
				     SeisPlot *,
				     SeisPlot::ZoomDir direction,
				     int stat)
{
  sp->getSeisWinMan()->unlock();
  if (stat !=ZoomerFail) {
	sp->_inform_list->callPostZoom(sp, direction);
	//sp->addPixmaps(); We want SeisZoomer to do this since that class
        //                  knows about all multiple SeisPlots. MLS 5/2002
  }
  else {
	Boolean save_show_warnings= sp->_show_warnings;
	sp->_inform_list->callPostZoom(sp, SeisPlot::Abort);
	sp->_show_warnings= True;
	sp->deliverError("Zoom failed- attempting to replot.",
		     ErrorHandler::Error);
	sp->_show_warnings= save_show_warnings;
  }
  sp->setDoAbortActionComplete();
}

class  ShareDataInform : public SeisInform {
   private:
       SeisPlot *_sharing_sp;
   public:
       ShareDataInform(SeisPlot *sp, SeisPlot *sharing_sp) :
			SeisInform(sp), _sharing_sp(sharing_sp) {}
       virtual void destroyed(SeisPlot *);
};




void ShareDataInform::destroyed(SeisPlot *sp)
{
  assert(_sharing_sp->_data_pointed_to_sp == sp);
  _sharing_sp->_data_pointed_to_sp     = NULL;
  _sharing_sp->_image.setPointToData(False);
  _sharing_sp->_image.setPointToHeaders(False);
  _sharing_sp->_image.releaseHeaderArray();
  _sharing_sp->_image.releaseFloatArray();
  _sharing_sp->_share_data_inform=  NULL;
  delete this;
}


extern "C" void read_data_madj(int);



/*
 *  This is the default constructor used only when an child class plans
 *  to call SeisPlot::constructor also.
 */
SeisPlot::SeisPlot()

	  : _show_warnings(False), _show_errors(True), _show_prog_err(True),
	    _multi_plane(False),
	    _gs_colors(DEFGS), _may_plot(False), _goto_disk(True),
	    _use_shared_gs(False),
	    _color_flags(0) , _description(NULL), _color_share_sp(NULL),
	    _slaves(NULL), _mode_str(NULL), _mode_widget(NULL),
	    _transform(NULL), _hctx(NULL), _new_border(True),
	    _show_left(True), _show_right(True),
	    _show_top(True), _show_bottom(False), _free_unneeded(False),
	    _updating_data(False), _inform_list(NULL),
	    _anno_override_app_request(False), _file_mod_time(0),
	    _data_pointed_to_sp(NULL), _share_data_inform(NULL),
            _sharing_x_resources(False), _cbar_on_hardcopy(On) ,
            _app_wants_persistent_anno(True), _widget_manager(NULL),
            _new_file(False), _redraw_action(Redraw),
            _mintime(0), _maxtime(0), _hpicks(0),
            _shared_contour_colors(FALSE), _color_info_changed(FALSE),
	    _color_required_plot(FALSE), _select(NULL), _results(NULL),
            _type(OVERLAY)
{
  _col_segment = new ColorInfoSegment (this, ColorInfoSet::SEISPLOT);
  _user = _image.getImageInput();
}



/*
 *  This constructor used for underlay images
 */
SeisPlot::SeisPlot( SeisWinMan  *widget_manager,
		    const int    ptype,
		    const int    do_multi_plane)
	:   _show_warnings(False), _show_errors(True), _show_prog_err(True),
	    _multi_plane(do_multi_plane),
	    _gs_colors(DEFGS), _may_plot(False), _goto_disk(True),
	    _color_flags(0) , _description(NULL),
	    _use_shared_gs(False), _color_share_sp(NULL),
	    _slaves(NULL), _mode_str(NULL), _mode_widget(NULL),
	    _transform(NULL), _hctx(NULL), _new_border(True),
	    _show_left(True), _show_right(True),
	    _show_top(True), _show_bottom(False), _free_unneeded(False),
	    _updating_data(False), _inform_list(NULL),
	    _anno_override_app_request(False), _file_mod_time(0),
	    _data_pointed_to_sp(NULL), _share_data_inform(NULL),
            _widget_manager(widget_manager),
            _sharing_x_resources(False), _cbar_on_hardcopy(On),
            _app_wants_persistent_anno(True),
            _new_file(False), _redraw_action(Redraw),
            _mintime(0), _maxtime(0), _hpicks(0),
	    _shared_contour_colors(FALSE), _color_info_changed(FALSE),
	    _color_required_plot(FALSE), _select(NULL), _results(NULL),
            _type(OVERLAY)
{
  char *plotboldfont;
  char *plotfont;
  _col_segment = new ColorInfoSegment (this, ColorInfoSet::SEISPLOT);
  _col= new ColorInfo;
  _sp_list.add(this);
  setNetEnv(NULL);
  plotboldfont= getResource( _widget_manager->W(),
                             BoldFontInstance, BoldFontClass,
                             (char*)DefGraphFont);
  plotfont=     getResource( _widget_manager->W(),
                             FontInstance, FontClass,
                             (char*)DefSmallGraphFont);
  //  clean_image(&_image,&_user);
  _image.initImage(widget_manager->drawingArea(),
		   plotboldfont, plotfont, NULL, NULL, False);
  _user = _image.getImageInput();
  init( widget_manager->drawingArea(), ptype);
}

/*
 *  This constructor usually called by the SeisPlotZoom child
 *  init_image_struct is not called in this constructor unless requested.
 */
SeisPlot::SeisPlot( const Widget  p,
		    const char    *name,
		    const Boolean do_scroll,
                    const Boolean do_initImage)

	: _show_warnings(False), _show_errors(True), _show_prog_err(True),
	  _multi_plane(False),
	  _gs_colors(DEFGS), _may_plot(False), _goto_disk(True),
	  _use_shared_gs(False), _description(NULL), _color_share_sp(NULL),
	  _slaves(NULL), _mode_str(NULL), _mode_widget(NULL),
	  _transform(NULL), _hctx(NULL), _new_border(True),
	  _show_left(True), _show_right(True),
	  _show_top(True), _show_bottom(False), _free_unneeded(False),
	  _updating_data(False), _inform_list(NULL),
	  _anno_override_app_request(False), _file_mod_time(0),
	  _data_pointed_to_sp(NULL), _share_data_inform(NULL),
          _sharing_x_resources(False), _cbar_on_hardcopy(On) ,
          _app_wants_persistent_anno(True),
          _new_file(False), _redraw_action(Redraw),
	  _mintime(0), _maxtime(0), _hpicks(0),
	  _shared_contour_colors(FALSE), _color_info_changed(FALSE),
	  _color_required_plot(FALSE), _select(NULL), _results(NULL),
          _type(OVERLAY)
{
  _col_segment = new ColorInfoSegment (this, ColorInfoSet::SEISPLOT);
  _col = new ColorInfo;
  _sp_list.add (this);
  //clean_image(&_image,&_user);
  _widget_manager = new SeisWinMan(this, p, name, do_scroll);
  _image._graphic = _widget_manager->drawingArea();

  if (do_initImage) {
    // this code is executed only for zoom separate window
    char *plotboldfont;
    char *plotfont;
    plotboldfont = getResource (_widget_manager->W(), BoldFontInstance,
      BoldFontClass, (char*)DefGraphFont);
    plotfont = getResource (_widget_manager->W(), FontInstance, FontClass,
      (char*)DefSmallGraphFont);
    _image.initImage (_widget_manager->drawingArea(), plotboldfont, plotfont,
      NULL, NULL, False);
  }

  _user = _image.getImageInput();
  init( _image._graphic, PlotImage::PlotWFILL);
}


/*
 *  This constructor usually called by the application
 */
SeisPlot::SeisPlot( const Widget  p,
		    const char    *name,
		    const int     ptype,
		    const Boolean do_scroll)

	: _show_warnings(False), _show_errors(True), _show_prog_err(True),
	  _multi_plane(False),
	  _gs_colors(DEFGS), _may_plot(False), _goto_disk(True),
	  _use_shared_gs(False), _description(NULL), _color_share_sp(NULL),
	  _slaves(NULL), _mode_str(NULL), _mode_widget(NULL),
	  _transform(NULL), _hctx(NULL), _new_border(True),
	  _show_left(True), _show_right(True),
	  _show_top(True), _show_bottom(False), _free_unneeded(False),
	  _updating_data(False), _inform_list(NULL),
	  _anno_override_app_request(False), _file_mod_time(0),
	  _data_pointed_to_sp(NULL), _share_data_inform(NULL),
          _sharing_x_resources(False), _cbar_on_hardcopy(On),
          _app_wants_persistent_anno(True), _widget_manager(NULL),
          _new_file(False), _redraw_action(Redraw),
          _mintime(0), _maxtime(0), _hpicks(0),
	  _shared_contour_colors(FALSE), _color_info_changed(FALSE),
	  _color_required_plot(FALSE), _select(NULL), _results(NULL),
          _type(OVERLAY)
{
  _col_segment = new ColorInfoSegment (this, ColorInfoSet::SEISPLOT);
  constructor(p,name,ptype,do_scroll,NULL);
  _user = _image.getImageInput();
}

/*
 *  This constructor usually called by the application
 *  when multiple SeisPlots rotate in one window
 */
SeisPlot::SeisPlot( SeisPlot     *other_sp,
		    const int     ptype)

	: _show_warnings(False), _show_errors(True), _show_prog_err(True),
	  _multi_plane(False),
	  _gs_colors(DEFGS), _may_plot(False), _goto_disk(True),
	  _use_shared_gs(False), _description(NULL), _color_share_sp(NULL),
	  _slaves(NULL), _mode_str(NULL), _mode_widget(NULL),
	  _transform(NULL), _hctx(NULL), _new_border(True),
	  _show_left(True), _show_right(True),
	  _show_top(True), _show_bottom(False), _free_unneeded(False),
	  _updating_data(False), _inform_list(NULL),
	  _anno_override_app_request(False), _file_mod_time(0),
	  _data_pointed_to_sp(NULL), _share_data_inform(NULL),
          _sharing_x_resources(False), _cbar_on_hardcopy(On),
          _app_wants_persistent_anno(True), _widget_manager(NULL),
          _new_file(False), _redraw_action(Redraw),
          _mintime(0), _maxtime(0), _hpicks(0),
	  _shared_contour_colors(FALSE), _color_info_changed(FALSE),
	  _color_required_plot(FALSE), _select(NULL), _results(NULL),
          _type(OVERLAY)
{
 assert(other_sp);
 _col_segment = new ColorInfoSegment (this, ColorInfoSet::SEISPLOT);
 constructor(NULL,NULL,ptype,True,other_sp);
 _hctx= other_sp->_hctx;
 _image._statusw= other_sp->_image._statusw;
 _mode_widget= other_sp->_mode_widget;
 other_sp->_inform_list->callNewSeisPlotCreatedInWindow(this);
 _user = _image.getImageInput();
 _slaves= other_sp->_slaves;
}



void SeisPlot::constructor( const Widget    p,
			    const char     *name,
			    const int       ptype,
			    const Boolean   do_scroll,
                                  SeisPlot *other_sp)
{
  char *plotboldfont;
  char *plotfont;
  _multi_plane= False;
  _col= new ColorInfo;
  _sp_list.add(this);
  read_data_madj(False);
  setNetEnv(NULL);
  if (other_sp) {
     _widget_manager= other_sp->_widget_manager;
     _widget_manager->addSeisPlot(this);
     _image._xydisp->xloc=   other_sp->_image._xydisp->xloc;
     _image._xydisp->yloc=   other_sp->_image._xydisp->yloc;
     _image._xydisp->zloc=   other_sp->_image._xydisp->zloc;
     _image._xydisp->status= other_sp->_image._xydisp->status;

     _anno_override_app_request= other_sp->_anno_override_app_request;
     _app_wants_persistent_anno= other_sp->_app_wants_persistent_anno;
     _new_border = other_sp->_new_border;
     _show_left  = other_sp->_show_left;
     _show_right = other_sp->_show_right;
     _show_top   = other_sp->_show_top;
     _show_bottom= other_sp->_show_bottom;

     if (other_sp->_hpicks) {
       _hpicks = new HorizonsPicks (other_sp->_hpicks->manager(), this,
         other_sp->_hpicks->style());
     }
  }
  else {
     if (!_widget_manager)
           _widget_manager= new SeisWinMan(this, p, name, do_scroll);
  }
  plotboldfont= getResource( _widget_manager->W(),
                             BoldFontInstance, BoldFontClass,
                             (char*)DefGraphFont);
  plotfont=     getResource( _widget_manager->W(),
                             FontInstance, FontClass,
                             (char*)DefSmallGraphFont);
  _image.initImage(_widget_manager->drawingArea(),
                     plotboldfont, plotfont, NULL, NULL, False);
  _user = _image.getImageInput();
  init( _widget_manager->drawingArea(), ptype);

//   Note the reason this is below the
//   previous "if (other_sp)" (ref line 314) is that stuff needed to
//   be initialized in the "init( _widget..." above first! This could be
//   very error prone!
//   
  if (other_sp) {
    // share _col, &_col_gs, & &_col_contour to corresponding ColorInfoSet's
    //   in other_sp
    ColorInfoSet *other_set, *set;
    other_set = ColorInfoCollection::fetchExisting (other_sp->_col);
          set = ColorInfoCollection::fetchExisting (          _col);
    other_set->addSharedSet (set);
    other_set = ColorInfoCollection::fetchExisting (&(other_sp->_col_gs));
         set = ColorInfoCollection::fetchExisting (&(          _col_gs));
    other_set->addSharedSet (set);
    other_set = ColorInfoCollection::fetchExisting (&(other_sp->_col_contour));
          set = ColorInfoCollection::fetchExisting (&(          _col_contour));
    other_set->addSharedSet (set);
  }
}



//static void clean_image(PlotImage *image, ImageInput *userd/)
//{
  //constructor calls this which we dont want to do with C++ version
  //of image library. It nulls out the user class



  //memset(image,'\0', sizeof(struct PlotImage));
  //memset(user,'\0', sizeof(struct ImageInput));
//}



SeisPlot::~SeisPlot ()
{

  _inform_list->callDestroyed(this);
  _slaves= NULL;
  cleanup();
  if (_sharing_x_resources)
          _image.setSharingResources(True);
  else
          _image.imageFreeXresources( );


  if (_cshares.top()) _cshares.top()->takeOverSharingFrom(this);

  _sp_list.remove(this);

  ColorInfoCollection::remove (_col_segment);
  CheckColorSegments::deleteSegment (_col_segment);

  if (_color_share_sp) {
    _color_share_sp->_cshares.remove(this);
  }
  else {
    freeColors(); // linux crashes when this happens

    // the only reason this is not generally needed is that
    //   the _col and _col_gs uses the _color_share_sp instead
    //   of the local ones
    ColorInfoCollection::remove (_col);
    ColorsetCollection::remove  (_col);

    ColorInfoCollection::remove (&_col_gs);
    ColorsetCollection::remove  (&_col_gs);

    delete _col;
    delete _sc;
  }

  //Added 10/98 in order to free contour colors
  if ( _col_contour.colorsafe) {
/*
    XFreeColors(_widget_manager->display(), _col_contour.cmap,
      _col_contour.pix, _col_contour.cnum, _col_contour.numplanes);
*/
    ColorsetCollection::clear (&_col_contour);

    _inform_list->callColorChange (0,CONT);
    _col_contour.colorsafe = False;
  }

  if (!_shared_contour_colors) {
    ColorInfoCollection::remove (&_col_contour);
    ColorsetCollection::remove  (&_col_contour);
  }

  _widget_manager->delSeisPlot(this);
  if (_description) free(_description);
  if (_mode_str)    free(_mode_str);
  if (_image._xydisp && _image._xydisp->override_x_str)
    free (_image._xydisp->override_x_str);
  if (_image._xydisp && _image._xydisp->override_y_str)
    free(_image._xydisp->override_y_str);
  delete _do_abort;
  delete _inform_list;
  if (_select ) delete _select;
  if (_results) delete _results;
}

void SeisPlot::cleanup()
{
  delPixmaps();
  _image.imageFree();
  _inform_list->callNoPlotDisplayed(this);
  SeisPlot *sp= _chained_sp.top();
  if (sp) sp->cleanup();
  _goto_disk= True;
  _image._displayed= False;
  _image._frames= 0;
}

SeisPlot& SeisPlot::operator=(SeisPlot& sp)
{

  cleanup();
  _user= sp._user;
  _image._xydisp->override_x_str= newstr(sp._image._xydisp->override_x_str);
  _image._xydisp->override_y_str= newstr(sp._image._xydisp->override_y_str);

  //External data type image
  if(sp._image._filedata == False && sp._image._point_to_data == True)
    {
    pointToArrayTypeData(&sp);
    setGridXYS(sp.gridX1(),sp.gridX2(),sp.gridY1(),sp.gridY2());
    _image._nsamp = sp._image._nsamp;
    for(int i=0;i<sp.frames();i++)
      setTracesPerPanel(i, sp.getTracesPerPanel(i));
    _image._ntot = sp.plottedNplt();
    }


  _goto_disk= True;
  _user->_filename[0]= '\0';
  setFilename(sp._user->_filename);
  _tmin=            sp._tmin;
  _tmax=            sp._tmax;
  _x_unit_per_inch= sp._x_unit_per_inch;
  _y_unit_per_inch= sp._y_unit_per_inch;
  _grid_x1=         sp._grid_x1;
  _grid_x2=         sp._grid_x2;
  _grid_y1=         sp._grid_y1;
  _grid_y2=         sp._grid_y2;
  _plot_width=      sp._plot_width;
  _plot_height=     sp._plot_height;

  _zoom_factor=     sp._zoom_factor;
  _zoom_type=       sp._zoom_type;
  _box_size=        sp._box_size;
  _origin_scale=    sp._origin_scale;
  _scan_a_screen=   sp._scan_a_screen;
  _3d_axis=         sp._3d_axis;
  _3d_slice=        sp._3d_slice;

  sp._sc->loadToSeisPlot(this);

  return *this;
}

/*
 * static routines to get resource
 */
static char *getResource(Widget  w,
                         char   *inststr,
                         char   *clstr,
                         char   *default_res)
{
 char instance_res[1000];
 char class_res[1000];
 char *res;

 make_wctree( w, instance_res, class_res, wprocALL);
 strcat(instance_res, ".");
 strcat(instance_res, inststr);
 strcat(class_res, ".");
 strcat(class_res, clstr);
 res= DefGetStrValue( XtDisplay(w), instance_res, class_res);
 if (!res) res= default_res;
 return res;
}



/*
 *  ===================== Method ===================
 */
void SeisPlot::init( Widget da,
		     int ptype)

{
  long vis_class;

  _temp_G.tstrt  = 0.0;
  _temp_G.srval  = 1.0;
  _temp_G.h      = 0;
  _temp_G.nbydp  = 0;
  _temp_G.nbyhd  = 0;
  _temp_G.ntrfil = 0;
  _temp_G.nhdwd  = 0;
  strcpy(_temp_G.ftyp,"");

  _inform_list= new InformList();
  _color_flags= test_vis( XtDisplay(da), &vis_class, NULL );

  _user->_mode= PlotImage::PlotWFILL;// init _user->_mode (plot type) to anything
  setPlotType(ptype);     // now set it the passed plot type

  Colormap cmap;
  XtVaGetValues( da, XmNcolormap, &cmap, NULL );
  _do_abort= new DoAbort(XtParent(da));
  _image.setImageAbortFunction((ImageAbortFunction)DoAbort::altUserAbort,
                               (void*)_do_abort);
  _image.setImageXYOutputFunction((ImageXYOutputFunction)xyUpdate, this);
  //the following line commented and replaced by setting the contour planes
  //to the multi_plane variable so that we can underlay contours
      //  _col_contour.numplanes= _col_gs.numplanes=0;
  _col_contour.numplanes= _multi_plane ? 1 : 0;
  _col->numplanes= _multi_plane ? 1 : 0;
   for(int i=0; (i<24); _col->pmsk[i++]= 0);
  _col_gs.cmap= _col_contour.cmap= _col->cmap= cmap;
  _col_contour.cnum= DEFCONTOUR;
  _col->cnum= DEFLOADABLE;
  _col_gs.cnum= DEFGS;
  _col_gs.colorsafe = _col_contour.colorsafe = _col->colorsafe= False;
  _col_gs.shared = _col_contour.shared = _col->shared = False;

  ColorInfoCollection::fetch ( _col        , _col_segment,
    ColorInfoSet::TWO_WAY);
  ColorInfoCollection::fetch (&_col_gs     , _col_segment,
    ColorInfoSet::TWO_WAY);
  ColorInfoCollection::fetch (&_col_contour, _col_segment,
    ColorInfoSet::TWO_WAY);

////////////////////////////// new ////////////////////////////
  _image.colorInfoInit (_col, &_col_contour);
////////////////////////////// new ////////////////////////////

  showBorders(_show_left, _show_right, _show_top, _show_bottom);
  initUser();
  _draw_extra_y_label = False;
  _draw_extra_x_label = False;
}


/*
 * Only used in one place and phasing out
 */

static void compress_and_load(Widget       /*w*/,
			      ColorInfoPtr col,
			      long         barnum,
			      ImageInput   *user,
			      PlotImage    *image)
{
   float rgbs[1024];
   long numc, comp=0, inten=10;
   long tracevalues;

   image->definedColor(rgbs, barnum, &numc, &tracevalues, col->cnum);
   if(col == image->getColorStructure())
     for (int i = 0; (i<1024); i++) user->_cbar_rgb[i]= rgbs[i];
   else
     for (int i = 0; (i<1024); i++) user->_contour_rgb[i]= rgbs[i];
   user->_num_cbar_cols= numc;
   image->storeColors(rgbs, numc, &comp, &inten, col, NULL);
}

void SeisPlot::freeColors ()
{
  Display *dpy = _widget_manager->display ();

  if (_col->colorsafe) {
//  XFreeColors (dpy, _col->cmap, _col->pix, _col->cnum, _col->numplanes);
    ColorsetCollection::clear (_col);
    callAllColorChange (0);
    _col->colorsafe = False;
  }
}

void SeisPlot::callAllColorChange(int cnum)
{
 SeisPlot *q;
 void *vp;

 _inform_list->callColorChange(cnum,COLOR);
 for(q=_cshares.top(&vp); (q); q= _cshares.next(&vp)) {
       q->callAllColorChange(cnum);
 } // end loop
}


Boolean SeisPlot::haveColors ()
{
//Display *dpy= _widget_manager->display();
  char wkstr[400];

  if (!_col->colorsafe) {
    // store gray colors
    int allocated_colors;
    if (ColorsetCollection::readOnly(_col)) {
      if (allocated_colors = !ColorsetCollection::allocate(_col)) {
	ColorsetCollection::store (_col);
      }
    }
    else {
      // historically when not readOnly, some differentiation with
      //   _col->numplanes was helpful!!!???
      if (allocated_colors = !ColorsetCollection::allocate(_col)) {
        ColorsetCollection::store (_col);
      }
    }
    if (allocated_colors) {
//    if (alloc_gray_colors( dpy, _col)) {
      ColorInfoSet *set = ColorInfoCollection::fetchExisting (_col);
      set->update (_col);
      callAllColorChange (_col->cnum);
      _image.setColorInfoStructure (_col); // for 8-bit va
    }
    else {
      sprintf(wkstr,"SeisPlot: Could not allocate %d colors.",_col->cnum);
      deliverError( wkstr, ErrorHandler::ProgError);
      if (_col->numplanes > 0) {
	sprintf (wkstr, "SeisPlot: the failed allocation included %d planes",
	  _col->numplanes);
	deliverError( wkstr, ErrorHandler::ProgError);
      }
    }
  }
  return (_col->colorsafe);
}


Boolean SeisPlot::haveContourColors ()
{
//Display *dpy= _widget_manager->display();
  char wkstr[400];

  if (!_col_contour.colorsafe) {
    // store gray colors
    int allocated_colors;
    if (ColorsetCollection::readOnly(&_col_contour)) {
      if (allocated_colors = !ColorsetCollection::allocate(&_col_contour)) {
	ColorsetCollection::store (&_col_contour);
      }
    }
    else {
      // historically when not readOnly, some differentiation with
      //   _col->numplanes was helpful!!!???
      if (allocated_colors = !ColorsetCollection::allocate(&_col_contour)) {
	ColorsetCollection::store (&_col_contour);
      }
    }
    if (allocated_colors) {
//    if (alloc_gray_colors( dpy, &_col_contour)) {
      ColorInfoSet *set = ColorInfoCollection::fetchExisting (&_col_contour);
      set->update (&_col_contour);
      _inform_list->callColorChange (_col_contour.cnum, CONT);
      _image.setSecondaryColorInfoStructure (&_col_contour);
    }
    else {
      sprintf(wkstr,
	"SeisPlot: Could not allocate %d colors", _col_contour.cnum);
      deliverError( wkstr, ErrorHandler::ProgError);
    }
  }
  return (_col_contour.colorsafe);
}

void SeisPlot::shareContourColors(ColorInfoPtr contour_colors)
{
  freeContourColors();

  ColorInfoSet *set = ColorInfoCollection::fetchExisting (&_col_contour);
  int role = set->role (_col_segment);

  if (!_shared_contour_colors) {
    ColorInfoCollection::remove (&_col_contour);
    ColorsetCollection ::remove (&_col_contour);
  }
  else {
    set->removeSharedElement (_col_segment);
  }
  _shared_contour_colors = TRUE;

  _col_contour = *contour_colors;

  if (set = ColorInfoCollection::fetchIfExisting(&_col_contour)) {
    set->addSharedElement (_col_segment, role);
  }
  else {
    set = ColorInfoCollection::fetch (&_col_contour, _col_segment, role);
  }
  set->update (&_col_contour);

  _image.setSecondaryColorInfoStructure (contour_colors);
}


Boolean SeisPlot::autoAllocColors ()
{
  long mode = plotType();
  Boolean succ= True;
  Boolean using_col= False;
  Boolean using_col_contour= False;
  Display *dpy= _widget_manager->display();
  Widget w = _image._graphic;



  switch (mode) {
      case PlotImage::PlotWONLY:
      case PlotImage::PlotWFILL:
      case PlotImage::PlotHEADER:
      case PlotImage::PlotGRID:
	  _user->_do_color= False;     // no color allocating needed
	  break;

      case PlotImage::PlotCOLOR:
	  if (_sc->predef() == PlotImage::GRAY) {
		 if (_use_shared_gs) {
		      _image.setColorInfoStructure(&_col_gs);
		      succ= True;
		 }
		 else {
		      succ= haveColors();
		      _user->_do_color= using_col= True;
		 }
	  }
	  else {
		 succ= haveColors();
		 _user->_do_color= using_col= True;
	  }
	  break;

      case PlotImage::PlotISO:
      case PlotImage::PlotSEMB:
	  succ= haveColors();
	  _user->_do_color= using_col= True;
	  break;
  } // end switch


  if ((mode ==PlotImage::PlotCONTOUR) || ((mode ==PlotImage::PlotSEMB) ||
      (mode ==PlotImage::PlotARRAY)   && (_user->_contours>0))){
     using_col_contour= True;
     succ= haveContourColors();
     if (succ) {
        _image.setSecondaryColorInfoStructure(&_col_contour);
        compress_and_load(w, &_col_contour, PlotImage::CONTOUR, _user,&_image);
     }
  }

  /*
   * if we are doing something with loadable colors then load the
   * appopriate color bar
   */
  if ( (succ) && (using_col) ) {
       _sc->prepareColors(this);
  }


  if ((!using_col) && (_free_unneeded)) {
    freeColors();
  }

  if ((!using_col_contour) && (_free_unneeded)&&(_col_contour.colorsafe)) {
    ColorsetCollection::clear (&_col_contour);
/*
  XFreeColors(_widget_manager->display(), _col_contour.cmap,
    _col_contour.pix, _col_contour.cnum, _col_contour.numplanes);
*/
    _inform_list->callColorChange (0, CONT);
  }

  return (succ);

}




/*
 * ====================== Method ==============================
 *  Set all values in the user struct
 */
void SeisPlot::initUser()
{
   _sc= new SeisColor(this,_col);
   _normal_error_processing= True;
   _user->_nplt=   100;
   _user->_ndo=    1;
   _user->_ct =    4.0;
   _user->_tdec =  1;
   _user->_hdrOne= 1;
   _user->_hdrTwo= 37;
   _user->_nskp =  0;
   _user->_ptl =   1.0;
   _user->_stl =   0.2;
   _user->_firstLbl = 1;
   _user->_LblInc   = 20;
   _user->_vel_min = 4000;
   _user->_vel_max = 20000;
   _user->_gradev =   True;
   _user->_gradeh =   True;
   _user->_filename[0]= '\0';
   _previous_filename[0]= '\0';
   _user->_ppc =  100;
   _user->_pnc =  100;
   _user->_do_percent= True;
   _user->_external_amplitude= 1.0;

   _user->_movie= False;
   _user->_frames= 0;
   _user->_skip_frames= 0;

   _zoom_type= Pointer;
   _zoom_factor= 2.0;
   _box_size= 300;
   setGridXYS(0.0, 1.0, 0.0, 1.0);

   _origin_scale=  True;
   _scan_a_screen= False;


   _image._xydisp->xloc= NULL;
   _image._xydisp->yloc= NULL;
   _image._xydisp->zloc= NULL;
   _image._xydisp->init= False;
   _image._xydisp->override_x_str= NULL;
   _image._xydisp->override_y_str= NULL;

   _tmin= 0.0;
   _tmax= 1.0;
   _plot_width= 5.0;
   _plot_height= 5.0;
   _x_unit_per_inch   =    20.0;
   _y_unit_per_inch   =    2.0;
   _grid_x1= 0;
   _grid_x2= 100;
   _grid_y1= 0;
   _grid_y2= 100;
   _3d_axis= 0;   // set to none
   _3d_slice= 1;  // set to first line

   switch ( plotType() ) {


     case PlotImage::PlotCONTOUR:
     case PlotImage::PlotSEMB:
	_user->_do_color = True;
	_user->_norm =  False;
	_x_unit_per_inch =    2.0;
	_y_unit_per_inch =    4.0;
	_user->_contours = 8;
	_user->_minp = .05;
	_user->_maxp = .98;
	_user->_gradev = True;
	_user->_gradeh = True;
	//_bar_number= SEMBLANCE;
	_sc->setPredef(PlotImage::SEMBLANCE);
	_user->_hdrOne= 6;
	_user->_hdrTwo= 37;
	setPlotDescription("Semblance");
	break;

     case PlotImage::PlotGS:
	_user->_do_color = True;
	_user->_norm =  False;
	_sc->setPredef(PlotImage::GRAY);
	setPlotDescription("Seismic");
	break;

     case PlotImage::PlotWONLY:
     case PlotImage::PlotWFILL:
     case PlotImage::PlotGRID:
     case PlotImage::PlotHEADER:
	_user->_do_color = False;
	_user->_norm =     False;
	_sc->setPredef(PlotImage::STANDARD);
	setPlotDescription("Seismic");
	break;

     case PlotImage::PlotISO:
	_user->_hdrOne   = 1;
	_user->_hdrTwo   = 2;
        _user->_xlabel_header = 1;
	_user->_do_color = True;
	_user->_norm     = True;
	_user->_firstLbl = 1;
	_user->_LblInc   = 1;
	_x_unit_per_inch= 2.0;
	_y_unit_per_inch= 4.0;
	_user->_G.srval= .004;
	_user->_G.nbydp = 8;
	_sc->setPredef(PlotImage::STANDARD);
	setPlotDescription("IsoVel");
	break;

   }

}



/*
 * ====================== Public Method ==============================
 *  return True if this SeisPlot is current in the window
 */
Boolean SeisPlot::isCurrentInWindow()
{
  assert (_widget_manager);
  Boolean retval= (_widget_manager->currentSP() == this);
  return retval;
}

/*
 * ====================== Public Method ==============================
 *  return the SeisPlot that is current in the window
 */
SeisPlot *SeisPlot::currentSPInWindow()
{
  assert (_widget_manager);
  return _widget_manager->currentSP();
}

/*
 * ====================== Public Method ==============================
 *  return the topmost widget created
 */
Widget SeisPlot::W()
{
  assert (_widget_manager);
  return _widget_manager->W();
}


/*
 * ====================== Public Method ==============================
 *  does error check before a plot
 */
Boolean SeisPlot::readyToPlot()
{
 Boolean succ= True;
 char estr[400];
 long ptype;
 strcpy(estr, "SeisPlot: not ready to plot");

 if (!XtWindow(imageGraphic())) {
	succ= False;
	strcpy(estr, "SeisPlot: Attempt to plot before widgets realized.");
	deliverError(estr,ErrorHandler::ProgError);
 }

 if (_user->_movie) {
     if ((_image._cpixm <0) || (_image._cpixm > _user->_frames))
	   _image._cpixm= 0;
     if (_user->_frames <= 0)  {
	   _user->_movie= False;
	   strcpy(estr,
	    "SeisPlot: movie of 0 frames requested- movie mode set to False");
	   deliverError(estr,ErrorHandler::Warning);
     }

 }
 else _image._cpixm= 0;

 ptype= plotType();
 if ( (strlen(_user->_filename) == 0) || ( ptype == PlotImage::PlotGRID) ||
      ( ptype == PlotImage::PlotISO) || ( ptype == PlotImage::PlotHEADER))
    return(succ);

 if ((succ) && (_user->_nplt < 1)) {
   succ= False;
   strcpy(estr, "SeisPlot: Number to plot is less than 1");
   deliverError(estr,ErrorHandler::Error);
 }
 if ((succ) && (_tmax <= _tmin)) {
   succ= False;
   strcpy(estr, "SeisPlot: Tmin must be less that Tmax");
   deliverError(estr,ErrorHandler::Error);
 }

 return (succ);
}

Boolean SeisPlot::checkPlotStatus(int pstat)
{
  Boolean retval= False;
  Pixel back_pix;
  SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
  Boolean user_aborted = False;

  strcpy(_last_errstr, _image.getErrorString());

  switch (pstat) {
      case PlotImage::PlotSuccess :
	      addPixmaps();
	      _inform_list->callNewPlot(this);
	      retval= True;
	   break;

      case PlotImage::PlotWarning :
	     deliverError(_last_errstr,ErrorHandler::Warning);
	     addPixmaps();
	     _inform_list->callNewPlot(this);
	     retval= False;
	   break;

      case PlotImage::ResourceFail :
      case PlotImage::EofFail :
      case PlotImage::ReadFail :
	      deliverError(_last_errstr,ErrorHandler::Error);
	      cleanup();
	   break;
      case PlotImage::UserAbort:
	      strcpy(_last_errstr, "User Aborted Plot.");
              _new_file = True;
              user_aborted = True;
	      redraw();
	      cleanup();
	      retval= False;
	   break;
  }
  if ( (pstat == PlotImage::PlotWarning) || (pstat ==PlotImage:: PlotSuccess) ){
	 if (scrwin) {
		 scrwin->setCornerFillColor(_image._white_pixel);
		 scrwin->redrawAnnotation();
	   if (_new_border) {
	       showBorders(_show_left, _show_right, _show_top, _show_bottom);
	       _new_border= False;
	   }  // end if _new_border
	 }  // end if scrwin
  }
  else {
	 if (scrwin) {
	      XtVaGetValues(imageGraphic(), XmNbackground, &back_pix, NULL);
	      scrwin->setCornerFillColor(back_pix);
	 } // end if
  } // end else

  _do_abort->actionComplete();
  _last_pstat= pstat;
  _updating_data= False;
  _goto_disk= (pstat > 0) ? False : True;
  if(user_aborted == False)_new_file= False;
  return retval;
}

void SeisPlot::callNewPlot ()
{
  _inform_list->callNewPlot (this);
}

void SeisPlot::displayPixmap()
{
  long chained_displayed= False;
  SeisPlot *sp= _chained_sp.top();
  if (sp) chained_displayed= sp->isPlotDisplayed();
  if (isPlotDisplayed()) {
     if (_slaves) {
         if ((chained_displayed) && (!_multi_plane))
	     _slaves->displayPixmap( _image._pixmary[_image._cpixm],
				     sp->_col->pmsk[0]);
         else
	     _slaves->displayPixmap( _image._pixmary[_image._cpixm]);
     } // end if
  } // end if
}

void SeisPlot::addPixmaps()
{
  if (_slaves) {
      _slaves->numPixmaps((int)plottedFrames());
      for(int i=0; (i<MAX_PIXMAP); i++)
	 if (_image._pixmary[i]) _slaves->addPixmap( _image._pixmary[i]);
      displayPixmap();
      _slaves->setBorder( (int)leftBorder(), (int)rightBorder(),
			  (int)topBorder(), 0);
  }
}

void SeisPlot::delPixmaps()
{
  if (_slaves) {
      for(int i=0; (i<MAX_PIXMAP); i++)
	 if (_image._pixmary[i])
		 _slaves->deletePixmap( _image._pixmary[i]);
  }
}


void SeisPlot::isDiff(float a, float b)
{
  float diff = a-b;

  if ( (diff < -0.00001) || (diff > 0.00001) )
	_goto_disk= True;
}

void SeisPlot::isDiff(double a, double b)
{
  double diff = a-b;

  if ( (diff < -0.00001) || (diff > 0.00001) )
	_goto_disk= True;
}


/*
 * this function is called from image as a function pointer in
 * in image stucture.  It is called ever time there is a motion event
 * on the plot.
 */
void SeisPlot::xyUpdate(void *data, int x, int y)
{
  SeisPlot *obj= (SeisPlot*)data;
  obj->_inform_list->callMouseOutputUpdate(obj, x, y);
}

void SeisPlot::updateUserForPlot()
{
//If we are reading file data then copy the temporary Global struct into
//the ImageInput Global struct.
 if(plotType() != PlotImage::PlotISO && plotType() != PlotImage::PlotGRID &&
    _image._point_to_data == False)
   {
   memcpy(&_user->_G, &_temp_G, sizeof(GLBL) );
   setnumHeaderWords(_user->_G.nhdwd);
   }

 _image._Cl.trnsps= False;

 switch (plotType()) {
       case PlotImage::PlotGRID:
       case PlotImage::PlotHEADER:
       case PlotImage::PlotISO:
	    _user->_ti= _plot_width;
	    _user->_is= _plot_height;
	    _user->_tmin= _grid_y1;
	    _user->_tmax= _grid_y2;
	    _user->_grid_x1= _grid_x1;
	    _user->_grid_x2= _grid_x2;
	    _user->_grid_y1= _grid_y1;
	    _user->_grid_y2= _grid_y2;
	    _image._grid_x1= _grid_x1;
	    _image._grid_x2= _grid_x2;
	    _image._grid_y1= _grid_y1;
	    _image._grid_y2= _grid_y2;
	    break;

       case PlotImage::PlotSEMB:
            _user->_grid_x1= _grid_x1;
	    _user->_grid_x2= _grid_x2;
	    _user->_grid_y1= _grid_y1;
	    _user->_grid_y2= _grid_y2;
	    _image._grid_x1= _grid_x1;
	    _image._grid_x2= _grid_x2;
	    _image._grid_y1= _grid_y1;
	    _image._grid_y2= _grid_y2;
            _user->_ti= _x_unit_per_inch;
	    _user->_is= _y_unit_per_inch;
	    _user->_tmin= _tmin;
	    _user->_tmax= _tmax;
            break;
       default:
	    _user->_ti= _x_unit_per_inch;
	    _user->_is= _y_unit_per_inch;
	    _user->_tmin= _tmin;
	    _user->_tmax= _tmax;
	    break;
  } // end switch

  /*
   *  3D stuff - if we are plotting a slice from a cube
   */
  if (tf_global_is3d(&_user->_G)) {
        float o1, o2, o3;
        float d1, d2, d3;
        int   n1, n2, n3;
        _image._Cl.index= _3d_slice;
        _image._Cl.axis=  _3d_axis;
        switch (_3d_axis) {
           case 1:  // plot a time slice
                    _image._Cl.trnsps= True;
                    tf_global_grid_orgs (&_user->_G, &o1, &o2, &o3);
                    tf_global_grid_delta(&_user->_G, &d1, &d2, &d3);
                    tf_global_grid_sizes(&_user->_G, &n1, &n2, &n3);

                    _user->_tmin = o3;
                    _user->_tmax = o3  + (n3 - 1) * d3;
                    _user->_G.tstrt = _user->_tmin;
                    _user->_G.srval = (max(_user->_tmin, _user->_tmax) -
                                     min(_user->_tmin, _user->_tmax) )
                                     / (n3 - 1);
                    _user->_nplt   = n2;
                    _image._nsamp = n3;

                    _user->_is= 1 / ((1/_user->_is) * d3);

                    break;
           case 2:  // plot a cross line
                    _user->_nplt= tf_global_grid_size(&_user->_G, 3);
                    break;
           case 3:  // plot a inline
                    _user->_nplt= tf_global_grid_size(&_user->_G, 2);
                    break;
        } // end switch
  }
  else {
        _image._Cl.index= 0;
        _image._Cl.axis= 0;
  }
}


/*
 *
 ********************************************************************
 ********************************************************************
 ********************                            ********************
 ********************       Action Methods       ********************
 ********************                            ********************
 ********************************************************************
 ********************************************************************
 *
 */
/*
 * ====================== Public Method ==============================
 *  plots the file or replots the data
 */
int SeisPlot::plot()
{
 long stat;
 int retval=False;
 Boolean chain_plot_exist= False;
 ShellWatch watch= _image._graphic;
 PickWatch pw;
 PutWidgetMsg mode_msg(_mode_widget,_mode_str);
 char *_save_errstr= NULL;
 long ptype;


 if ( !readyToPlot() ) {
      _last_pstat= PlotImage::ReadFail;
      return False;
 }

 if ( !autoAllocColors()) {
      _last_pstat= PlotColorFail;
      return False;
 }

 if (_widget_manager->find(this)) _widget_manager->setCurrentSP(this);

 _image._zoomed= False;
 _image._zindex= NO_ZOOM;
 SeisPlot *sp= _chained_sp.top();
 if (sp) {
    sp->delPixmaps();
    sp->cleanup();
 }
 chain_plot_exist= (_image._chain_image != NULL);

 ptype= plotType();
 _inform_list->callPrePlot(this);
 updateEnable();

 if ( ((strlen(_user->_filename) == 0) || ( ptype == PlotImage::PlotGRID) ||
      ( ptype == PlotImage::PlotHEADER))  && ( ptype != PlotImage::PlotISO) ) {

       if (ptype != PlotImage::PlotHEADER)//I might should call setPlotType here
		_user->_mode= PlotImage::PlotGRID;
       updateUserForPlot();
       _image.setGridSize();
       delPixmaps();
       stat = _image.imageGrid();
       retval= checkPlotStatus((int)stat);
 }
 else {
    updateUserForPlot();
    if (_goto_disk) {
      delPixmaps();
      if (ptype != PlotImage::PlotISO) _do_abort->setNewAction();
      stat= _image.checkSize();

      switch (stat) {                             /* dump out */
	case PlotImage::ResourceFail:
	case PlotImage::ReadFail:
	case PlotImage::EofFail:
	case PlotImage::UserAbort:
                  retval= checkPlotStatus((int)stat);
	   break;

	case PlotImage::PlotWarning:
              strcpy(_last_errstr, _image.getErrorString());
	      _save_errstr= newstr(_last_errstr);
	      stat= _image.plot();
	      /*
	       * if a successful plot still downgrade the status to warning
	       * because we had a problem in check_size
	       */
	      if (stat == PlotImage::PlotSuccess) {
		     stat= PlotImage::PlotWarning;
		     strcpy(_last_errstr, _save_errstr);
	      }
	      retval= checkPlotStatus((int)stat);
	      free(_save_errstr);
	      break;

	case PlotImage::PlotSuccess:
	      stat= _image.plot();
	      retval= checkPlotStatus((int)stat);
              break;
      }

    } // end if
    else {
	      ShellWatch watch= _image._graphic;
	      delPixmaps();
              if (ptype != PlotImage::PlotISO) _do_abort->setNewAction();
	      stat= _image.modifyPlot();
	      retval= checkPlotStatus((int)stat);
    }
 }

 if ( (chain_plot_exist) && (_image._chain_image == NULL) ) sp->cleanup();
 _color_required_plot = FALSE;
 _color_info_changed = FALSE;
 return retval;
}

//Blank out an existing image
void SeisPlot::blankImage(float xloc, float yloc)
{
SeisScrWin  *scrwin= _widget_manager->scrolledWindow();

  if(imageIsDisplayed())
    {
    _image.blankImage(xloc,yloc);
    //following line put in so Ed's va picking routines will know to redraw
    _inform_list->callExpose(this,0,0,(int)_image._graph_width,
                             (int)_image._graph_height);
    if (scrwin) scrwin->redrawAnnotation();
    }
}

//compute semblance contours
void SeisPlot::contourSemblanceData(float *di, float *dj, float *conval,
                                    float *z1, float *z2, float *z3, float *z4,
                                    float *dx, float *dy,
                                    float *xs, float *ys,
                                    float *velocity, float *conint,
                                    float *zminm, float *zmaxm,
                                    float *gpiy, long *is, long *numvels)
{
  _image.contourLevel(   di,  dj,  conval,
                         z1,  z2,  z3,  z4,
                         dx,  dy,
                         xs,  ys,
                         velocity,  conint,
                         zminm,  zmaxm,
                         gpiy, is, numvels);
}

//Redraw an entire pixmap with modified data
long SeisPlot::redrawImage( long frame_index )
{
long stat;
long retval = False;
SeisScrWin  *scrwin= _widget_manager->scrolledWindow();

 if(imageIsDisplayed())
    {
    _inform_list->callPrePlot(this);
    stat = _image.redrawImage(frame_index);
    if (scrwin) scrwin->redrawAnnotation();
    retval= checkPlotStatus((int)stat);
    }

 return retval;
}


//Draw a label in the upper left annotation area
void SeisPlot::drawLabel(char *label, long frame, int x, int y,
                         Boolean is_horizontal,
                         Boolean auto_position)
{
SeisScrWin  *scrwin= _widget_manager->scrolledWindow();

  if(imageIsDisplayed())
    {
    _image.drawLabel(label, frame, x, y, is_horizontal, auto_position);
    if (scrwin) scrwin->redrawAnnotation();
    }
}

/*
 * ====================== Public Method ==============================
 *  Scan file left or right
 */
int SeisPlot::scan(ScanDir dir)
{
   int sdir = -1;
   char errstr[400];
   long stat = PlotImage::ReadFail;
   ShellWatch watch = _image._graphic;
   SeisScrWin  *scrwin = _widget_manager->scrolledWindow ();
   PickWatch pw;


  if (usingSelector() && _select == NULL) {
    SLErrorPop *errpop = new SLErrorPop (_widget_manager->W(),
      "Error", "Scanning not supported while using trace selector option");
    return PlotImage::ReadFail;
  }

  if (isPlotDisplayed()) {
    _widget_manager->setCurrentSP (this);
    _do_abort->setNewAction ();
    if       (dir==Left)   sdir= PlotImage::ScanLeft;
    else if  (dir==Right)  sdir= PlotImage::ScanRight;
    else deliverError ("Invalid direction to scan", ErrorHandler::ProgError);

    if ((dir == Left) || (dir==Right)) {
      _inform_list->callPreScan (this, dir); // resets _num_traces to zero
                                             // in trace_selector.cc

      delPixmaps ();
      stat = _image.imageScan (sdir, (int)_origin_scale,
        (int)_scan_a_screen);
      _last_pstat = (int)stat;

      if (stat != PlotImage::PlotSuccess) {
	reestablishTraces ();
	long mystat = _image.imageScan (0, (int)_origin_scale,
          (int)_scan_a_screen);
      }

      //Scan back to original plot if user aborted
      if (stat == PlotImage::UserAbort) {
	_do_abort->actionComplete ();
	if (sdir == PlotImage::ScanLeft) sdir = PlotImage::ScanRight;
	else                             sdir = PlotImage::ScanLeft;
	stat = _image.imageScan (sdir, (int)_origin_scale,
          (int)_scan_a_screen);
	_last_pstat= (int)stat;
      }

      addPixmaps ();

      if (scrwin) {
	scrwin->redrawAnnotation ();
      }

      if (stat==PlotImage::ReadFail) dir = NotChanged;
      if ((stat == PlotImage::PlotWarning)||
	  (stat == PlotImage::PlotSuccess)||
	  (stat == PlotImage::ReadFail)     ) {
	_inform_list->callPostScan (this, dir);
      }
      else if (stat == PlotImage::ResourceFail) {
	cleanup ();
      }
      strcpy (errstr,_image.getErrorString());
      if (stat!=PlotImage::PlotSuccess) {
	deliverError (errstr, ErrorHandler::Error);
	SLErrorPop *errpop = new SLErrorPop (_widget_manager->W(),
          "Notice", errstr);
      }
    }

    _do_abort->actionComplete ();
  } // end if
  return stat;
}

/*
 * ====================== Public Method ==============================
 *  Movie left or right
 */
void SeisPlot::movie(int dir)
{

   if ((dir == Left) || (dir==Right)) {
   }

}

/*
 * ====================== Public Method ==============================
 *  Movie to a specific frame
 */
void SeisPlot::movieToFrame(int frame, MovieDir change_type)
{
SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
int x,y,width,height;

  if (isPlotDisplayed()) {
      _widget_manager->setCurrentSP(this);
      _inform_list->callPreMovie(this, change_type);

      if (frame <= _image._frames) {
            //If backing store is not set we can just movie the visible
            //area since scrolling will expose and redraw the portion
            //that is not visible when the user wants to see that area.
            if(!hasBackingStore()) {
                getVisibleArea(&x, &y, &width, &height);
                if(width > 0 && height > 0)
                     _image.imageMovie(frame,  x,  y, width, height);
              }
            else {
                _image.imageMovie(frame, 0,0,
                                PlotImage::ImageAll,PlotImage::ImageAll );
            }
	    displayPixmap();
	    if (scrwin) scrwin->redrawAnnotation();
      }
      else
	   deliverError("SeisPlot::movieToFrame: Invalid frame specified",
			 ErrorHandler::ProgError);

      _inform_list->callPostMovie(this, change_type);
  } // end if
}


/*
 * ====================== Public Method ==============================
 *  Zoom Up
 */
void SeisPlot::zoomUp()
{
 InformSeisZoomer *dozoom;
 int ztype;

  if (isPlotDisplayed()) {
     _widget_manager->setCurrentSP(this);
     _widget_manager->lock("Display locked for zooming.");
     if (_zoom_type == Box)     ztype= SpArea;
     if (_zoom_type == Pointer) ztype= SpPointer;

     dozoom= new InformSeisZoomer(this, _zoom_factor, ztype);

     if (_zoom_type == Box) dozoom->setPixelSize(_box_size);

     _inform_list->callPreZoom(this, dozoom, SeisPlot::Up);
     //delPixmaps(); We want SeisZoomer to do this because that class
     //              knows of all multiple file SeisPlots. MLS 05/2002
     dozoom->startZoom();
  } // end if


}



/*
 * ====================== Public Method ==============================
 *  Zoom Down
 */
void SeisPlot::zoomDown()
{
  ShellWatch watch= _image._graphic;
  PickWatch pw;

  if (isPlotDisplayed()) {
       _do_abort->setNewAction();
       _widget_manager->setCurrentSP(this);
       _do_abort->setNewAction();
       _inform_list->callPreZoom(this, NULL, SeisPlot::Down);
       //delPixmaps(); We want SeisZoomer to do this because that class
       //              knows of all multiple file SeisPlots. MLS 05/2002
       InformSeisZoomer(this).zoomDown();
       _do_abort->actionComplete();
  } // end if

}


/*
 * ====================== Public Method ==============================
 *  Return to orginal size
 */
void SeisPlot::originalSize()
{
  ShellWatch watch= _image._graphic;
  PickWatch pw;

  if (isPlotDisplayed()) {
    _do_abort->setNewAction();
    _inform_list->callPreZoom(this, NULL, SeisPlot::Orginal);
    //delPixmaps(); We want SeisZoomer to do this because that class
    //              knows of all multiple file SeisPlots. MLS 05/2002
    InformSeisZoomer(this).zoomOriginal();
    _do_abort->actionComplete();
  } // end if


}


/*
 * ====================== Public Method ==============================
 *  Create and zoom in separate window with pan buttons
 */
SeisPlotZoom *SeisPlot::zoomUpSeparateWin()
{
  SeisPlotZoom      *spz  = NULL;
  SeisPlotZoomUnder *spzu = NULL;
  
  SeisPanner   *spp;
  SeisMovie *zoom_movie;

  if (isPlotDisplayed()) {
    _do_abort->setNewAction();
    _widget_manager->setCurrentSP(this);

    spp = new SeisPanner(_widget_manager->W(), "panner", NULL, movie(),
                         getHelpCtx());
    spz = new SeisPlotZoom(spp->W(),  "zplot",  this);
    if (_image._chain_image != NULL) {
      // This SeisPlot has an underlay
      spzu = new SeisPlotZoomUnder (spz);
    }
    spz->_spp = spp;
    spp->setPanImage (spz, spzu);
    XtVaSetValues( spz->W(),  XmNtopAttachment,    XmATTACH_FORM,
			      XmNrightAttachment,  XmATTACH_FORM,
			      XmNleftAttachment,   XmATTACH_WIDGET,
			      XmNleftWidget,       spp->panW(),
			      XmNbottomAttachment, XmATTACH_WIDGET,
			      XmNbottomWidget,     spp->sepW(),  NULL);
    XtVaSetValues( spp->W(),  XmNwidth,740,        XmNheight,650,NULL);


    if( movie() )
       {
       zoom_movie= new SeisMovie( spp->W(), "zoom_movie", spz, _hctx);
       XtVaSetValues( zoom_movie->W(), XmNbottomAttachment, XmATTACH_FORM,
				       XmNleftAttachment,   XmATTACH_FORM,
				       XmNbottomOffset,     1,
				       XmNleftOffset,       1,
				       NULL );

       }

    spz->zoomUp();

    if( movie() )
       {
       spp->addMovie(zoom_movie);
       zoom_movie->setRange( 1, (int)spz->frames() );
       zoom_movie->manage();
       }


  } // end if

  _do_abort->actionComplete();

  return(spz);
}

Boolean SeisPlot::inbounds(XExposeEvent *ev, PlotImage *image)
{
  Boolean retval= False;
  if ( (ev->x + ev->width > image->_left_border &&
        ev->x < image->_graph_width + image->_left_border) ||
       (ev->y + ev->height > image->_top_border &&
        ev->y < image->_graph_height + image->_top_border))
                          retval= True;
  return retval;
}


/*
 *
 ********************************************************************
 *              Do all exposures
 ********************************************************************
 */
void SeisPlot::expose(Widget,
		      XtPointer,
		      XmDrawingAreaCallbackStruct *CBdata)
{

  XExposeEvent *ev;
  ev= (XExposeEvent *)CBdata->event;


  if (!store_events(ev) ) {
    if (inbounds(ev, &_image)) {
	  if ( isPlotDisplayed() ) {
		 redraw( ev->x, ev->y, ev->width, ev->height);
	  } // end if
	  else if (_image._chain_image) {
	         if ( _chained_sp.top()->isPlotDisplayed() ) {
		          redraw( ev->x, ev->y, ev->width, ev->height);
                 }
          }
	  else if (_image._chain_image == NULL) {
                 clear();
	  } // end else
    } // end if
  } // end if store_events
}


void SeisPlot::redraw(  long  x,
			long  y,
			long  width,
			long  height)
{
  if (_widget_manager->currentSP() == this) {

    // perhaps a full refresh is too much if only a ColorInfo changed consider
    //   using:
    //long frame = _image.getCurrentPixmapIndex ();
    //recolorFrame (frame);
    //return;

     SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
     Boolean chained_displayed= _chained_sp.top() ?
                        (Boolean)_chained_sp.top()->isPlotDisplayed() : False;
     if (isPlotDisplayed() || chained_displayed )
         if (_redraw_action == Redraw) {
	   _image.refresh( x, y, width, height);
	   _color_info_changed = FALSE;
	 }
	 else
	   clear();
     else if (imageGraphic()) {
          Widget w= imageGraphic();
          if (XtWindow(w))  {
                XClearWindow( XtDisplay(w), XtWindow(w) );
                if (scrwin) {
                     scrwin->clearAnnotation();
                     scrwin->resetCornerFillColor();
                }
          }
     }

     if (   scrwin
        && (width  == PlotImage::ImageAll)
        && (height == PlotImage::ImageAll)
        && isPlotDisplayed() )    scrwin->redrawAnnotation();


     if (width  == PlotImage::ImageAll) x=0, width  = _image._graph_width;
     if (height == PlotImage::ImageAll) y=0, height = _image._graph_height;

     if (isPlotDisplayed() && _redraw_action == Redraw) {
	    _inform_list->callExpose(this,(int)x,(int)y,(int)width,(int)height);
     } // end if
  } // end if
}



void SeisPlot::clear()
{
  Widget w= imageGraphic();
  if (XtWindow(w))
   {
   XClearWindow( XtDisplay(w), XtWindow(w) );
   _widget_manager->scrolledWindow()->clearAnnotation();
   }
}

void SeisPlot::redrawToWidget(  Widget w,
				long  x,
				long  y,
				long  width,
				long  height,
				long  destx,
				long  desty)
{
 _image.refreshMain(x, y, width, height, destx, desty, True, w);
}


void SeisPlot::setRedrawAction(RedrawType redraw)
{
  _redraw_action= redraw;
}



void SeisPlot::deliverError(char *errstr, int etype)
{
  if (_normal_error_processing) {
     ErrorHandler err= _image._graphic;
     err.setEtype(ErrorHandler::Warning,   ErrorHandler::GUI, _show_warnings);
     err.setEtype(ErrorHandler::Error,     ErrorHandler::GUI, _show_errors);
     err.setEtype(ErrorHandler::ProgError, ErrorHandler::CUI, _show_prog_err);
     err.deliverError(errstr, etype);
  }
  strcpy(_last_errstr,errstr);
}

/*
 *
 ********************************************************************
 *              Adding and removing Inform classes
 ********************************************************************
 */
void SeisPlot::addInformer(SeisInform *p)
{
  _inform_list->add(p);
}


void SeisPlot::delInformer(SeisInform *p)
{
  _inform_list->remove(p);
}



/*
 *
 ********************************************************************
 ********************************************************************
 ********************                            ********************
 ********************     set Type Methods       ********************
 ********************                            ********************
 ********************************************************************
 ********************************************************************
 *
 */

/*
 * ====================== Public Method ==============================
 *  sets new file name and reads globals
 */
Boolean SeisPlot::setFilename(char *infile)
{
   int istat;
   Boolean succ =False;
   char    fname[300];
   SLErrorPop *warning;

   //struct stat file_stats;
   time_t mod_time;

   if (infile) {
      NetEnv::expTilde( _netenv, fname, infile);

      if (strcmp(fname,_user->_filename) == 0)  {
          mod_time= NetEnv::modTime(_netenv, fname);
	  if (mod_time != 0) {
	       if (mod_time != _file_mod_time) {
		    _file_mod_time= mod_time;
	       } // end if
	       else {
		    return _may_plot;
	       } // end else
	  } // end if stat
      } // end if strcmp

      if(strlen(_user->_filename))
         strcpy(_previous_filename, _user->_filename);
      strcpy( _user->_filename, fname);
      _goto_disk= True;
      _new_file= True;

      if (NetEnv::access(_netenv, fname, F_OK|R_OK)==0) {
         mod_time= NetEnv::modTime(_netenv, fname);
	 if (mod_time != 0) {
	    _file_mod_time= mod_time;
	 } // end if
	 if (plotType() == PlotImage::PlotISO) {
	      succ= False;
	      deliverError(
              "SeisPlot::setFilename:failed-mode cannot be PlotImage::PlotISO",
		  ErrorHandler::ProgError);
	      _may_plot= True;
	 }
	 else {
	    NetEnv::tfioGetGlobalData( _netenv, fname, &_temp_G, &istat);

	    if(istat == 0 || istat == MAYBE_INCOMPLETE_FILE) {
		_maxtime=  (_temp_G.ndptr - 1) * _temp_G.srval +
                                                   _temp_G.tstrt;
		_tmin = _mintime = _temp_G.tstrt;
		_tmax = _maxtime;
		succ= True;
		_may_plot= True;

                if (tf_global_is3d(&_temp_G)) {
                        // do any 3d checking here
                }
                else {
                       _image._Cl.index= 0;
                       _image._Cl.axis= 0;
                }
                if(istat == MAYBE_INCOMPLETE_FILE)
                   warning = new SLErrorPop(_widget_manager->W(),
                                   "Warning",
                                   "File may not be complete");
	    } //end if
	    else {
		succ= False;
		_may_plot= False;
		deliverError(
		   "SeisPlot::setFilename: Could not find global file",
		   ErrorHandler::ProgError);
	    } //end else
	 } // end else
      } //end if
      else {
	  deliverError("SeisPlot::setFilename: Could not find file",
				ErrorHandler::ProgError);
      }
   }
   else {
      _user->_filename[0]= '\0';
      _may_plot= False;
   }
   return (succ);
}

void SeisPlot::resetPreviousFilename()
{
  if(fileHasNeverBeenPlotted() && strlen(_previous_filename))
    {
    strcpy(_user->_filename,_previous_filename);
    setFilename(_previous_filename);
    }
}

Boolean SeisPlot::fileHasNeverBeenPlotted()
{
  return (_new_file);
}

/*
 * ====================== Public Method ==============================
 *  set type of plot (wiggle, wiggle file, color, semblance, etc)
 */
void SeisPlot::setPlotType(int mode)
{
  int oldmode= (int)_user->_mode;
  _user->_mode= mode;
  if ( (mode == PlotImage::PlotCOLOR) || (mode == PlotImage::PlotCONTOUR) ||
       (mode == PlotImage::PlotSEMB)  || (mode == PlotImage::PlotISO) ) {
	if (!(_color_flags & MAY_COL)) {
	   deliverError(
	   "SeisPlot::setPlotType: Cannot plot this type of plot without color",
	   ErrorHandler::ProgError);
	   _user->_mode = PlotImage::PlotWFILL;
	   mode = (int)PlotImage::PlotWFILL;
	} // end if
  } // end if
  if ( ((oldmode == PlotImage::PlotGRID) ||
       (oldmode == PlotImage::PlotHEADER)) &&
       (mode    !=PlotImage:: PlotGRID) &&
       (strlen(_user->_filename) > 0) ) {
	    char tmp_filename[300];
	    strcpy(tmp_filename, _user->_filename);
	    _user->_filename[0]= '\0';
	    setFilename(tmp_filename);
  } // end if

  isDiff(oldmode,_user->_mode);


//set the appropriate mouse read out type
  switch(_user->_mode)
    {
    case PlotImage::PlotWONLY:
    case PlotImage::PlotWFILL:
    case PlotImage::PlotCOLOR:
    case PlotImage::PlotGRID:
    case PlotImage::PlotHEADER:
       _image.setMouseReadoutType(PlotImage::MOUSE_AMP);
       break;

    case PlotImage::PlotCONTOUR:
    case PlotImage::PlotSEMB:
    case PlotImage::PlotISO:
       _image.setMouseReadoutType(PlotImage:: MOUSE_VEL);
       break;
    }//end switch

  _inform_list->callPlotTypeChange(this,mode,oldmode);
}


/*
 * ====================== Public Method ==============================
 *  is color posible
 */
Boolean SeisPlot::colorCapable()
{
  long visclass;
  long cflags;
  Boolean retval;

  cflags= test_vis( XtDisplay( _image._graphic), &visclass, NULL);

  if (cflags & MAY_COL) retval= True;
  else                  retval= False;

  return retval;
}


static char *NoShareMsg =
   "SeisPlot::shareColorsWith: Circular sharing loop; No color will occur.\n";

/*
 * ====================== Public Method ==============================
 *  use another SeisPlot to get the colors for this SeisPlot instance
 */
void SeisPlot::shareColorsWith (SeisPlot *sp)
{
  if (ColorsetCollection::readOnly(_col)) {
    shareReadOnlyColorsWith (sp);
  }
  else {
    shareDynamicColorsWith (sp);
  }
}

void SeisPlot::shareReadOnlyColorsWith (SeisPlot *sp)
{
  shareDynamicColorsWith (sp);
}

void SeisPlot::shareDynamicColorsWith(SeisPlot *sp)
{
  SeisPlot *q;
  int i;
  Boolean realloc_col= False;
  Boolean i_allocated;
  long    new_planes;
  int    new_cnum;

  if ((_color_share_sp == sp) && (_color_share_sp)) return;

  ColorInfoSet *set = ColorInfoCollection::fetchExisting (_col);
  int role;

  if ((sp!=this)&&(sp)) {
       for(i=0, q=sp; ((i<1000)&&(q)); i++, q=q->_color_share_sp);
       if (i>=999)
	   printf( NoShareMsg);
       else {
	   i_allocated= _color_share_sp ? False : True;
	   if (_color_share_sp) {
	     // remember the role
	     role = set->role (_col_segment);
	     // it was shared by somebody else before, undo it
	     set->removeSharedElement (_col_segment);
             _color_share_sp->_cshares.remove(this);
	   }
	   else {
	     // remember the role
	     role = set->role ();
	   }

	   _color_share_sp= sp;
	   if (_col->numplanes > _color_share_sp->_col->numplanes) {
		     new_planes= _col->numplanes;
		     realloc_col= True;
	   }
	   else {
		     new_planes= _color_share_sp->_col->numplanes;
	   }
	   if (_col->cnum > _color_share_sp->_col->cnum) {
		     new_cnum= _col->cnum;
		     realloc_col= True;
	   }
	   else {
		     new_cnum= _color_share_sp->_col->cnum;
	   }

	   if (i_allocated) {
	        // before this call, I had allocated the colors myself
	        freeColors();

		// eliminate any vestage of _col's existance
		ColorInfoCollection::remove (_col);
		ColorsetCollection::remove  (_col);

		delete _col;
		delete _sc;

	   }

	   _col= _color_share_sp->_col;

	   // get the ColorInfoSet associated with the new ColorInfo
	   ColorInfoSet *set = ColorInfoCollection::fetchExisting (_col);

	   // add this program segment to the shared element list
	   set->addSharedElement (_col_segment, role);

	   // synchronize with the PlotImage
	   _image.setColorInfoStructure (_col);

	   _sc=  _color_share_sp->_sc;
	   _sc->addSP(this);
	   _color_share_sp->_cshares.add(this);

	   /**/
	   if (realloc_col) {
	       _color_share_sp->freeColors();
	       _color_share_sp->_col->numplanes= new_planes;
	       _color_share_sp->setLoadableColors(new_cnum);
	   }
	   else {
	       set->update (_col);
	       _inform_list->callColorChange(_col->cnum,COLOR);
	   }
       } // End else

  }
  if (!sp) {
       if (_color_share_sp) {
	      // eliminate the sharing

	      // remember the role
	      role = set->role (_col_segment);
	      set->removeSharedElement (_col_segment);

	      _col= new ColorInfo;

	      set = ColorInfoCollection::fetch (_col, _col_segment, role);
	      ColorsetCollection::fetch (_col);

	      _col->numplanes= _multi_plane ?
			       _color_share_sp->_col->numplanes : 0;
	      _col->cmap=      _color_share_sp->_col->cmap;
	      _col->cnum=      _color_share_sp->_col->cnum;
	      _col->colorsafe= False;
	      _col->shared= _color_share_sp->_col->shared;
	      if (_col->cnum >1)
		       setLoadableColors(_col->cnum);
	      _color_share_sp->_sc->removeSP(this);
	      _sc=  new SeisColor(this,_col);
	      _sc->setPredef( (int)_color_share_sp->_sc->predef() );
              _color_share_sp->_cshares.remove(this);
	      _color_share_sp= NULL;

	      // synchronize with the PlotImage
	      _image.setColorInfoStructure (_col);
       } // end if
       _color_share_sp= NULL;
  } // end if
}


void SeisPlot::takeOverSharingFrom(SeisPlot *sp)
{
  SeisPlot *q;
  void *vp= NULL;

  for(q=sp->_cshares.top(&vp); (q); q= sp->_cshares.top(&vp)) {
        if (q != this) _cshares.add(q);
        sp->_cshares.remove(q);
  }
  _cshares.add(sp);
  sp->_color_share_sp= this;
  _color_share_sp= NULL;

  for(q=_cshares.top(&vp); (q); q= _cshares.next(&vp)) {
        q->_color_share_sp= this;
  }

}


/*
 * ====================== Public Method ==============================
 *  set predefined color bar
 */
void SeisPlot::setPreDefColor(int c)
{
  _sc->setPredef(c);
}

/*
 * ====================== Public Method ==============================
 *  get predefined color bar
 */
int SeisPlot::getPreDefColor()
{
  return _sc->predef();
}

/*
 * ====================== Public Method ==============================
 *  set number of loadable colors
 */
Boolean SeisPlot::setLoadableColors(int n)
{
Boolean retval= False;

  if (n>0) {
     if (_col->colorsafe) freeColors();
     _col->cnum= n;
     retval= haveColors();
     if (retval) _sc->setPredef((int)_sc->predef());
  }

  return retval;
}


/*
 * ====================== Public Method ==============================
 *  Free loadable colors
 */
void SeisPlot::freeLoadableColors()
{

  if(_col->colorsafe)
    {
    freeColors();
    _col->cnum= 0;
    }

}

/*
 * ====================== Public Method ==============================
 *  set number of contour colors
 */
Boolean SeisPlot::setContourColors (int n)
{
  if (_col_contour.colorsafe) {
    ColorsetCollection::clear (&_col_contour);
/*
  Display *dpy= XtDisplay( _image._graphic);
  XFreeColors(dpy, _col_contour.cmap, _col_contour.pix,
			  _col_contour.cnum, _col_contour.numplanes);
*/
    _inform_list->callColorChange (0, CONT);
  }
  _col_contour.cnum = n;
  return (haveContourColors());
}


/*
 * ====================== Public Method ==============================
 *  Free contour colors
 */
void SeisPlot::freeContourColors()
{
  if (_col_contour.colorsafe) {
    ColorsetCollection::clear (&_col_contour);
/*
    XFreeColors(_widget_manager->display(), _col_contour.cmap,
                _col_contour.pix,
  	        _col_contour.cnum, _col_contour.numplanes);
*/
    _inform_list->callColorChange (0,CONT);
  }
}



/*
 * ====================== Public Method ==============================
 *  set number of contour colors
 */
void SeisPlot::setPlotDescription(char *s)
{
  if (_description) free(_description);
  _description= newstr(s);
}

/*
 * ====================== Public Method ==============================
 *  set English or Metric
 */
void SeisPlot::setUnits(const int u)
{
 _user->_metric= u;
 _inform_list->callUnitChange(this, u);
}


/*
 * ====================== Public Method ==============================
 *  set plot height and width in inch or cm
 */
void SeisPlot::setPlotSize( float width, float height)
{
  setGridWidth(width);
  setGridHeight(height);
}


/*
 * ====================== Public Method ==============================
 *  set plot width in inch or cm
 */
void SeisPlot::setGridWidth( float width)
{
  _plot_width= width;
}


/*
 * ====================== Public Method ==============================
 *  set plot height in inch or cm
 */
void SeisPlot::setGridHeight( float height)
{
  _plot_height= height;
}


/*
 * ====================== Public Method ==============================
 *  set plot inch per second or cm per second
 */
void SeisPlot::setIS(float is)
{
  _y_unit_per_inch = is;
}

/*
 * ====================== Public Method ==============================
 *  set plot traces per inch or traces per cm
 */
void SeisPlot::setTI(float ti)
{
  _x_unit_per_inch = ti;
}

/*
 * ====================== Public Method ==============================
 * set axis when working with a cube
 */
void SeisPlot::set3dAxis(int axis)
{
  isDiff((long)axis, (long)_3d_axis);
  _3d_axis= axis;
}

/*
 * ====================== Public Method ==============================
 * set slice when working with a cube
 */
void SeisPlot::set3dSlice(int slice)
{
  isDiff((long)slice, (long)_3d_slice);
  _3d_slice= slice;
}

/*
 * ====================== Public Method ==============================
 *  for subclasses- notifies base class that the subclass is
 *  sharing things such as gc, fonts, etc with some other seisplot
 */
void SeisPlot::setSharingXResources()
{
 _sharing_x_resources= True;
}

/*
 * ====================== Public Method ==============================
 *  set backing store on or off
 */
void SeisPlot::backingStore(Boolean doit)
{
  Widget w= imageGraphic();
  SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
  if (w) {
    Window win= XtWindow(w);
    if (win) {
	XSetWindowAttributes wattr;
	Screen *scr= XtScreen(w);
	unsigned long flags= 0;
	if (doit) {
	     if (DoesBackingStore(scr)) {
		  wattr.backing_store= WhenMapped;
		  flags|= CWBackingStore;
                  setBS(True);
                  _inform_list->calBackingStoreChange(this,True);
             }
	}
	else {
	     wattr.backing_store= NotUseful;
	     wattr.save_under=    False;
	     flags= CWBackingStore|CWSaveUnder;
             setBS(False);
             _inform_list->calBackingStoreChange(this,False);
	}
	XChangeWindowAttributes( XtDisplay(w), win, flags, &wattr );
    }
    else {
     printf("SeisPlot::backingStore cannot be changed until after realize.\n");
    }
  }
  if (scrwin) scrwin->backingStore(doit);
}

/*
 * ====================== Public Method ==============================
 *  set the Widget for Seisplot to output plotting messages to
 */
void SeisPlot::setMessageWidget(Widget w)
{
  _image.setStatusWidget(w);
}

/*
 * ====================== Public Method ==============================
 *  the scroll bar on right if true left if false
 */
void SeisPlot::setVerticalScrollBarOnRight(Boolean doit)
{
 SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
 if (doit) scrwin->setVertPlacement(SLScrollWin::Right);
 else      scrwin->setVertPlacement(SLScrollWin::Left);
}

/*
 * ====================== Public Method ==============================
 *  return the maximum number of wiggle traces per inch
 */
int SeisPlot::maxWigglesPerInch()
{
  Display *dpy= XtDisplay(imageGraphic());
  Screen  *scr= XtScreen(imageGraphic());
 int retval= _image.horizontalPixelsPerInch(dpy,
                       XScreenNumberOfScreen(scr)  ) / 2;

  return retval;
  //return 1;
}


/*
 * ====================== Public Method ==============================
 *  set the Widget for Seisplot to output mode messages to
 *  also set the string that will be outputed
 */
void SeisPlot::setModeWidget(Widget mode, char *mode_str)
{
  _mode_widget= mode;
  if (_mode_str) free (_mode_str);
  _mode_str= newstr(mode_str);
}


/*
 * ====================== Public Method ==============================
 *  user requested show annotation at all times or not
 */
void SeisPlot::showAnnotation(Boolean show)
{
 SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
 if (!_anno_override_app_request) {
      if (scrwin) scrwin->showAnnotation(show);
 }
 _app_wants_persistent_anno= show;
}

/*
 * ====================== Methods ==============================
 *  override user reqested show annotation
 */
void SeisPlot::showAnnotationOverride(Boolean show)
{
 SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
 _anno_override_app_request= True;
 if (scrwin) scrwin->showAnnotation(show);
}

/*
 * ====================== Methods ==============================
 *  return to the user requested annoation
 */
void SeisPlot::showAnnotationRequested()
{
 SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
 if (scrwin) scrwin->showAnnotation(_app_wants_persistent_anno);
 _anno_override_app_request= False;
}


/*
 * ====================== Public Method ==============================
 *  which border to show for scrolling
 */
void SeisPlot::showBorders(Boolean l, Boolean r, Boolean t, Boolean b)
{
  SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
  _show_left= l;
  _show_right= r;
  _show_top= t;
  _show_bottom= b;
  if (scrwin) {
      if (_show_left)   scrwin->setLeftBorder( (int)leftBorder() );
      else              scrwin->setLeftBorder( 0 );
      if (_show_right)  scrwin->setRightBorder( (int)rightBorder() );
      else              scrwin->setRightBorder( 0 );
      if (_show_top)    scrwin->setTopBorder( (int)topBorder() );
      else              scrwin->setTopBorder( 0 );
      if (_show_bottom) scrwin->setBottomBorder( (int)bottomBorder() );
      else              scrwin->setBottomBorder( 0 );
  }
}

/*
 * ====================== Public Method ==============================
 * Find traces based on up to 3 header words
 */
long SeisPlot::findTraces(char *filename,       int primary_header,
                          float primary_min,    float primary_max,
                          float primary_inc,    int secondary_header,
                          float secondary_min,  float secondary_max,
                          float secondary_inc,   int tertiary_header,
                          float tertiary_min,   float tertiary_max,
                          float tertiary_inc,   long nplt,
                          long iskp,            long ndo,
                          long nskp,            long num_frames,
                          int pixmap)
{
long found;

  found = _image.findTraces(filename,         primary_header,
                            primary_min,      primary_max,
                            primary_inc,      secondary_header,
                            secondary_min,    secondary_max,
                            secondary_inc,    tertiary_header,
                            tertiary_min,     tertiary_max,
                            tertiary_inc,     nplt,
                            iskp,             ndo,
                            nskp,             num_frames,
                            pixmap);

  return found;
}

/*
 * Find traces based on NDoTraceSelection
 */
long SeisPlot::findTraces (char *filename,       NDoTraceSelection *select,
                           long nplt,            long iskp,
			   long ndo,             long nskp,
			   long num_frames,      int pixmap)
{
long found;

  found = _image.findTraces (filename,         select,
                             totalTraces(),    nplt,
			     iskp,             ndo,
			     nskp,             num_frames,
			     pixmap);

  return found;
}

long SeisPlot::reestablishTraces ()
{
  long found;
  if (_select != NULL) {
    found = _image.findTraces (filename(), _select, totalTraces(), nplt(), 0,
      1, 0, _image._frames, 0);
  }
  else {
    found = nplt ();
  }
  return found;
}

Boolean SeisPlot::isRegularized (NDoTraceSelection *select)
{
  Boolean retval = _image.isRegularized (filename(), select, totalTraces());
  return retval;
}

Boolean SeisPlot::matches (NDoTraceSelection *select)
{
  Boolean retval = _image.matches (filename(), select, totalTraces());
  return retval;
}

long SeisPlot::findTracesByPattern(char *filename,     int xheader,
                                   int yheader,        long num_traces,
                                   long *num_gathers,  int *user_aborted,
                                   float *xloc,        float *yloc)
{
long found;

  found = _image.findTracesByPattern(filename,      xheader,
                                     yheader,       num_traces,
                                     num_gathers,   user_aborted,
                                     xloc,          yloc);

  return found;
}


/*
 * ====================== Public Method ==============================
 *  Set the trace selector array
 */
int SeisPlot::setTraceSelectorTrace(int pixmap, long trace)
{
  return _image.setTraceSelectorTrace(pixmap, trace);
}

/*
 * ====================== Public Method ==============================
 *  Get the number of traces in a pixmap
 */
long SeisPlot::getSelectorNumTraces(int pixmap)
{
  return _image.getSelectorNumTraces(pixmap);
}


long SeisPlot::displayedTraces(int pixmap)
{
  if(_image.usingSelector())
    return getSelectorNumTraces(pixmap);
  else
    return _image.getNumberDisplayedTraces();
}


/*
 * ====================== Public Method ==============================
 *  re-annotate an existing image without re-reading data
 */
void SeisPlot::reAnnotate()
{
  _image.reAnnotate();
}


/*
 * ====================== Public Method ==============================
 *  Sets annotation of grid type images to be symetrical
 */
Boolean SeisPlot::setSymetricalAnnotation(float x1,float x2,float y1,float y2)
{
float xrange, yrange;
float anno_interval, fraction;
//float x_increment;
double anno_log;
long templog;


  if(!getWidget())
   {
   _user->_symetrical_anno = False;
   return(False);
   }

  xrange = x2 - x1;
  if(xrange < 0.0) xrange = (-xrange);
  yrange = y2 - y1;
  if(yrange < 0.0) yrange = (-yrange);
  if(xrange == 0.0 || yrange == 0.0)
    {
    _user->_symetrical_anno = False;
    return(False);
    }

  //Set the y interval
  anno_log = log10( yrange );
  templog = (long)anno_log;
  if( (float)templog > anno_log) templog = templog - 1;
  fraction = anno_log - templog;
  if(fraction < 0.23)
    {
    templog = templog - 1;
    anno_interval = 2.0 * (pow(10.0, (float)templog));
    }
  else if (fraction < 0.57)
    {
    templog = templog - 1;
    anno_interval = 5.0 * (pow(10.0, (float)templog));
    }
  else if (fraction < 0.90)
    {
    anno_interval = pow(10.0, (float)templog);
    }
  else
    {
    anno_interval = 2.0 *(pow(10.0, (float)templog));
    }
  if(anno_interval < 0.0) anno_interval = (-anno_interval);
  setTimingLines(anno_interval,anno_interval);
  //End setting y interval


  //Set the x info, the real work here is done in libimage
  _user->_label_by_headers = False;

/*The following would label the x by its header values close to the y interval.
  Implement later, will need to compute pixels numx
  x_increment = xrange / (float)(numx - 1.0);
  anno_interval /=  x_increment;
  sp->setLblInc((long)anno_interval);
*/

  _user->_symetrical_anno = True;
  return(True);

}

/*
 * ====================== Public Method ==============================
 * Returns the inches of the width or height
 * required to make a plot symetrical
 * ===================================================================
 */
float SeisPlot::getSymetricalSize(Boolean get_xsize, float x1, float x2,
				  float y1, float y2,float *other_side_size,
				  Boolean limit_screen_size)
{
long hpix_per_inch;
long vpix_per_inch;
float xperpix, yperpix;
long numpix;
float new_side_size;
float screen_width_inches;
float screen_height_inches;
float reduction_factor;
float use_one_inch = 1.0;


  if(!getWidget()) return(0.0);

  if(y1 == y2 || x1 == x2) return( use_one_inch );

  screen_width_inches = (float)(DisplayWidthMM(XtDisplay(getWidget()),
				DefaultScreen(XtDisplay(getWidget())))) * .0394;
  screen_height_inches= (float)(DisplayHeightMM(XtDisplay(getWidget()),
				DefaultScreen(XtDisplay(getWidget())))) * .0394;
  hpix_per_inch =  _image.horizontalPixelsPerInch(XtDisplay(getWidget()),
				DefaultScreen(XtDisplay(getWidget())));
  vpix_per_inch =  _image.verticalPixelsPerInch(XtDisplay(getWidget()),
				DefaultScreen(XtDisplay(getWidget())));


  if(get_xsize)
    {
    numpix = (long)(*other_side_size  * (float)vpix_per_inch );
    yperpix = (max(y2,y1) - min(y2,y1)) / (float)numpix;
    xperpix = (max(x2,x1) - min(x2,x1)) / yperpix;
    new_side_size = xperpix / (float)hpix_per_inch;
    if(new_side_size > screen_width_inches && limit_screen_size)
      {
      reduction_factor = screen_width_inches / new_side_size;
      new_side_size = screen_width_inches;
      *other_side_size *= reduction_factor;
      }
    if(new_side_size == 0.0) new_side_size = use_one_inch;
    return( new_side_size );
    }
  else
    {
    numpix = (long)(*other_side_size  * (float)hpix_per_inch );
    xperpix = (max(x2,x1) - min(x2,x1)) / (float)numpix;
    yperpix = (max(y2,y1) - min(y2,y1)) / xperpix;
    new_side_size = yperpix / (float)vpix_per_inch;
    if(new_side_size > screen_height_inches && limit_screen_size)
      {
      reduction_factor = screen_height_inches / new_side_size;
      new_side_size = screen_height_inches;
      *other_side_size *= reduction_factor;
      }
    if(new_side_size < 1.0) new_side_size = use_one_inch;
    return( new_side_size );

    }


}




/*
 * ====================== Public Method ==============================
 *  replot a zoomed image without re-reading data or re-allocating memory
 */
void SeisPlot::replotZoom()
{
 long stat, first_trace, numtraces;
 int retval,x1,x2,y1,y2;
 static long width, height;
 long lborder = _image._left_border;
 long tborder = _image._top_border;
 double old_factor;


  _widget_manager->setCurrentSP(this);
  old_factor = _image._zoom_factor;
  if(_image._zoom_factor > 1.0) // first time in with this image
     {
     width = _image.getXimageWidth();
     height= _image.getXimageHeight();
     _image._zoom_factor = 1.0;
     }
  _image._zoom_factor = 1.0;
  numtraces = (int)(_image._zoomxary[_image._zindex][1]
	    - _image._zoomxary[_image._zindex][0] + 1.0);


  if(plotType() != PlotImage::PlotGRID && plotType() != PlotImage::PlotHEADER)
     {
     first_trace= _image.getFirstTraceInImage();
     if(first_trace + numtraces - 1 > _image.getTracesInMemory())
	first_trace = _image.getTracesInMemory() - numtraces + 1;
     x1 = (int)lborder + int( (first_trace-1) * _image.getTraceWidth() );
     x2 = x1 + int( (numtraces-1) * _image.getTraceWidth());
     }
  else
     {
     x1 = int( lborder + (_image.getX1() * _image.getXvaluePerPixel()));
     x2 = x1 + (int)width;
     }
  y1 = (int)tborder;
  y2 = (int)(height + tborder);


  updateEnable();
  _inform_list->callPrePlot(this);
  stat= _image.processZoom(PlotImage::Z_UP,x1,y1,x2,y2,True);
  _image._zoom_factor = old_factor;
  retval= checkPlotStatus((int)stat);


}

/*
 *
 ********************************************************************
 ********************************************************************
 ********************                            ********************
 ********************    Methods for Hardcopy    ********************
 ********************                            ********************
 ********************************************************************
 ********************************************************************
 *
 */
void SeisPlot::computePaneledHardWidthHeight(float   seismic_width,
                                             float   seismic_height,
                                             float  *tot_width,
                                             float  *tot_height,
                                             Boolean left_cbar)
{
  float width,height;
  *tot_width= 0;
  *tot_height= 0;
  for(int i=0; (i<plottedFrames()); i++) {
           computeHardWidthHeight(seismic_width, seismic_height,
                                  &width, &height, left_cbar, i+1, True);
           *tot_width+=  width;
  }
  *tot_height= height;
}



void SeisPlot::computeHardWidthHeight(float   seismic_width,
                                      float   seismic_height,
                                      float  *tot_width,
                                      float  *tot_height,
                                      Boolean left_cbar,
                                      int     frame,
                                      Boolean panel)

{
  float width,height;
  Boolean lcbar= False, rcbar= False;

  lcbar= SeisHardCopy::doColorBarForFrame(this, left_cbar, panel, frame,
                                           SeisHardCopy::OnLeft);
  rcbar= SeisHardCopy::doColorBarForFrame(this, left_cbar, panel, frame,
                                           SeisHardCopy::OnRight);

  width=  seismic_width +  HardCopyPlot::defaultLeftBorderWidth() +
                           HardCopyPlot::defaultRightBorderWidth();
  if (lcbar) width+= 2;
  if (rcbar) width+= 2;
  height= seismic_height + HardCopyPlot::defaultTopBorderHeight() +
                           HardCopyPlot::defaultBottomBorderHeight();
  *tot_width= width;
  *tot_height= height;
}




/*
 * ====================== Public Method ==============================
 *  make hardcopy plot
 */
int SeisPlot::writeHardCopy(SeisHardCopy *shc)
{
  int stat;
  _inform_list->callPreWriteHardCopy(this);
  stat=  shc->writePlot(this);
  _inform_list->callPostWriteHardCopy(this);
  return stat;
}

/*
 * ====================== Public Method ==============================
 *  draw the color bar - 3 options: draw it, don't, or draw a square
 */
void SeisPlot::drawColorBarOnHardCopy(HardCbarType which_type)
{
 _cbar_on_hardcopy= which_type;
}


/*
 *
 ********************************************************************
 ********************************************************************
 ********************                            ********************
 ******************** Methods for Transformation ********************
 ********************                            ********************
 ********************************************************************
 ********************************************************************
 *
 */
/*
 * ====================== Public Method ==============================
 *  get a X pixel from a x value
 */
long SeisPlot::getTraceFromPixel(long pixel)
{

  return ( (long) (_image.getXfromXpixel((int)pixel) + 0.5) );

}

/*
 * ====================== Public Method ==============================
 *  get a amplitude value from the data array
 *  May want to make this more general accepting byte data in the
 *  future.
 */
float SeisPlot::getZvalueFromPixelXY(int x, int y)
{
double xval;
double zval;

  assert(_image._float_array != NULL);

  xval= (x - _image._left_border) * _image._x_value_per_pixel + _image._grid_x1;

  _image.parseArrayValue(y, _image._top_border, xval, &zval,
                         _image._nhdrs, _image._original_samples,
                         _image._hd, _image._float_array);
  return (float)zval;
}
/*
 * ====================== Public Method ==============================
 *  get a X pixel from a x value
 */
short SeisPlot::xPixel(float x)
{
float newx;
int xpixel;

  newx= _transform ? _transform->convDataToXOffset(this,x) : x;

  xpixel =  manualTransformX() ? _image.getManualXpixelFromX(newx)
                               : _image.getXpixelFromX(newx);
  xpixel = min(xpixel,SHRT_MAX);
  xpixel = max(xpixel,SHRT_MIN);
  return((short)xpixel);
}

/*
 * ====================== Public Method ==============================
 *  get a Y pixel from a y value
 */
short SeisPlot::yPixel(float y)
{
float newy;
int ypixel;

  newy= _transform ? _transform->convDataToYOffset(this,y) : y;
  ypixel =  manualTransformY() ? _image.getManualYpixelFromY(newy)
                               : _image.getYpixelFromY(newy);
  ypixel = min(ypixel,SHRT_MAX);
  ypixel = max(ypixel,SHRT_MIN);
  return((short)ypixel);
}

/*
 * ====================== Public Method ==============================
 *  get a trace or x value from a pixel
 */
float SeisPlot::xWC(int x)
{
  float newx;
  newx = manualTransformX() ? (float)_image.getManualXfromXpixel(x)
                            : (float)_image.getXfromXpixel(x);
  return _transform ? _transform->convXOffsetToData(this,newx) : newx;
}

/*
 * ====================== Public Method ==============================
 *  get a time or other y value from a pixel
 */
float SeisPlot::yWC(int y)
{
  float newy;
  newy = manualTransformY() ? (float)_image.getManualYfromYpixel(y)
                            : (float)_image.getYfromYpixel(y);
  return _transform ? _transform->convYOffsetToData(this,newy) : newy;
}

/*
 *
 ********************************************************************
 ********************************************************************
 ************************               *****************************
 ************************  Misc Methods *****************************
 ************************               *****************************
 ********************************************************************
 ********************************************************************
 *
 */

/*
 *====================== Public Method ==============================
 *  return true if a plot is actually dipslayed
 */
long SeisPlot::isPlotDisplayed()
{
   return _image._displayed;
}

/*
 *====================== Public Method ==============================
 *  return annotation font structure
 */
XFontStruct *SeisPlot::annoFont()
{
  return _image._font_fixed;
}

/*
 *====================== Public Method ==============================
 *  return bold annotation font structure
 */
XFontStruct *SeisPlot::annoBoldFont()
{
  return _image._font_bold;
}

/*
 *====================== Public Method ==============================
 *  determine if this type of file allows to scale the data to the
 *  whole file.  We can do this with byt and tfiles but not
 *  with seg files.
 */
Boolean SeisPlot::canScaleToFile()
{
  Boolean retval= True;
  if (strstr(_temp_G.ftyp, "SEGY") || _temp_G.trmaxg == 0.0)
    // || strstr(_temp_G.ftyp, "TROT") ) retval= False;
    retval = False;
  return retval;
}

/*
 *====================== Public Method ==============================
 *  Place application value in the amplitude readout
 */
void SeisPlot::setLocationOutputType(int mouse_type, char *label, double value)
{
  _image.setMouseReadoutType(mouse_type);
  _image._aux_label = label;
  _image._aux_value = value;
  if (_image._xydisp->zloc)
        wprocVAShowMsg( _image._xydisp->zloc, "%s %6.3f", label, value);
}

/*
 *====================== Public Method ==============================
 *  set the Widgets and turn on the xy readout
 */
void SeisPlot::setLocationOutput(Widget xloc, Widget yloc, Widget aout)
{
  _widget_manager->setLocationOutput(this,xloc,yloc,aout);
}

void SeisPlot::removeLocationOutput()
{
  _widget_manager->removeLocationOutput(this);
}

void SeisPlot::setLocationOutputXLabel(char *xstr)
{
  if (_image._xydisp->override_x_str) free(_image._xydisp->override_x_str);
  _image._xydisp->override_x_str= newstr(xstr);
}

void SeisPlot::setLocationOutputYLabel(char *ystr)
{
  if (_image._xydisp->override_y_str) free(_image._xydisp->override_y_str);
  _image._xydisp->override_y_str= newstr(ystr);
}

void SeisPlot::setXReadoutHeader(int h)
                                {_image._xydisp->setXReadoutHeader(h);}
int  SeisPlot::getXReadoutHeader()
                                {return _image._xydisp->getXReadoutHeader();}
void SeisPlot::setAltYReadoutHeader(int h)
                                {_image._xydisp->setAltYReadoutHeader(h);}
int  SeisPlot::getAltYReadoutHeader()
                                {return _image._xydisp->getAltYReadoutHeader();}


/*
 * ====================== Public Method ==============================
 *  return a copy of the plot label string
 */
char *SeisPlot::plotLabel()
{
  return _user->_plotLabel;
}

/*
 * ====================== Public Method ==============================
 *  set the plot label string
 */
void SeisPlot::setPlotLabel(char *label, Boolean label_only,
                            Boolean draw_now, int x, int y)
{
if (!label) label= "";
int labellen= strlen(label);
int old_labellen = strlen(plotLabel());
int old_x = _user->_plotlabel_x;
int old_y = _user->_plotlabel_y;
int rect_x, rect_y, rect_xsize, rect_ysize;

  if(labellen)
    {
    strncpy(_user->_plotLabel,label,labellen);
    _user->_plotLabel[labellen]= '\0';
    }
  else
    {
    _user->_plotLabel[0]= '\0';
    return;
    }

  _user->_plotlabel_x = x;
  _user->_plotlabel_y = y;


//wait for plot creation to draw label
  if(label_only == True && draw_now == False)
    {
    _user->_plotlabel_only = label_only;
    _user->_annotate = True;
    }

//draw label on existing plot
  if(draw_now)
    {
    assert(imageIsDisplayed());
    labellen = strlen(plotLabel());

    if(old_x == 0 && old_y == 0)
      {
      rect_x = (int)max(0,(int)leftBorder() - _image._boldcharwidth);
      rect_y = (int)max(0, imageHeight() + topBorder());
      }
    else
      {
      rect_x = (int)max(0, old_x - _image._boldcharwidth);
      rect_y = (int)max(0, old_y - _image._boldcharheight);
      }
    rect_xsize = (int)((old_labellen + 1) * _image._boldcharwidth);
    rect_ysize = (int)(2 * _image._boldcharheight);
    //blank out any pre-existing label
    XSetForeground(XtDisplay(imageGraphic()), _image._gc1, _image._white_pixel);
    XSetBackground(XtDisplay(imageGraphic()), _image._gc1, _image._black_pixel);
    XFillRectangle(XtDisplay(imageGraphic()), imagePixmap((int)currentFrame()),
                   _image._gc1, rect_x, rect_y, rect_xsize, rect_ysize);
    XSetForeground(XtDisplay(imageGraphic()), _image._gc1, _image._black_pixel);
    XSetBackground(XtDisplay(imageGraphic()), _image._gc1, _image._white_pixel);
    redraw(rect_x, rect_y, rect_xsize, rect_ysize);
    if(x == 0 && y == 0)//default, put both labels at bottom left
      {
      x = (int)(leftBorder() + .5 * _image._boldcharwidth);
      y = (int)(imageHeight() + topBorder() + _image._boldcharheight);
      if(canOverlay()) y += (int)_image._boldcharheight;
      }
    XDrawString(XtDisplay(imageGraphic()),imagePixmap((int)currentFrame()),
                _image._gc2, x, y, plotLabel(), labellen);
    rect_x = (int)max(0, x - _image._boldcharwidth);
    rect_y = (int)max(0, y - _image._boldcharheight);
    rect_xsize = (int)((labellen + 1) * _image._boldcharwidth);
    rect_ysize = (int)(2 * _image._boldcharheight);
    redraw(rect_x, rect_y, rect_xsize, rect_ysize);
    }



}

//====================================================================
//======= Add an extra x axis label to the hardcopy ==================
//====================================================================
void SeisPlot:: setExtraXHardcopyAnnotation(Boolean draw_it, char *label)
{
  _draw_extra_x_label = draw_it;
  if(draw_it)
    strcpy(_extra_x_label, label);
  else
    strcpy(_extra_x_label, "");
}

//====================================================================
//======= Add an extra y axis label to the hardcopy ==================
//====================================================================
void SeisPlot:: setExtraYHardcopyAnnotation(Boolean draw_it, char *label)
{
  _draw_extra_y_label = draw_it;
  if(draw_it)
    strcpy(_extra_y_label, label);
  else
    strcpy(_extra_y_label, "");
}

//====================================================================
//======= Get an extra x axis label to the hardcopy ==================
//====================================================================
Boolean SeisPlot::getExtraXHardcopyAnnotation(char *label)
{
  if(_draw_extra_x_label)
    {
    strcpy(label, _extra_x_label);
    return True;
    }
  else
    {
    return False;
    }
}

//====================================================================
//======= Get an extra y axis label to the hardcopy ==================
//====================================================================
Boolean SeisPlot::getExtraYHardcopyAnnotation(char *label)
{
  if(_draw_extra_y_label)
    {
    strcpy(label, _extra_y_label);
    return True;
    }
  else
    {
    return False;
    }
}


/*
 * ====================== Public Method ==============================
 *  return a copy of the last error string
 */
char *SeisPlot::lastError()
{
  return newstr(_last_errstr);
}

/*
 * ====================== Public Method ==============================
 *  set the netenv pointer pointer for getter date remotely
 */
void SeisPlot::setNetEnv(NetEnv *netenv)
{
   _netenv= netenv;
   _image._netenv= netenv;
}

/*
 * ====================== Public Method ==============================
 *  return the netenv pointer pointer for getter date remotely
 */
NetEnv *SeisPlot::netEnv()
{
  return _netenv;
}

/*
 * ====================== Public Method ==============================
 *  return the pointer to this SeisPlots SeisWinMan
 *  this is used for multiple SeisPlots sharing one window
 */
SeisWinMan *SeisPlot::getSeisWinMan()
{
  return _widget_manager;
}

/*
 * ====================== Public Method ==============================
 *  return the pointer to this SeisPlot's chained SeisPlot
 *  the chained SeisPlot is usually an underlay SeisPlot
 */
SeisPlot *SeisPlot::getChainedSP()
{
  return _chained_sp.top();
}

void SeisPlot::removeChainedSP (SeisPlot *sp)
{
  if (_image._chain_image != NULL) {
// this was added to remove freed memory purify errors after deleting
//   a SeisPlotUnder object followed by the delete of its pertinent sp
//   object.  the code was grabbed from the imageFree fnc
//   this is extremely poor OOP design and should be revised in any
//   serious attempt to object orientate the sp and image libraries
    if (!_image._chain_image->_underlay_only) {
      _image._chain_image->imageFree ();
      _image._chain_image = NULL;
    }
  }
  _chained_sp.remove (sp);
}

void SeisPlot::addHorizons (HorizonsManager *manager,
  HorizonsVectStyle *style)
{
  assert (manager && style);
  _hpicks = new HorizonsPicks (manager, this, style);
}

/*
 * ====================== Public Method ==============================
 *  get the first displayed sample of a trace
 */
long SeisPlot::firstSampleOfTrace()
{

 return (long)((_image._zoomyary[_image._zindex][0]-memTmin())
                / (srval() * tdec()) + 0.5);

}


/*
 * ====================== Public Method ==============================
 *  get the index of the first trace in an image which may be zoomed
 *  or plotted right to left
 */
long SeisPlot::firstTraceIndex()
{

  if(rToL())
     return (long)_image._zoomxary[_image._zindex][1] - 1;
  else
     return (long)_image._zoomxary[_image._zindex][0] - 1;
}

/*
 * ====================== Public Method ==============================
 *  get a trace header value from a pixel
 */
float SeisPlot::xHeader(int x, int header)
{
  return (float)_image.getHeaderFromTrace( (int)(xWC(x) + .5),header);
}


/*
 * ====================== Public Method ==============================
 *  get a trace header value from a trace number and header number
 */
float SeisPlot::getHeaderFromTrace(int trace_number, int header_number)
{
  return (float)_image.getHeaderFromTrace(trace_number,header_number);
}


/*
 * ====================== Public Method ==============================
 *  get closest trace from a trace header value
 */
float SeisPlot::getTraceFromHeader(int header_number, float header_value)
{
int pixel;
float trace;

 pixel = (int)_image.getPixelFromHeader(header_value,  header_number);
 if(pixel < 0) return -1;

 trace = manualTransformX() ? (float)_image.getManualXfromXpixel(pixel)
                            : (float)_image.getXfromXpixel(pixel);
 return  trace + .5;
}


/*
 * ====================== Public Method ==============================
 *  get closest X pixel from a trace header value
 */
short SeisPlot::headerPixel(int header_number, float header_value)
{
  return (short)_image.getPixelFromHeader(header_value,  header_number);
}

/*
 * ====================== Public Method ==============================
 *  get a y sample index into memory trace data array from a y pixel
 *  Note you must add previous traces * nsamp to the index returned
 */
long SeisPlot::ySampleNumFromPixel(int ypixel)
{
 float time;

  time = _image.getTimeFromYpixel(ypixel);
  return( _image.getSampleIndexFromTime(time) );

}

/*
 * ====================== Public Method ==============================
 *  get x and y pixels from a trace number and sample
 */
void SeisPlot::xyFromTraceAndSample(float trace, long sample, int *x, int *y)
{
 float time;

  time  = _image.getTimeFromSampleIndex(sample);
  *y    = _image.getYpixelFromTime(time);
  *x    = _image.getXpixelFromX(trace);

}


/*
 * ====================== Public Method ==============================
 *  get the area with just data and not annotation
 */
void SeisPlot::getClipArea(int *x, int *y, int *width, int *height)
{
  *x= (int)leftBorder();
  *y= (int)topBorder();
  *width=  (int)(plottedWidth()  - leftBorder() - rightBorder());
  *height= (int)(plottedHeight() - topBorder() - bottomBorder());
}

/*
 * ====================== Public Method ==============================
 *  get the drawing area widget
 */
Widget SeisPlot::getWidget()
{
  Widget w= NULL;
  if (_widget_manager) w= _widget_manager->drawingArea();
  return w;
}

/*
 * ====================== Public Methods ==============================
 *  for setting and getting external amplitude for plot
 */
double SeisPlot::externalAmp()
{
  return  _user->_external_amplitude;
}
void SeisPlot::setExternalAmp(double amp)
{
  _goto_disk= True;
  _user->_external_amplitude = amp;
}

/*
 * ====================== Public Method ==============================
 *  mkae the ti & is make the traces size to a screen full
 */
void SeisPlot::maximizeScale(int /*plot_type*/)
{
  _user->_ti= _x_unit_per_inch;
  _user->_is= _y_unit_per_inch;
  _user->_tmin= _tmin;
  _user->_tmax= _tmax;
  _image.maximize();
  _x_unit_per_inch= _user->_ti;
  _y_unit_per_inch= _user->_is;
}

/*
 * ====================== Public Method ==============================
 *  get the visable area from the scrolled window
 *  this overides the PlotBase virtual function
 */
void SeisPlot::getVisibleArea(int *x, int *y, int *width, int *height)
{
  SeisScrWin  *scrwin= _widget_manager->scrolledWindow();
  if (scrwin) scrwin->getVisibleArea(x,y,width,height);
  else {
     *x= 0;
     *y= 0;
     *width= 0;
     *height= 0;
  }
}
/*
 * ============== Public Methods- called from scroll window  ===========
 */
void SeisPlot::callVisibleAreaChange(int          x,
                                     int          y,
                                     unsigned int width,
                                     unsigned int height)
{
  if (_inform_list)
      _inform_list->callVisibleAreaChange(this, x,y,(int)width,(int)height);
}


void SeisPlot::callStartingDragScroll()
{
  if (_inform_list)
      _inform_list->callStartingDragScroll(this);
}

void SeisPlot::callEndingDragScroll()
{
  if (_inform_list)
      _inform_list->callEndingDragScroll(this);
}

/*
 *
 ********************************************************************
 ********************************************************************
 ************************               *****************************
 ************************  Grid Methods *****************************
 ************************               *****************************
 ********************************************************************
 ********************************************************************
 *
 */

//void SeisPlot::setImageGridX1(float g)
//{
// I_set_x1(&_image, g);
// setGridX1(g);
//}
//void SeisPlot::setImageGridX2(float g)
//{
// I_set_x2(&_image, g);
// setGridX2(g);
//}
//void SeisPlot::setImageGridY1(float g)
//{
// I_set_y1(&_image, g);
// setGridY1(g);
//}
//void SeisPlot::setImageGridY2(float g)
//{
// I_set_y2(&_image, g);
// setGridY2(g);
//}

/*
 * ====================== Public Methods ==============================
 *  set x1, x2, y1, y2
 */
void  SeisPlot::setGridX1(float g)   {_grid_x1= g;}
void  SeisPlot::setGridX2(float g)   {_grid_x2= g;}
void  SeisPlot::setGridY1(float g)   {_grid_y1= g;}
void  SeisPlot::setGridY2(float g)   {_grid_y2= g;}


/*
 * ====================== Public Method ==============================
 *  set x1, x2, y1, y2 in one call
 */
void  SeisPlot::setGridXYS(float x1, float x2, float y1, float y2,
                           Boolean do_manual_annotation)
{
  setGridX1(x1);
  setGridY1(y1);
  setGridX2(x2);
  setGridY2(y2);

  if(do_manual_annotation)
    {
    setManualX1(x1);
    setManualX2(x2);
    setManualY1(y1);
    setManualY2(y2);
    setManualTransform(True);
    setSecTimingLine(0.0);
    }
}


/*
 *
 ********************************************************************
 ********************************************************************
 ************************                         *******************
 ************************ Methods for Array plots *******************
 ************************                         *******************
 ********************************************************************
 ********************************************************************
 *
 */
/*
 * ====================== Public Method ==============================
 *  initialize an array type plot
 */
void SeisPlot::initArrayTypePlot()
{

  _image._filedata = False;
  setUnits(PlotEnglish);
  if ( _plot_width > 0)  setGridWidth( _plot_width);
  else                   setGridWidth( 4.0);
  if ( _plot_height > 0) setGridHeight( _plot_height);
  else                   setGridHeight( 4.0);
  setPrimTimingLine(1.0);
  setSecTimingLine(0.5);
  setLabeling(1,1);
  setHeaders(1,2);
  setMinMaxVel(4000.0, 20000.0);
  setMinColorAmp(4000.0);
  setMaxColorAmp(20000.0);
  setMinVel(4000.0);
  setMaxVel(20000.0);
  setMaxDataAmp(20000.0);
  setNPlt(1);
  initArrayTypeData( 1, 1, 1, 1, NULL);
  setNSkp(0);
  setNdo(1);
  setISkp(0);
  setPlotType(PlotImage::PlotISO);
  setNorm(PlotImage::NORM);
  setTdec(1);
  setGradeVert(True);
  setGradeHorz(True);
  setSrval(0.004);
  setNumDataPoints(8);
  setnumHeaderWords(PlotImage::MAXHDRS);
  setDoColor(True);
  setDoMedian(False);
  setDoPercent(False);
  setDoAmplitude(True);
  if(!_sc->predef()) _sc->setPredef(PlotImage::STANDARD);
  setPlotDescription("IsoVel");
  if(!_user->_num_cbar_cols) _user->_num_cbar_cols = _sc->numcolors();

}


/*
 * ====================== Public Method ==============================
 * Sets image pointers to outside data arrays
 */
void SeisPlot::pointToArrayTypeData(SeisPlot *sp)
{
  if (sp) {
        if (sp->_data_pointed_to_sp != NULL) {
            sp= sp->_data_pointed_to_sp;
        } // end if
        assert(sp->_data_pointed_to_sp == NULL);
        _image._point_to_data  = True;
        _image._point_to_headers = True;
        _image._filedata = False;
        _image._hd = sp->_image._hd;
        _image._float_array = sp-> _image._float_array;

        //next necessary so that the header array allocation size is correct
        _user->_G.nbyhd = sp->_user->_G.nbyhd;
        _user->_G.ntrfil= sp->_user->_G.ntrfil;

        if (_share_data_inform == NULL) {
            _share_data_inform= new ShareDataInform(sp, this);
        } // end if
        else {
            assert(_data_pointed_to_sp);
            if (sp != _data_pointed_to_sp) {
                   assert(_share_data_inform->find(_data_pointed_to_sp));
                   _share_data_inform->delSeisPlot(_data_pointed_to_sp);
                   _share_data_inform->addSeisPlot(sp);
            }
        }
        _data_pointed_to_sp= sp;
  }
  else {
       if (_share_data_inform && _data_pointed_to_sp)
              _share_data_inform->delSeisPlot(_data_pointed_to_sp);
       _data_pointed_to_sp     = NULL;
       _image.setPointToData(False);
       _image.setPointToHeaders(False);
       _image._hd= NULL;
       _image._float_array= NULL;
  }
}

/*
 * ====================== Public Method ==============================
 */
void SeisPlot::setHeader(long header_index, float v)
{
  _image._hd[header_index]= v;
  if (_data_pointed_to_sp) _data_pointed_to_sp->updateEnable();
  updateEnable();
}




/*
 * ====================== Public Method ==============================
 *  Set number of traces in a panel and allocate header array
 *  If array_in is null the float array is allocated and can
 *  be referenced thru the argument list or by method
 *  floatTraceData(). If array_in is not null then seis plot uses
 *  the address of array_in in image generation.
 */
Boolean SeisPlot::initArrayTypeData( int   frame,
                                     long  num_frames,
                                     long  numx,
                                     long  numy,
                                     float *array_in,
				     int   interp_lt_2D)

{

  updateEnable();

  //Initialize some variables to prevent runtime warnings
  strcpy(_user->_G.ftyp,"");
  _user->_G.nbydp = 8;



  if(num_frames < 1) num_frames = 1;
  _user->_nplt = _image._tpnl[frame - 1] = _image._ntot = (int)numx;
  _user->_G.ntrfil = (int)(_user->_nplt * num_frames);
  _user->_G.tstrt  = _grid_y1;
  _image._nsamp    = numy;
  _user->_G.srval  = (_grid_y2 - _grid_y1) /
                                     (float)(max(numy - 1.0,1.0));
  _temp_G.srval = _user->_G.srval;

  _user->_hdrOne   = matchHeader();
  _user->_xlabel_header = matchHeader();

  if(_image._hd == NULL)
     _image._hd=(float *) calloc(1,(int)((numx*PlotImage::MAXHDRS*sizeof(float))
                               *num_frames));
  else
     _image._hd=(float *)realloc(_image._hd,
                               (int)((numx*PlotImage::MAXHDRS*sizeof(float))
                               *num_frames));

  if(array_in == NULL)
     {
     _image._point_to_data = False;
     if(_image._float_array == NULL)
        _image._float_array=(float *) calloc(1,((int)(numx*numy*num_frames
                                           *sizeof(float))));
     else
        _image._float_array=(float *)realloc(_image._float_array,
                                              (int)((numx*numy*num_frames
                                           *sizeof(float))));
     }
  else
    {
    _image._point_to_data = True;
    //if(_image._float_array != NULL) free(_image._float_array);
    _image._float_array = array_in;
    }

  _image._interp_lt_2D = interp_lt_2D;

  if(_image._hd == NULL || _image._float_array == NULL)
     {
     printf("couldnt allocate required arrays in initArrayTypeData\n");
     return(False);
     }
  else
     {
     return(True);
     }
}



/*
 *======================= Public Method ==============================
 *  notify seisplot that the array type data is no longer valid
 */

void SeisPlot::cancelArrayTypeData()
{

  _image._point_to_data = False;
  if(_image._float_array != NULL) free(_image._float_array);
  _image._float_array = NULL;

}
/*
 * ====================== Public Method ==============================
 *  regularize user's data array and store in image's trace array
 *  store user's x locations in image's header array
 */
void SeisPlot::setArrayTypeData(int   frame,
                                long  column,
                                long  num_yvals,
                                float xlocation,
                                float *values,
                                float *ys)
{
 long i;
 long first_data_index;
 long header_index;
 long skip_traces = 0;


  for(i=1;i<frame;i++) skip_traces += _image._tpnl[i-1];
  first_data_index = _image._nsamp * (skip_traces + column);
  header_index     = _image._nhdrs * (skip_traces + column) + matchHeader() - 1;

  setHeader(header_index,xlocation);

  regularize_sampling(ys, values, num_yvals, gridY1(), gridY2(), _image._nsamp,
                     &_image._float_array[first_data_index]);

}


/*
 * ====================== Public Method ==============================
 *  Edit an array type display
 */
long SeisPlot::modArrayTypeImage( long first_column,
                                  long last_column,
                                  long first_sample,
                                  long last_sample,
                                  long pixmap_index,
                                  SeisPlot *overlay_sp)
{
long stat;
long expose_x, expose_y;
long expose_width, expose_height;

 stat = _image.updateImage(first_column, last_column,
                    first_sample, last_sample, pixmap_index,
                    &expose_x, &expose_y, &expose_width, &expose_height);


 if (isPlotDisplayed() && stat)
    _inform_list->callExpose(this,(int)expose_x,(int)expose_y,
                            (int)expose_width,(int)expose_height);

 //If there is an overlay call expose on it so that any picks that exist
 //will be redrawn
 if(overlay_sp)
   {
   if(overlay_sp->isPlotDisplayed())
     overlay_sp->_inform_list->callExpose(overlay_sp,(int)expose_x,
                            (int)expose_y,(int)expose_width,
                            (int)expose_height);
   }

 return stat;
}

//==========================================================================
//=== Update an area of gridded contoured image                          ===
//==========================================================================
int SeisPlot::contourGridderUpdate(long              first_column,
                                   long              last_column,
                                   long              starting_sample,
                                   long              ending_sample,
                                   long              pixmap_index,
                                   SeisPlot          *overlay_sp)
{
int stat;
long expose_x, expose_y;
long expose_width, expose_height;

  stat = _image.contourGridderUpdate(first_column,
                                     last_column,
                                     starting_sample,
                                     ending_sample,
                                     pixmap_index,
                                     &expose_x, &expose_y,
                                     &expose_width, &expose_height);

  if (isPlotDisplayed() && stat)
    _inform_list->callExpose(this,(int)expose_x,(int)expose_y,
                            (int)expose_width,(int)expose_height);


 //If there is an overlay call expose on it so that any picks that exist
 //will be redrawn
 if(overlay_sp)
   {
   if(overlay_sp->isPlotDisplayed())
     overlay_sp->_inform_list->callExpose(overlay_sp,(int)expose_x,
                            (int)expose_y,(int)expose_width,
                            (int)expose_height);
   }

  return stat;

}


//==========================================================================
//=========  Redraw the image marks or mark out of date ====================
//==========================================================================
void SeisPlot::imageMarks(long                 active_func,
                          float                *xloc,
                          long                 num_marks,
                          float                *times,
                          SeisPlot             *overlay_sp)
{
SeisScrWin  *scrwin= _widget_manager->scrolledWindow();

  _image.imageMarks(active_func, xloc, num_marks, times);

  if (isPlotDisplayed())
    {
    _inform_list->callExpose(this,(int)0,(int)0, (int)_image.getGraphWidth(),
                             (int)_image.getGraphHeight() );
    if (scrwin) scrwin->redrawAnnotation();
    }
 //If there is an overlay call expose on it so that any picks that exist
 //will be redrawn
 if(overlay_sp)
   {
   if(overlay_sp->isPlotDisplayed())
     overlay_sp->_inform_list->callExpose(overlay_sp,
                             (int)0,(int)0,
                             (int)overlay_sp->plottedWidth(),
                             (int)overlay_sp->plottedHeight());
   }

}


/*
 * ====================== Public Method ==============================
 *  Get over/under coordinates based on matching headers
 *  may be called from either seisplot
 */
void SeisPlot::getOverUnderXYs(float *over_x1,   float *over_y1,
                               float *over_x2,   float *over_y2,
                               float *under_x1,  float *under_y1,
                               float *under_x2,  float *under_y2)
{

  if (_image._over_image) {
     //get the overlay corners
     if (over_x1)
        *over_x1 = _image._over_image->_hd[
                               (_image._over_image->getFirstTraceInImage()- 1)
                              * _image._over_image->getNumberOfHeaders()
                              + _image._over_image->_user->getMatchXheader()-1];
     if (over_x2)
        *over_x2 = _image._over_image->_hd[
                            ((_image._over_image->getFirstTraceInImage() - 1)
                            + _image._over_image->getNumberDisplayedTraces()-1)
                            * _image._over_image->getNumberOfHeaders()
                            + _image._over_image->_user->getMatchXheader()- 1];
     if (over_y1) *over_y1 = _image._grid_y1;
     if (over_y2) *over_y2 = _image._grid_y2;


     //get the underlay corners
     if (under_x1)
        *under_x1 = _image._hd[(_image.getFirstTraceInImage() - 1)
                              * _image.getNumberOfHeaders()
                              + _image._over_image->_user->getMatchXheader()-1];
     if (under_x2)
        *under_x2 = _image._hd[((_image.getFirstTraceInImage() - 1)
                              + _image.getNumberDisplayedTraces()-1)
                              * _image.getNumberOfHeaders()
                              + _image._over_image->_user->getMatchXheader()-1];
     if (under_y1) *under_y1 = _image._grid_y1;
     if (under_y2) *under_y2 = _image._grid_y2;
  } // end if

  else if (_image._chain_image) {
     //get the overlay corners
     if (over_x1)
         *over_x1 = _image._hd[     (_image.getFirstTraceInImage() - 1)
                                    * _image.getNumberOfHeaders()
                                    + matchHeader() - 1];
     if (over_x2)
         *over_x2 = _image._hd[(( _image.getFirstTraceInImage()- 1)
                                     + _image.getNumberDisplayedTraces()-1)
                                     *  _image.getNumberOfHeaders()
                                     + matchHeader() - 1];
     if (over_y1) *over_y1 = _image._grid_y1;
     if (over_y2) *over_y2 = _image._grid_y2;
     //get the underlay corners
     if (under_x1)
         *under_x1 = _image._chain_image->_hd[
                               (_image._chain_image->getFirstTraceInImage()-1)
                               * _image._chain_image->getNumberOfHeaders()
                               + matchHeader() - 1];
     if (under_x2)
         *under_x2 = _image._chain_image->_hd[
                           ((_image._chain_image->getFirstTraceInImage()-1)
                           + _image._chain_image->getNumberDisplayedTraces()-1)
                           * _image._chain_image->getNumberOfHeaders()
                           + matchHeader() - 1];
     if (under_y1) *under_y1 = _image._grid_y1;
     if (under_y2) *under_y2 = _image._grid_y2;
  } // end if _image._chain_image
  else
     assert(0);
}


/*
 *
 ********************************************************************
 ********************************************************************
 ****************                                      **************
 **************** Methods for getting Traces & Headers **************
 ****************                                      **************
 ********************************************************************
 ********************************************************************
 *
 */
/*
 * ====================== Public Method ==============================
 * The following section returns trace and header data in a read only
 * manner.  The array are too big to practically make copies.
 */
const float *SeisPlot::floatTraceData()
{
  return _image.getFloatArrayForUpdate();
}
const unsigned char *SeisPlot::byteTraceData()
{
  return _image.getByteArrayForUpdate();
}
const unsigned char *SeisPlot::firstDisplayedByteTraceData()
{
  return _image.getDisplayedByteData();
}
const float *SeisPlot::firstDisplayedFloatTraceData()
{
  return _image.getDisplayedFloatData();
}
const float *SeisPlot::firstDisplayedHeaderData()
{
  return _image.getDisplayedHeaderData();
}
const unsigned char *SeisPlot::firstMemoryByteTraceData()
{
  return _image.getMemoryByteData();
}
const float *SeisPlot::firstMemoryFloatTraceData()
{
  return _image.getMemoryFloatData();
}
const float *SeisPlot::firstMemoryHeaderData()
{
  return _image.getMemoryHeaderData();
}
const float *SeisPlot::headers()
{
   return _image.getHeaderArrayForUpdate();
}

/*
 * ====================== Public Method ==============================
 * The following section returns trace and header data.
 * The method should be used if you are planning on modifying the data.
 * The array are too big to practically make copies.
 */
void SeisPlot::updateEnable()
{
 if (!_updating_data) {
     _inform_list->callPreDataChange(this);
 }
 _updating_data= True;
}



float *SeisPlot::floatTraceDataForUpdate()
{
  updateEnable();
  return _image.getFloatArrayForUpdate();
}

unsigned char *SeisPlot::byteTraceDataForUpdate()
{
  updateEnable();
  return _image.getByteArrayForUpdate();
}

unsigned char *SeisPlot::firstDisplayedByteTraceDataForUpdate()
{
  updateEnable();
  return _image.getDisplayedByteData();
}

float *SeisPlot::firstDisplayedFloatTraceDataForUpdate()
{
  updateEnable();
  return _image.getDisplayedFloatData();
}

float *SeisPlot::firstDisplayedHeaderDataForUpdate()
{
  updateEnable();
  return _image.getDisplayedHeaderData();
}


unsigned char *SeisPlot::firstMemoryByteTraceDataForUpdate()
{
  updateEnable();
  return _image.getByteArrayForUpdate();
}

float *SeisPlot::firstMemoryFloatTraceDataForUpdate()
{
  updateEnable();
  return _image.getFloatArrayForUpdate();
}

float *SeisPlot::firstMemoryHeaderDataForUpdate()
{
  updateEnable();
  return _image.getHeaderArrayForUpdate();
}

float *SeisPlot::headersForUpdate()
{
  updateEnable();
  return _image.getHeaderArrayForUpdate();
}

int SeisPlot::allocateVelocityArray( int asize)
{
int stat;

  stat = _image.allocateVelocityArray( asize );
  return stat;
}

long SeisPlot::getByteDefaults(float            *xloc,
                              float            *yloc,
                              long             xloc_hdr,
                              long             yloc_hdr,
                              long             *num_gathers)
{
long stat;

  stat = _image.bytedefs(xloc, yloc, xloc_hdr, yloc_hdr, num_gathers );
  return stat;
}

Boolean SeisPlot::isByteData()
{
//We have 2 tfile formats, the normal from cps is float but the
//files generated by govo are byte
  if(strstr(_user->_G.ftyp,"TFILE") !=0)
    return (_user->_G.nbydp < 4) ? True : False;
  else//All other types (note new cps always return float, even when 8bit)
    return (_user->_G.nbydp < 4 && strstr(_user->_G.ftyp,"BYTE") !=0 ) ? True
                                                                    : False;
}

CubeTrcio *SeisPlot::getCubeTrcio ()
{
  _temp_G.crossline_header = _user->_G.crossline_header;
  _temp_G.inline_header    = _user->_G.inline_header;
  read_data_check_3d (&_user->_G);
  return read_data_check_3d (&_temp_G);
}


void SeisPlot::setSelectorParameters(int   use,
                                     int   primary_header,
                                     float primary_min,
                                     float primary_max,
                                     float primary_inc,
                                     int   secondary_header,
                                     float secondary_min,
                                     float secondary_max,
                                     float secondary_inc,
                                     int   tertiary_header,
                                     float tertiary_min,
                                     float tertiary_max,
                                     float tertiary_inc,
                                     long  traces_found,
                                     long  frames_found,
                                     long  traces_in_last_frame,
                                     long  secondary_search,
                                     long  tertiary_search)
{
  _image.useSelector(use);
  _goto_disk = True;

  if(use)
    {
    _primary_header   = primary_header;
    _primary_min      = primary_min;
    _primary_max      = primary_max;
    _primary_inc      = primary_inc;
    _secondary_header = secondary_header;
    _secondary_min    = secondary_min;
    _secondary_max    = secondary_max;
    _secondary_inc    = secondary_inc;
    _tertiary_header  = tertiary_header;
    _tertiary_min     = tertiary_min;
    _tertiary_max     = tertiary_max;
    _tertiary_inc     = tertiary_inc;
    _traces_found     = traces_found;
    _frames_found     = frames_found;
    _traces_in_last_frame = traces_in_last_frame;
    _secondary_search = secondary_search;
    _tertiary_search  = tertiary_search;
    }

}


void SeisPlot::getSelectorParameters(int   *primary_header,
                                     float *primary_min,
                                     float *primary_max,
                                     float *primary_inc,
                                     int   *secondary_header,
                                     float *secondary_min,
                                     float *secondary_max,
                                     float *secondary_inc,
                                     int   *tertiary_header,
                                     float *tertiary_min,
                                     float *tertiary_max,
                                     float *tertiary_inc,
                                     long  *traces_found,
                                     long  *frames_found,
                                     long  *traces_in_last_frame,
                                     long  *secondary_search,
                                     long  *tertiary_search)
{
  *primary_header  = _primary_header;
  *primary_min     = _primary_min;
  *primary_max     = _primary_max;
  *primary_inc     = _primary_inc;
  *secondary_header= _secondary_header;
  *secondary_min   = _secondary_min;
  *secondary_max   = _secondary_max;
  *secondary_inc   = _secondary_inc;
  *tertiary_header = _tertiary_header;
  *tertiary_min    = _tertiary_min;
  *tertiary_max    = _tertiary_max;
  *tertiary_inc    = _tertiary_inc;
  *traces_found    = _traces_found;
  *frames_found    = _frames_found;
  *traces_in_last_frame = _traces_in_last_frame;
  *secondary_search= _secondary_search;
  *tertiary_search = _tertiary_search;
}


void SeisPlot::setNDoSelectorParameters (int use, NDoTraceSelection *select,
  NDoTraceFind *results)
{
  _image.useNDoSelector (use);
  _goto_disk = True;

  if (use) {
    assert (select != NULL);
    if (_select) _select->set (select);
    else         _select = new NDoTraceSelection (select);
    if (_results) _results->set (results);
    else          _results = new NDoTraceFind (results);
  }
}


void SeisPlot::getSelectorParameters (NDoTraceSelection **select,
  NDoTraceFind **results)
{
  *select  = _select;
  *results = _results;
}


void SeisPlot::resetSelectorNumTraces ()
{
  if (_results) _results->initialize ();
  _image.resetSelectorNumTraces ();
}


void SeisPlot::updateSelector (NDoTraceSelection *select)
{
  if (select != NULL && _select != NULL) {
    _select->setRows (select);
  }
}

void SeisPlot::setDoAbortNewAction()
{
  _do_abort->setNewAction();
}

void SeisPlot::setDoAbortActionComplete()
{
  _do_abort->actionComplete();
}

void SeisPlot::unmanageZoomAbortButton()
{
  _inform_list->callZoomInProgress(this, SeisPlot::Up);
}

#define LAV_HEADER 24

double SeisPlot::findLav (int type)
{
  double retval;

  long offset = currentFrame() * originalTraces();
  const float *hdrs = headers() + numHeaders() * offset;
  double lav = 0;
  int k2;
  switch (type) {
  case NOLAV :
    retval = -1.0;
    break;
  case LAVBYFILE :
    assert (0); // needed for segy but not implemented yet
    if (imageIsDisplayed()) {
      assert (0);
    }
    else {
      retval = -1.0;
    }
    break;
  case LAVBYPANEL :
    if (imageIsDisplayed()) {
      for (k2 = 0, retval = 0; k2 < originalTraces(); k2++) {
	lav = hdrs[k2*numHeaders()+LAV_HEADER];
	if (lav > retval) retval = lav;
      }
    }
    else {
      retval = -1.0;
    }
    break;
  }
  return retval;
}

void SeisPlot::colorInfoChangedImmediately (ColorInfo *col)
{
  if (col == _col || col == &_col_gs || col == &_col_contour) {
    _color_info_changed = TRUE;
    ColorInfoCollection::setLoopService ();
  }
  else {
    int now = 0;
  }
}

void SeisPlot::colorInfoChangedFromApply (ColorInfo *col)
{
  if (_color_info_changed && (col == _col || col == &_col_gs ||
      col == &_col_contour)) {
    redraw ();
  }
  else {
    int now = 0;
  }
}

void SeisPlot::colorInfoChangedFromLoop (ColorInfo *col)
{
  if (_color_info_changed && (col == _col || col == &_col_gs ||
      col == &_col_contour)) {
    if (ColorInfoCollection::useApply() == FALSE) redraw ();
  }
  else {
    int now = 0;
  }
}

void SeisPlot::setColorRequiredPlotFlag (int flag)
{
  _color_required_plot = flag;
}

void SeisPlot::drawNewPixmap ()
{
  if (_chained_sp.top()) {
    if (_chained_sp.top()->_color_required_plot) {
      _chained_sp.top()->plot ();
    }
    else {
      _chained_sp.top()->redraw ();
    }
  }

  if (_color_required_plot) {
    plot ();
  }
  else {
    redraw ();
  }
}

/*
int SeisPlot::recolorFrame (long frame)
{
  long stat;
  int retval=False;
  long ptype= plotType();

  if (!(ptype == PlotImage::PlotWONLY || ptype == PlotImage::PlotWFILL)) {
    // a butchering of SeisPlot::plot () follows:
    if ( !autoAllocColors()) {
      _last_pstat= PlotColorFail;
      return False;
    }

    delPixmaps();
    if (ptype != PlotImage::PlotISO) _do_abort->setNewAction ();
    stat = _image.colorReplotFrame (frame);
    retval = checkPlotStatus ((int)stat);
  }
  return retval;
}
*/

// I'm being defined elsewhere
void SeisPlot::setColInfo (ColorInfo *col)
{
  ColorInfoSet *    set = ColorInfoCollection::fetchExisting ( col);
  ColorInfoSet *loc_set = ColorInfoCollection::fetchExisting (_col);

  // define the local ColorInfo from elsewhere
  memcpy (_col, col, sizeof(ColorInfo));

  if (_col->cnum > 0) {
    // the given ColorInfo is non-trivial so update the local ColorInfoSet
    // notify _col's loc_set that the colors have probably been updated
    loc_set->update (_col);
  }

  // I'm not sure about the need for the following on-going relationship.
  // add the segment associated with col's set as a TWO_WAY communicator
  loc_set->addSharedElement (set->segment(), ColorInfoSet::TWO_WAY);
  // notice, this will not be removeSharedElement until the caller
  //   does a ColorInfoCollection::remove (col) which is beyond our
  //   scope. This may be a problem!!!!!
}

// I'm being defined elsewhere
void SeisPlot::setContColInfo (ColorInfo *col)
{
  ColorInfoSet *    set = ColorInfoCollection::fetchExisting (  col        );
  ColorInfoSet *loc_set = ColorInfoCollection::fetchExisting (&_col_contour);

  // define the local ColorInfo from elsewhere
  memcpy (&_col_contour, col, sizeof(ColorInfo));

  if (_col_contour.cnum > 0) {
    // the given ColorInfo is non-trivial so update the local ColorInfoSet
    // notify _col's loc_set that the colors have probably been updated
    loc_set->update (&_col_contour);
  }

  // I'm not sure about the need for the following on-going relationship.
  // add the segment associated with col's set as a TWO_WAY communicator
  loc_set->addSharedElement (set->segment(), ColorInfoSet::TWO_WAY);
  // notice, this will not be removeSharedElement until the caller
  //   does a ColorInfoCollection::remove (col) which is beyond our
  //   scope. This may be a problem!!!!!
}

void SeisPlot::setCoordinateHeader (int n)
{
  _image._coordinate_header = n;
}
