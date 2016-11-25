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
#ifndef _VA_VECT_COLORS_HH
#define _VA_VECT_COLORS_HH

#include "vect/vector.hh"

class VaVectColors
{
	public:
	
		/*
		 * DEF_COL is non-zero because sometimes markers and lines
		 * are different colors.
		 * AOL is active overlay, this is used for the RMS,
		 * average, or interval velocity of the active function.
		 */
		enum {	DEF_COL = 1, ACT_COL, SEL_COL, REF_COL, CMP_COL,
			AOL_COL    , PIL_COL, NIL_COL, PXL_COL, NXL_COL };
		enum {	REF = 0, CMP, AOL, PIL, NIL, PXL, NXL, SEL,
			NUM_SHOW_FUNC };
		enum {	ISO_SEL, GVS_SEL, SEL_SEL, GEO_SEL };
		/* _show_overlay_markers */
		enum {	OVER_LINE, OVER_BOTH, OVER_MARK };
		/* _show_overlay_active_pick */
		enum {	MARK_ALL , MARK_ACT , MARK_NONE };

		VaVectColors();
		virtual ~VaVectColors();
		void updateVectColors();

		void setShowFunc        (int which, int set);
		int  getShowFunc        (int which         );
		const char *getFuncColor(int which         );

		void setSelectType(int set);
		int  getSelectType(       );

		void setIsoTSOverlay(int set);
		int  getIsoTSOverlay(       );

		void setEnableShowFuncs(int set);
		int  getEnableShowFuncs(       );

		void setDeselectFirst(int set);
		int  getDeselectFirst(       );

		void setShowActiveFunc(int set);
		int  getShowActiveFunc(       );

		void setShowHyper(int set);
		int  getShowHyper(       );

		void setShowOverlayMarkers(int set);
		int  getShowOverlayMarkers(       );

		void setOverlayType(int set);
		int  getOverlayType(       );

		void setShowDopplerMute(int set);
		int  getShowDopplerMute(       );

		void  setDopplerMuteParameter(float set);
		float getDopplerMuteParameter(         );

		void setShowOverlayActiveFunc(int set);
		int  getShowOverlayActiveFunc(       );

		void setShowOverlayActivePick(int set);
		int  getShowOverlayActivePick(       );

		void setCrossHairSize(int  length, int  width);
		void getCrossHairSize(int *length, int *width);
		void setCrossHairVisibility(int set);
		int  getCrossHairVisibility(       );

		void setShowHorizonMarkers(int set);
		int  getShowHorizonMarkers(       );

		void setShowActiveHorizon(int set);
		int  getShowActiveHorizon(       );

		const char *crossHairColor();
		const char *semblanceActiveFuncColor();
		const char *semblanceRbnColor();
		unsigned int semblanceWidth()
			{ return 2; }
		Vector::VectorStyle semblanceLineStyle()
			{ return Vector::SolidLine; }
		Vector::VectorMarker semblanceActiveFuncMarker()
			{ return  Vector::XMarker; }
		unsigned int semblanceActiveFuncMarkerSize()
			{ return 13; }
		unsigned int semblanceActiveFuncMarkerLineWidth()
			{ return 2; }
		const char *semblanceActivePickColor();
		const char *semblanceDefaultPickColor();
		Vector::VectorMarker semblanceOverlayMarker()
			{ return  Vector::FilledSquareMarker; }
		unsigned int semblanceOverlayMarkerSize()
			{ return 5; }
		unsigned int semblanceOverlayMarkerLineWidth()
			{ return 0; }

		const char *cmpActivePickColor();
		const char *cmpDefaultPickColor();
		const char *cmpRbnColor();
		const char *cmpDopplerMuteColor();
		unsigned int cmpWidth()
			{ return 2; }
		Vector::VectorStyle cmpLineStyle()
			{ return Vector::SolidLine; }

		const char *gvsActiveFuncColor();
		const char *gvsDefaultFuncColor();
		const char *gvsActivePickColor();
		const char *gvsDefaultPickColor();
		unsigned int gvsFuncWidth()
			{ return 3; }
		Vector::VectorStyle gvsFuncLineStyle()
			{ return Vector::SolidLine; }
		Vector::VectorMarker gvsFuncMarker()
			{ return  Vector::FilledSquareMarker; }
		unsigned int gvsFuncMarkerSize()
			{ return 9; }
		unsigned int gvsFuncMarkerLineWidth()
			{ return 0; }

		const char *gvsRbnColor();
		Vector::VectorMarker gvsRbnMarker()
			{ return  Vector::XMarker; }
		unsigned int gvsRbnMarkerSize()
			{ return 13; }
		unsigned int gvsRbnMarkerLineWidth()
			{ return 2; }

		/*
		 * Marker size and line width are the same whether iso
		 * is profile (inline or crossline) or grid (timeslice).
		 */
		unsigned int isoMarkerSize()
			{ return 5; }
		unsigned int isoMarkerLineWidth()
			{ return 2; }

		const char *isoProfileColor();
		const char *isoProfileDefaultPickColor();
		const char *isoProfileActivePickColor();
		unsigned int isoProfileWidth()
			{ return 2; }
		Vector::VectorStyle isoProfileLineStyle()
			{ return Vector::SolidLine; }
		Vector::VectorMarker isoProfileMarker()
			{ return  Vector::XMarker; }

		const char *isoGridDefaultColor();
		const char *isoGridActiveColor();
		Vector::VectorMarker isoGridMarker()
			{ return  Vector::FilledSquareMarker; }

		const char *isoRbnColor();
		Vector::VectorMarker isoRbnMarker()
			{ return  Vector::XMarker; }
		unsigned int isoRbnMarkerSize()
			{ return 13; }
		unsigned int isoRbnMarkerLineWidth()
			{ return 2; }

		const char *gridDefaultColor();
		const char *gridActiveColor();
		Vector::VectorMarker gridMarker()
			{ return  Vector::FilledSquareMarker; }
		unsigned int gridMarkerSize()
			{ return 5; }
		unsigned int gridMarkerLineWidth()
			{ return 1; }
		const char *gridRbnColor();
		unsigned int gridRbnWidth()
			{ return 2; }

		const char *crossplotActiveFuncColor();
		const char *crossplotRbnColor();
		unsigned int crossplotWidth()
			{ return 2; }
		const char *crossplotActivePickColor ();
		const char *crossplotDefaultPickColor();
		Vector::VectorMarker crossplotMarker()
			{ return  Vector::FilledSquareMarker; }
		unsigned int crossplotMarkerSize()
			{ return 5; }
		unsigned int crossplotMarkerLineWidth()
			{ return 0; }

		unsigned int defaultHorizonWidth()
			{ return 2; }
		unsigned int activeHorizonWidth()
			{ return 5; }
		unsigned int horizonMarkerSize()
			{ return 7; }
		unsigned int horizonMarkerLineWidth()
			{ return 0; }

	protected:


	private:

		int         _show_func [NUM_SHOW_FUNC]           ;
		const char *_func_color[NUM_SHOW_FUNC]           ;
		int         _select_type                         ;
		int         _iso_ts_overlay                      ;
		int         _enable_show_funcs                   ;
		int         _deselect_first                      ;
		int         _cross_hair_length, _cross_hair_width;
		int         _cross_hair_visibility               ;
		int         _show_active_func                    ;
		int         _show_hyper                          ;
		int         _show_overlay_markers                ;
		int         _overlay_type                        ;
		int         _show_doppler_mute                   ;
		float       _doppler_mute_param                  ;
		int         _show_overlay_active_func            ;
		int         _show_overlay_active_pick            ;
		int         _show_horizon_markers                ;
		int         _show_active_horizon                 ;
};

#endif /* _VA_VECT_COLORS_HH */
