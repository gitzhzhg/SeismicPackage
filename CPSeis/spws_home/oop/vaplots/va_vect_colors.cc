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
// $Id: va_vect_colors.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_picks.hh"
#include "vaplots/va_horizons.hh"
#include "vf/vf_constants.hh"

VaVectColors::VaVectColors()
	: _select_type             (  SEL_SEL),
	  _iso_ts_overlay          (        0),
	  _enable_show_funcs       (        1),
	  _deselect_first          (        0),
	  _cross_hair_length       (       50),
	  _cross_hair_width        (        3),
	  _cross_hair_visibility   (        1),
	  _show_active_func        (        1),
	  _show_hyper              (        1),
	  _show_overlay_markers    (OVER_LINE),
	  _overlay_type            (     VTNM),
	  _show_doppler_mute       (        0),
	  _doppler_mute_param      (     1.7F),
	  _show_overlay_active_func(        0),
	  _show_overlay_active_pick(MARK_NONE),
	  _show_horizon_markers    (OVER_LINE),
	  _show_active_horizon     (        1)
{
	static const char *def_col[NUM_SHOW_FUNC] = {
		"brown"        ,
		"yellow"       ,
		"green"        ,
		"orange"       ,
		"magenta"      ,
		"salmon"       ,
		"cyan"         ,
		"green"        };

	for (int i = 0; i < NUM_SHOW_FUNC; i++)
	{
		if (i == VaVectColors::AOL)
			_show_func[i] = 1;
		else
			_show_func[i] = 0;

		_func_color[i] = def_col[i];
	}
}

VaVectColors::~VaVectColors()
{
	/* do nothing */
}

void VaVectColors::updateVectColors()
{
	VaPicks::broadcastUpdateVectColors();
}

void VaVectColors::setShowFunc(int which, int set)
{
	if (_show_func[which] != set)
	{
		assert((set == 0) || (set == 1));

		_show_func[which] = set;

		VaPicks::broadcastSetShowFunc(which, set);
	}
}

int VaVectColors::getShowFunc(int which)
{
	return _show_func[which];
}

const char *VaVectColors::getFuncColor(int which)
{
	return _func_color[which];
}

void VaVectColors::setSelectType(int set)
{
	if (set != _select_type)
	{
		assert((set >= ISO_SEL) && (set <= GEO_SEL));

		_select_type = set;

		VaPicks::broadcastSetSelectType(set);
	}
}

int VaVectColors::getSelectType()
{
	return _select_type;
}

void VaVectColors::setIsoTSOverlay(int set)
{
	if (set != _iso_ts_overlay)
	{
		_iso_ts_overlay = set;

		VaPicks::broadcastSetIsoTSOverlay(set);
	}
}

int VaVectColors::getIsoTSOverlay()
{
	return _iso_ts_overlay;
}

void VaVectColors::setEnableShowFuncs(int set)
{
	if (set != _enable_show_funcs)
	{
		_enable_show_funcs = set;

		VaPicks::broadcastSetEnableShowFuncs(set);
	}
}

int VaVectColors::getEnableShowFuncs()
{
	return _enable_show_funcs;
}

void VaVectColors::setShowActiveFunc(int set)
{
	if (set != _show_active_func)
	{
		_show_active_func = set;

		VaPicks::broadcastSetShowActiveFunc(set);
	}
}

int VaVectColors::getShowActiveFunc()
{
	return _show_active_func;
}

void VaVectColors::setShowHyper(int set)
{
	if (set != _show_hyper)
	{
		_show_hyper = set;

		VaPicks::broadcastSetShowHyper(set);
	}
}

int VaVectColors::getShowHyper()
{
	return _show_hyper;
}

void VaVectColors::setShowOverlayMarkers(int set)
{
	if (set != _show_overlay_markers)
	{
		_show_overlay_markers = set;

		VaPicks::broadcastSetShowOverlayMarkers(set);
	}
}

int VaVectColors::getShowOverlayMarkers()
{
	return _show_overlay_markers;
}

void VaVectColors::setOverlayType(int set)
{
	assert((set >= VTNM) && (set <= VTDP));

	if (set != _overlay_type)
	{
		_overlay_type = set;

		VaPicks::broadcastSetOverlayType(set);
	}
}

int VaVectColors::getOverlayType()
{
	return _overlay_type;
}

void VaVectColors::setShowDopplerMute(int set)
{
	if (set != _show_doppler_mute)
	{
		_show_doppler_mute = set;

		VaPicks::broadcastSetShowDopplerMute(set);
	}
}

int VaVectColors::getShowDopplerMute()
{
	return _show_doppler_mute;
}

void VaVectColors::setDopplerMuteParameter(float set)
{
	if (set != _doppler_mute_param)
	{
		_doppler_mute_param = set;

		VaPicks::broadcastSetDopplerMuteParameter(set);
	}
}

float VaVectColors::getDopplerMuteParameter()
{
	return _doppler_mute_param;
}

void VaVectColors::setShowOverlayActiveFunc(int set)
{
	if (set != _show_overlay_active_func)
	{
		_show_overlay_active_func = set;

		VaPicks::broadcastSetShowOverlayActiveFunc(set);
	}
}

int VaVectColors::getShowOverlayActiveFunc()
{
	return _show_overlay_active_func;
}

void VaVectColors::setShowOverlayActivePick(int set)
{
	if (set != _show_overlay_active_pick)
	{
		_show_overlay_active_pick = set;

		VaPicks::broadcastSetShowOverlayActivePick(set);
	}
}

int VaVectColors::getShowOverlayActivePick()
{
	return _show_overlay_active_pick;
}

void VaVectColors::setDeselectFirst(int set)
{
	if (set != _deselect_first)
		_deselect_first = set;
}

int VaVectColors::getDeselectFirst()
{
	return _deselect_first;
}

void VaVectColors::setCrossHairSize(int length, int width)
{
	if ((length != _cross_hair_length) || (width != _cross_hair_width))
	{
		_cross_hair_length = length;
		_cross_hair_width  = width ;

		VaPicks::broadcastSetCrossHairSize(length, width);
	}
}

void VaVectColors::getCrossHairSize(int *length, int *width)
{
	*length = _cross_hair_length;
	*width  = _cross_hair_width ;
}

void VaVectColors::setCrossHairVisibility(int set)
{
	if (set != _cross_hair_visibility)
	{
		_cross_hair_visibility = set;

		VaPicks::broadcastSetCrossHairVisibility(set);
	}
}

int VaVectColors::getCrossHairVisibility()
{
	return _cross_hair_visibility;
}

void VaVectColors::setShowHorizonMarkers(int set)
{
	if (set != _show_horizon_markers)
	{
		_show_horizon_markers = set;

		VaHorizons::broadcastSetShowHorizonMarkers(set);
	}
}

int VaVectColors::getShowHorizonMarkers()
{
	return _show_horizon_markers;
}

void VaVectColors::setShowActiveHorizon(int set)
{
	if (set != _show_active_horizon)
	{
		_show_active_horizon = set;

		VaHorizons::broadcastSetShowActiveHorizon(set);
	}
}

int VaVectColors::getShowActiveHorizon()
{
	return _show_active_horizon;
}

/*
 * All RET_STR stuff defined in implementation file because
 * HPOC will not handle it in header file
 */
#define RET_STR(x) static const char *retval = x; return retval;

const char * VaVectColors::crossHairColor()
	{ RET_STR("red") }
const char * VaVectColors::semblanceActiveFuncColor()
	{ RET_STR("white") }
const char * VaVectColors::semblanceRbnColor()
	{ RET_STR("red") }
const char * VaVectColors::semblanceActivePickColor()
	{ RET_STR("red") }
const char * VaVectColors::semblanceDefaultPickColor()
	{ RET_STR("blue") }
const char * VaVectColors::cmpActivePickColor()
	{ RET_STR("red") }
const char * VaVectColors::cmpDefaultPickColor()
	{ RET_STR("blue") }
const char * VaVectColors::cmpRbnColor()
	{ RET_STR("red") }
const char * VaVectColors::cmpDopplerMuteColor()
	{ RET_STR("yellow") }
const char * VaVectColors::gvsActiveFuncColor()
	{ RET_STR("red") }
const char * VaVectColors::gvsDefaultFuncColor()
	{ RET_STR("blue") }
const char * VaVectColors::gvsActivePickColor()
	{ RET_STR("red") }
const char * VaVectColors::gvsDefaultPickColor()
	{ RET_STR("blue") }
const char * VaVectColors::gvsRbnColor()
	{ RET_STR("red") }
const char * VaVectColors::isoProfileColor()
	{ RET_STR("white") }	/* Vector class index 0 */
const char * VaVectColors::isoProfileDefaultPickColor()
	{ RET_STR("blue") }	/* Vector class index 1 */
const char * VaVectColors::isoProfileActivePickColor()
	{ RET_STR("red") }	/* Vector class index 2 */
const char * VaVectColors::isoGridDefaultColor()
	{ RET_STR("black") }	/* Vector class index 3 */
const char * VaVectColors::isoGridActiveColor()
	{ RET_STR("white") }	/* Vector class index 4 */
const char * VaVectColors::isoRbnColor()
	{ RET_STR("red") }
const char * VaVectColors::gridDefaultColor ()
	{ RET_STR("blue") }
const char * VaVectColors::gridActiveColor()
	{ RET_STR("red"  ) }
const char * VaVectColors::gridRbnColor()
	{ RET_STR("red") }

const char * VaVectColors::crossplotActiveFuncColor()
	{ RET_STR("red") }
const char * VaVectColors::crossplotRbnColor()
	{ RET_STR("brown") }
const char * VaVectColors::crossplotActivePickColor()
	{ RET_STR("red") }
const char * VaVectColors::crossplotDefaultPickColor()
	{ RET_STR("blue") }
