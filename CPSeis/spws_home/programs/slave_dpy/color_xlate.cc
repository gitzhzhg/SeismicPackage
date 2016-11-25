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
#include "ipc/color_xlate.hh"
#include "sl/paintset.hh"
#include "sl/colorset_collection.hh"
#include "sl/paintset_collection.hh"
#include "sl/color_info_set.hh"

#include <stdlib.h>

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

ColorTranslate::ColorTranslate(Display *remoteDisplay, Window remoteWindow,
	Display *localDisplay, Window localWindow, int numPixels,
        int numPlanes)
{
	XWindowAttributes attributes;
	int i;

	// initialize color stuff
	_col_segment = new ColorInfoSegment (this,
          ColorInfoSet::COLORTRANSLATE);

	_localDisplay  = localDisplay;
	assert(XGetWindowAttributes(_localDisplay , localWindow , &attributes));
	_localColormap  = attributes.colormap;

	_remoteDisplay = remoteDisplay;
	assert(XGetWindowAttributes(_remoteDisplay, remoteWindow, &attributes));
	_remoteColormap = attributes.colormap;

	_totalPlanes = PlanesOfScreen(attributes.screen);

#ifndef NDEBUG
	_maxPlaneMask = (Pixel) (1 << (_totalPlanes - 1));
#endif

	assert(_planeRemapped = new Bool [_totalPlanes]);
	assert(_transPlane    = new Pixel[_totalPlanes]);

	_totalPixels = 1 << _totalPlanes;
	assert(_totalPixels / 2 == _maxPlaneMask);

	assert(_pixelRemapped = new Bool [_totalPixels]);
	assert(_transPixel    = new Pixel[_totalPixels]);

	if (remoteDisplay == localDisplay && _remoteColormap == _localColormap)
	{
		for (i = 0; i < _totalPixels; i++)
		{
			_pixelRemapped[i] = True;
			_transPixel   [i] = (Pixel) i;
		}

		for (i = 0; i < _totalPlanes; i++)
		{
			_planeRemapped[i] = True;
			_transPlane   [i] = intToMask(i);
		}

		_numPixelsAvail = _numPixels = 0;
		_numPlanesAvail = _numPlanes = 0;
	}
	else
	{
		assert(  numPlanes >= 0 && numPixels >= 0 );
//		assert(!(numPlanes >  0 && numPixels == 0));
		assert(  numPlanes == 0 || numPixels >  0 );

		// assume PseudoColor
		if (numPixels | numPlanes)
		{
			if (numPlanes)
				assert(_planePtr = _availPlane =
					new Pixel[numPlanes]);
			else
				_availPlane = (Pixel *) NULL;

			if (numPixels)
			{
				assert(_pixelPtr = _availPixel =
					new Pixel[numPixels]);

/*
				if (!XAllocColorCells(_localDisplay,
					_localColormap, False, _availPlane,
					numPlanes, _availPixel, numPixels))
				{
					handleError(_ERROR_OUT_OF_COLORS);
				}
*/
				assert (numPlanes <= 1);
				_loc_col = new ColorInfo;
				_loc_col->numplanes = numPlanes;
				_loc_col->cmap      = _localColormap;
				_loc_col->cnum      = numPixels;
				int error = ColorsetCollection::allocate
				  (_loc_col);
			        if (error) {
				  handleError (_ERROR_OUT_OF_COLORS);
				}
				ColorInfoSet *set
                                  = ColorInfoCollection::fetch (_loc_col,
                                  _col_segment);
			}
		}

		for (i = 0; i < _totalPixels; i++)
			_pixelRemapped[i] = False;

		for (i = 0; i < _totalPlanes; i++)
			_planeRemapped[i] = False;

		_numPixelsAvail = _numPixels = numPixels;
		_numPlanesAvail = _numPlanes = numPlanes;
	}
}

ColorTranslate::~ColorTranslate()
{
	delete [] _pixelRemapped;
	delete [] _transPixel;
	delete [] _planeRemapped;
	delete [] _transPlane;

	if (_numPixels | _numPlanes)
	{
		int i;
		Pixel planeMask;

		for (i = 0, planeMask = 0; i < _numPlanes; i++)
			planeMask |= _availPlane[i];

/*
		if (!checkError()) 
			XFreeColors(_localDisplay, _localColormap, _availPixel,
				_numPixels, planeMask);
*/
		// delete color stuff
		if (!checkError()) ColorsetCollection::clear (_loc_col);
		ColorsetCollection ::remove (_loc_col);
		ColorInfoCollection::remove (_loc_col);
		delete _loc_col;

		if (_numPixels)
			delete [] _availPixel;

		if (_numPlanes)
			delete [] _availPlane;
	}

        ColorInfoCollection::remove (_col_segment);
	CheckColorSegments::deleteSegment (_col_segment);
}

Pixel ColorTranslate::translatePixel(Pixel remotePixel)
{
	if (_pixelRemapped[remotePixel])
	{
		/*
		 * Could have switched if to (!_pixelRemapped[remotePixel])
		 * when this block became empty, but I decided not to be
		 * a code meddler.
		 */
	}
	else
	{
		Pixel *allLocalPixels, *allRemotePixels;
		int numPerms = 1 << (_numPlanes - _numPlanesAvail);
		assert(allLocalPixels  = new Pixel[numPerms]);
		assert(allRemotePixels = new Pixel[numPerms]);

		getAllPixels(allLocalPixels, allRemotePixels, remotePixel);

		setRGBs(allLocalPixels, allRemotePixels, numPerms);

		delete [] allLocalPixels;
		delete [] allRemotePixels;
	}

	return(_transPixel[remotePixel]);
}

void ColorTranslate::translatePixels(unsigned char *localPixels,
	unsigned char *remotePixels, int numPixels)
{
	assert(numPixels > 0);

	int numPerms       = 1 << (_numPlanes - _numPlanesAvail);
	int possiblePixels = numPixels * numPerms;
	Pixel *allLocalPixels, *allRemotePixels;
	assert(allLocalPixels  = new Pixel[possiblePixels]);
	assert(allRemotePixels = new Pixel[possiblePixels]);

	int newPixels, i;

	for (i = 0, newPixels = 0; i < numPixels; i++)
	{
		if (_pixelRemapped[*remotePixels])
		{
			*localPixels++ = (unsigned char)
				(_transPixel[*remotePixels++] & 255);
		}
		else
		{
			getAllPixels(allLocalPixels + newPixels,
				allRemotePixels + newPixels,
				(Pixel) *remotePixels);

			newPixels += numPerms;

			*localPixels++ = (unsigned char)
				(_transPixel[*remotePixels++] & 255);
		}
	}

	if (newPixels)
	{
		assert(newPixels <= possiblePixels);
		setRGBs(allLocalPixels, allRemotePixels, newPixels);
	}

	delete [] allLocalPixels;
	delete [] allRemotePixels;
}

Pixel ColorTranslate::translatePlane(Pixel remotePlaneMask)
{
	int remotePlaneIndex = maskToInt(remotePlaneMask);

	if (!_planeRemapped[remotePlaneIndex])
	{
		// Get possibles before adding plane.
		int possiblePixels = (_numPixels - _numPixelsAvail)
			* (1 << (_numPlanes - _numPlanesAvail));

		assert(_numPlanesAvail);
		Pixel localPlaneMask = _transPlane[remotePlaneIndex]
			= *_planePtr++;
		_numPlanesAvail--;
		_planeRemapped[remotePlaneIndex] = True;

		Pixel *allLocalPixels, *allRemotePixels;
		int newPixels, i;

		if (possiblePixels)
		{
			assert(allLocalPixels  = new Pixel[possiblePixels]);
			assert(allRemotePixels = new Pixel[possiblePixels]);
		}

#ifndef NDEBUG
		// Make sure no pixel in new plane is already set.
		for (i = 0; i < _totalPixels; i++)
			assert(!(((Pixel) i & remotePlaneMask)
				&& _pixelRemapped[i]));
#endif

		for (i = 0, newPixels = 0;
			i < _totalPixels && newPixels < possiblePixels;
			i++)
		{
			if (_pixelRemapped[i]
				&& !((Pixel) i & remotePlaneMask))
			{
				Pixel remotePixel = (Pixel) i | remotePlaneMask;
				Pixel localPixel  =
					_transPixel[i] | localPlaneMask;
				_transPixel[remotePixel] = localPixel;
				_pixelRemapped[remotePixel] = True;
				allLocalPixels [newPixels  ] = localPixel ;
				allRemotePixels[newPixels++] = remotePixel;
			}
		}

		assert(newPixels == possiblePixels);


		if (newPixels)
		{
			setRGBs(allLocalPixels, allRemotePixels, newPixels);

			delete [] allLocalPixels ;
			delete [] allRemotePixels;
		}
	}

	return(_transPlane[remotePlaneIndex]);
}

void ColorTranslate::updateRGBs()
{
	int numPixels = (_numPixels - _numPixelsAvail)
		* (1 << (_numPlanes - _numPlanesAvail));

	if (numPixels)
	{
		Pixel *localPixels , *localPixelPtr ;
		assert(localPixelPtr  = localPixels  = new Pixel[numPixels]);
		Pixel *remotePixels, *remotePixelPtr;
		assert(remotePixelPtr = remotePixels = new Pixel[numPixels]);

		int usedPixels, i;

		for (i = 0, usedPixels = 0; usedPixels < numPixels; i++)
			if (_pixelRemapped[i])
			{
				*localPixelPtr++  = _transPixel[i];
				*remotePixelPtr++ = (Pixel) i;
				usedPixels++;
			}

		assert(i <= _totalPixels);

		setRGBs(localPixels, remotePixels, numPixels);

		delete [] localPixels ;
		delete [] remotePixels;
	}
}

Pixel ColorTranslate::intToMask(int i)
{
	assert(i >= 0 && i < _totalPlanes);

	return((Pixel) (1 << i));
}

int ColorTranslate::maskToInt(Pixel mask)
{
	assert(mask > (Pixel) 0 && mask <= _maxPlaneMask);

	int retval, check;
	for (retval = 0, check = (int) mask >> 1;
		check;
		check >>= 1, retval++);

	assert(1 << retval == (int) mask);	// only 1 bit set

	return(retval);
}

void ColorTranslate::setRGBs(Pixel *localPixels, Pixel *remotePixels, int num)
{
/*
	XColor *colors, *colorPtr;
	assert(colors = new XColor[num]);
	int i;

	for (i = 0, colorPtr = colors; i < num; i++, colorPtr++)
	{
		colorPtr->pixel = *remotePixels++;
	}

	XQueryColors(_remoteDisplay, _remoteColormap, colors, num);

	for (i = 0, colorPtr = colors; i < num; i++, colorPtr++)
	{
		colorPtr->pixel = *localPixels++;
		colorPtr->flags = DoRed | DoGreen | DoBlue;
	}

	XStoreColors(_localDisplay, _localColormap, colors, num);

	delete [] colors;
*/
	XColor *colors = (XColor *)malloc (num*sizeof(XColor));
	int i;

	Paintset *remote_paintset = PaintsetCollection::fetchExisting (
          _remoteColormap);

	for (i = 0/*, colorPtr = colors*/; i < num; i++/*, colorPtr++*/)
	{
	        remote_paintset->getXColor ((*remotePixels++), &colors[i]);
	}

	_loc_col->cnum = num;
	ColorsetCollection ::store            (_loc_col, colors);
	ColorInfoCollection::updateEverywhere (_loc_col        );

	for (i = 0; i < num; *localPixels++ = _loc_col->pix[i++]);
}

void ColorTranslate::getAllPixels(Pixel *localPixels, Pixel *remotePixels,
	Pixel remotePixel)
{
	assert(_numPixelsAvail);
	_transPixel   [remotePixel] = *_pixelPtr++;
	_pixelRemapped[remotePixel] = True;
	_numPixelsAvail--;

	localPixels [0] = _transPixel[remotePixel];
	remotePixels[0] =             remotePixel;
	int offset = 1;

	int i;	// Used in debug prints, too.

	for (i = 0; i < _totalPlanes; i++)
		if (_planeRemapped[i])
		{
			Pixel mask = intToMask(i);
			assert(!(remotePixel & mask));	// no setting planes

			for (int j = 0; j < offset; j++)
			{
				Pixel localPixel  = localPixels [j]
					| _transPlane[i];
				// Note 2nd remotePixel with different scope.
				Pixel remotePixel = remotePixels[j]
					| mask          ;

				localPixels [j + offset] = localPixel ;
				remotePixels[j + offset] = remotePixel;

				_transPixel   [remotePixel] = localPixel;
				_pixelRemapped[remotePixel] = True;
			}

			offset <<= 1;
		}

	assert(offset == 1 << (_numPlanes - _numPlanesAvail));
}

void ColorTranslate::colorInfoChanged (ColorInfo *col)
{
  assert (col == _loc_col);
  assert (0); // don't know what to do here
}
