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
#ifndef CUBEMASTER_HH
#define CUBEMASTER_HH

#include "cube/cube_display_list.hh"

class Cube;
class AmpInfo;
class CubeInformList;
class CubeInform;
class SeisColorPop;
class CubeDisplay;


class CubeMaster : public CubeDisplayList {

   protected:
         static CubeMaster *_cube_master;
         CubeDisplay       *_curr_cube_disp;
         AmpInfo           *_amp_info;  // not defined yet
         CubeInformList    *_inform_list;
         int                _norm_type;
         CubeMaster();
         virtual ~CubeMaster();

   public:
         static CubeMaster *instance();

         void add(CubeDisplay *cube_display);
         void remove(CubeDisplay *cube_display);

         void notifyOfNewCubeCreated(Cube *);
         void addInformer(CubeInform *inform);
         void delInformer(CubeInform *inform);
         SeisColorPop *colorPop(CubeDisplay *);
};


#endif
