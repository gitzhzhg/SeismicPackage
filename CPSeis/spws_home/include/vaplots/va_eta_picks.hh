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
#ifndef _VA_ETA_PICKS_HH
#define _VA_ETA_PICKS_HH

#include "vaplots/va_picks.hh"
#include "vaplots/va_vect_picks.hh"
#include "oprim/base_data.hh"
#include "vaplots/va_eta_plot.hh"//temporary


#define VA_ETA_PICKS "VaEtaPicks"


class VaEtaPickData : public VaPickData
{
public:
  VaEtaPickData(class VfManager *manager,
                class VaPicks *picks);
  virtual ~VaEtaPickData();
  virtual int getNumPts(long junk);
  virtual void setNumPoints(int n){_num_points = n;}

private:  
  int _num_points;

};

class VaEtaPicker : public VaPicker
{
public:
	
  VaEtaPicker(class PlotBase *plot,
              class VfManager *manager,
              class VaPicks *picks,
              class VectorLinkedList *vectors,
              class SeisPlot *semblance_plot);
  virtual ~VaEtaPicker();

  virtual void deletePick();

  void bracketTime(float y_val, int vel_type = VTIN, int deleting = 0);

  int index() {return _index;}

  int inserting() {return _insert;}
  
protected:

  int canEdit();//Temporary

private:

  class SeisPlot *_sp;
  int _started;
  float _vhor, _vnmo;
  float _eta, _yval, _xval;
  class VaEtaPicks  *_eta_picks;
  VaEtaPlot         *_eta_plot;

  virtual void noModButtonOnePress  (int x , int y);
  virtual void noModButtonOneMotion (int x1, int x2,
                                     int y1, int y2);
  virtual void noModButtonOneRelease(int x1, int x2,
                                     int y1, int y2);

  virtual void noModButtonTwoPress  (int x , int y);
  virtual void noModButtonTwoMotion (int x1, int x2,
                                     int y1, int y2);
  virtual void noModButtonTwoRelease(int x1, int x2,
                                     int y1, int y2);
};

class VaEtaPicks : public VaPicks
{
public:
	
  VaEtaPicks(class VfManager *manager, class VfHorizons *horizons,
             class VaPlot *plot, SeisPlot *sp,
             class VaVectColors *colors, SeisPlot *semblance_plot);
  virtual ~VaEtaPicks();
  virtual void insertStart(int index, float x, float y);
  virtual void replaceDrag(int index, float x, float y);
  virtual void cleanupPick(int index, float x, float y);
  Boolean haveVector();
  virtual void deletePick(SeisPlot *sp);
  VaEtaPicker *getPicker();


protected:
  class SeisVectLinkedList  *_white_rbn_vectors;
  class VectData            *_white_rbn_data;
  class Vector              *_white_rbn_vector;

 
private:
  VaEtaPickData  *_eta_data;
  SeisPlot       *_semblance_plot;

  /*
   * From VaPicks
   */
  virtual void init(SeisPlot *sp);
  virtual char *getClassName();

  virtual VaPicker *newSeparateWindow(SeisPlot *sp);

};

#endif /* _VA_ETA_PICKS_HH */
