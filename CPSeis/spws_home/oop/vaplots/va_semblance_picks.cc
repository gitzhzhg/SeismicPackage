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
// $Id: va_semblance_picks.cc,v 1.6 2004/07/16 12:42:22 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_semblance_picks.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_plot.hh"
#include "vaplots/va_eta_plot.hh"
#include "vaplots/va_xh_trans.hh"
#include "vaplots/va_eta_overlays.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"
#include "sp/seis_plot.hh"
#include "plot_image.hh"
#include "plot/pick_watch.hh"
#include "sl/shell_watch.hh"

#include <math.h>
#include <assert.h>




//!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Some of this is temporary until the vel file format supports eta vals
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!













VaSemblancePickData::VaSemblancePickData(VfManager *manager, VaPicks *picks)
  : VaPickData(manager, picks),
    _hiding_active_pick(0), _mod_count(0), _multi_hold(0)
{
  _overlay_type = _picks->getColors()->getOverlayType();
}

VaSemblancePickData::~VaSemblancePickData()
{
  /* do nothing */
}

void VaSemblancePickData::beforeChanges()
{
  assert(!vaIsHoldingVectors() && !_mod_count && !_multi_hold);

  _change_act_pick = _rem_ins_pick = 0;
  _doing_total_changes = 0;
}

void VaSemblancePickData::afterChanges()
{
  assert(!vaIsHoldingVectors() && !_mod_count && !_multi_hold);

  int old_act_pick;

  if (_change_act_pick)
    {
      assert(_old_act_func == (int) manager()->activeDataset()->
             getActiveVelocityFunction());

      int indices[2];
      int numIndices = 0;

      if (_old_act_pick != -1)
        {
          if (_rem_ins_pick)
            {
              old_act_pick = adjustIndex(_old_act_pick,
                                         _pick_index, _pick_nrem, _pick_nins);

              if (old_act_pick != -1)
                indices[numIndices++] = old_act_pick;
            }
          else
            {
              indices[numIndices++] = _old_act_pick;
            }
        }

      int new_act_pick = (int) manager()->activeDataset()->
        getActivePick((long) _old_act_func);

      if (new_act_pick != -1)
        {
          if (_rem_ins_pick)
            {
				/*
				 * Only recolor if not redrawn by
				 * remove/insert
				 */
              if (!newIndexEffected(new_act_pick,
                                    _pick_index, _pick_nrem, _pick_nins))
                {
                  indices[numIndices++] = new_act_pick;
                }
            }
          else
            {
              indices[numIndices++] = new_act_pick;
            }
        }

      if ((numIndices == 2) && (indices[0] == indices[1]))
        numIndices = 0;

      if (numIndices)
        modAttributesByIndices(indices, numIndices, 1);

      if (((VaSemblancePicks *) _picks)->
          getOverlay(VaVectColors::AOL)->isVisible()
          && (_overlay_type != VTNM))
        {
          int aol_indices[4];
          int aol_numIndices = 0;
          int num, i;
          int extra = (_overlay_type == VTIN) ? 0 : 1;

          if (_old_act_pick != -1)
            {
              if (_rem_ins_pick)
                old_act_pick = adjustIndex(
                                           _old_act_pick, _pick_index,
                                           _pick_nrem, _pick_nins, extra);
              else
                old_act_pick = _old_act_pick;

              if (old_act_pick != -1)
                {
                  num = 1;
                  xlateRange(&old_act_pick, &num,
                             VaVectColors::AOL);

                  for (i = 0; i < num; i++)
                    aol_indices[aol_numIndices++]
                      = old_act_pick + i;
                }
            }

          if (new_act_pick != -1)
            {
              if (_rem_ins_pick)
                {
                  if (newIndexEffected(new_act_pick,
                                       _pick_index, _pick_nrem,
                                       _pick_nins, extra))
                    {
                      new_act_pick = -1;
                    }
                }

              if (new_act_pick != -1)
                {
                  num = 1;
                  xlateRange(&new_act_pick, &num,
                             VaVectColors::AOL);

                  for (i = 0; i < num; i++)
                    aol_indices[aol_numIndices++]
                      = new_act_pick + i;
                }
            }

          if      ((aol_numIndices == 2)
                   && (aol_indices[0] == aol_indices[1]))
            {
              aol_numIndices = 0;
            }
          else if ((aol_numIndices == 4)
                   && (aol_indices[0] == aol_indices[2]))
            {
              assert(aol_indices[1] == aol_indices[3]);
              aol_numIndices = 0;
            }
			
          if (aol_numIndices)
            modAttributesByIndices(aol_indices,
                                   aol_numIndices, 1,
                                   VaVectColors::AOL);
        }
    }
}

void VaSemblancePickData::preTotalChanges(VfDataset *dataset)
{
  if (manager()->activeDataset() == dataset)
    {
      modIndicesBefore(0, getNumPts());

      for (int i = 0; i < VaVectColors::SEL; i++)
        if (((VaSemblancePicks *) _picks)->getOverlay(i)->
            isVisible())
          {
            modIndicesBefore(0, getNumPts((long) i),
                             (long) i);
          }

      assert(_mod_count == 0);
      _mod_count++;

      assert(_doing_total_changes == 0);
      _doing_total_changes = 1;
    }
}

void VaSemblancePickData::postTotalChanges(VfDataset *dataset)
{
  if (manager()->activeDataset() == dataset)
    {
      assert((_mod_count == 1) && !_multi_hold
             && !vaIsHoldingVectors());

      int num_others_vis, i;
      for (num_others_vis = i = 0; i < VaVectColors::SEL; i++)
        {
          if (((VaSemblancePicks *) _picks)->getOverlay(i)->
              isVisible())
            {
              num_others_vis++;
            }
        }

      int need_hold = num_others_vis > 0;
      if (need_hold)
        vaHoldVectors();

      modIndicesAfter(0, getNumPts());
      modDone();

      for (i = 0;
           (num_others_vis > 0) && (i < VaVectColors::SEL);
           i++)
        {
          if (((VaSemblancePicks *) _picks)->getOverlay(i)->
              isVisible())
            {
              modIndicesAfter(0, getNumPts((long) i),
                              (long) i);
              modDone((long) i);
              num_others_vis--;
            }
        }

      assert(num_others_vis == 0);

      if (need_hold)
        vaFlushVectors();

      _mod_count--;
    }
}

void VaSemblancePickData::preNewActiveVelocityFunction(VfDataset *dataset)
{
  if (manager()->activeDataset() == dataset)
    {
      modIndicesBefore(0, getNumPts());

      for (int i = VaVectColors::CMP; i <= VaVectColors::AOL; i++)
        {
          if (((VaSemblancePicks *) _picks)->getOverlay(i)->
              isVisible())
            {
              modIndicesBefore(0, getNumPts((long) i),
                               (long) i);
            }
        }

      _mod_count++;
    }
}

void VaSemblancePickData::postNewActiveVelocityFunction(VfDataset *dataset)
{
  if (manager()->activeDataset() == dataset)
    {
      int i_held_em_global = 0, i_held_em_local = 0;

      int num_vis, i;
      for (num_vis = 0, i = VaVectColors::CMP;
           i <= VaVectColors::AOL;
           i++)
        {
          if (((VaSemblancePicks *) _picks)->getOverlay(i)->
              isVisible())
            {
              num_vis++;
            }
        }

      int changeOther = (num_vis > 0);

      if (!_multi_hold)
        {
          if (_mod_count > 1)
            {
              vaHoldVectors();
              _multi_hold = i_held_em_global = 1;
            }
          else if (changeOther)
            {
              vaHoldVectors();
              i_held_em_local = 1;
            }
        }

      modIndicesAfter(0, getNumPts());
      modDone();

      for (i = VaVectColors::CMP;
           (num_vis > 0) && (i <= VaVectColors::AOL);
           i++)
        {
          if (((VaSemblancePicks *) _picks)->
              getOverlay(i)->isVisible())
            {
              modIndicesAfter(0, getNumPts((long) i),
                              (long) i);
              modDone((long) i);
              num_vis--;
            }
        }

      assert(num_vis == 0);

      _mod_count--;

      if (_multi_hold && (_mod_count == 0))
        {
          assert(!(i_held_em_global || i_held_em_local));

          vaFlushVectors();
          _multi_hold = 0;
        }
      else if (i_held_em_local)
        {
          vaFlushVectors();
        }
    }

  //Temporary kludge
  if( ((VaSemblancePicks *) _picks)->getEtaOverlays() != NULL)
    ((VaSemblancePicks *) _picks)->getEtaOverlays()->changeActiveVelocityFunction();
}

void VaSemblancePickData::preModifyPicks(VfDataset *dataset,
                                         long ifun, int type, long ipick, long nrem)
{
  assert(manager()->activeDataset() == dataset);
  assert(dataset->getActiveVelocityFunction() == ifun);

  int pick = (int) ipick;
  int rem  = (int) nrem ;
  checkRange(&pick, &rem, defaultId, type);
  modIndicesBefore(pick, rem);

  if (((VaSemblancePicks *) _picks)->
      getOverlay(VaVectColors::AOL)->isVisible()
      && (_overlay_type != VTNM))
    {
      pick = (int) ipick;
      rem  = (int) nrem ;
      checkRange(&pick, &rem, (long) VaVectColors::AOL, type);
      xlateRange(&pick, &rem, (long) VaVectColors::AOL      );

      modIndicesBefore(pick, rem, (long) VaVectColors::AOL);
    }

  assert(_mod_count == 0);
  _mod_count++;
}

void VaSemblancePickData::postModifyPicks(VfDataset *dataset,
                                          long ifun, int type, 
                                          long ipick, long nrem, long nins)
{
  assert(manager()->activeDataset() == dataset);
  assert(dataset->getActiveVelocityFunction() == ifun);

  assert((_mod_count == 1) && !_multi_hold && !vaIsHoldingVectors());

  int changeOther = ((VaSemblancePicks *) _picks)->
    getOverlay(VaVectColors::AOL)->isVisible()
    && (_overlay_type != VTNM);

  if (changeOther)
    vaHoldVectors();

  int pick = (int) ipick;
  int ins  = (int) nins ;
  checkRange(&pick, &ins, defaultId, type);
  modIndicesAfter(pick, ins);
  modDone();

  if (changeOther)
    {
      pick = (int) ipick;
      ins  = (int) nins ;
      checkRange(&pick, &ins, (long) VaVectColors::AOL, type);
      xlateRange(&pick, &ins, (long) VaVectColors::AOL      );

      modIndicesAfter(pick, ins, (long) VaVectColors::AOL);
      modDone        (           (long) VaVectColors::AOL);
    }

  if (changeOther)
    vaFlushVectors();

  _mod_count--;

  /*
   * For recoloring in afterChanges
   */
  _rem_ins_pick = 1;
  _pick_index = (int) ipick;
  _pick_nrem  = (int) nrem;
  _pick_nins  = (int) nins;
}

void VaSemblancePickData::preNewActiveDataset()
{
  preTotalChanges(manager()->activeDataset());
}

void VaSemblancePickData::postNewActiveDataset()
{
  postTotalChanges(manager()->activeDataset());
}

void VaSemblancePickData::preNewActivePicks(VfDataset *dataset,
                                            long ifun, long nchng)
{
  if (manager()->activeDataset() == dataset)
    {
      _old_act_func = (int) dataset->getActiveVelocityFunction();

      if (((long) _old_act_func >= ifun        )
          && ((long) _old_act_func <  ifun + nchng))
        {
          _change_act_pick = 1;
          _old_act_pick = (int) dataset->getActivePick(
                                                       (long) _old_act_func);
        }
    }
}

void VaSemblancePickData::postNewActivePicks(VfDataset * /*dataset*/,
                                             long /*ifun*/, long /*nchng*/)
{
  /* do it in afterChanges */
}

void VaSemblancePickData::preNewReferenceDataset()
{
  if (((VaSemblancePicks *) _picks)->getOverlay(VaVectColors::CMP)->
      isVisible() && !_doing_total_changes)
    {
      modIndicesBefore(0, getNumPts((long) VaVectColors::CMP),
                       (long) VaVectColors::CMP);

      assert(_mod_count == 0);
      _mod_count++;
    }
}

void VaSemblancePickData::postNewReferenceDataset()
{
  if (((VaSemblancePicks *) _picks)->getOverlay(VaVectColors::CMP)->
      isVisible() && !_doing_total_changes)
    {
      assert((_mod_count == 1) && !_multi_hold
             && !vaIsHoldingVectors());

      modIndicesAfter(0, getNumPts((long) VaVectColors::CMP),
                      (long) VaVectColors::CMP);
      modDone((long) VaVectColors::CMP);

      _mod_count--;
    }
}

void VaSemblancePickData::preNewReferenceVelocityFunction(VfDataset *dataset)
{
  if (manager()->activeDataset() == dataset
      && ((VaSemblancePicks *) _picks)->getOverlay(VaVectColors::REF)->
      isVisible())
    {
      modIndicesBefore(0, getNumPts((long) VaVectColors::REF),
                       (long) VaVectColors::REF);

      _mod_count++;
    }
}

void VaSemblancePickData::postNewReferenceVelocityFunction(VfDataset *dataset)
{
  if (manager()->activeDataset() == dataset
      && ((VaSemblancePicks *) _picks)->getOverlay(VaVectColors::REF)->
      isVisible())
    {
      int i_held_em_global = 0;

      if (!_multi_hold && (_mod_count > 1))
        {
          vaHoldVectors();
          _multi_hold = i_held_em_global = 1;
        }

      modIndicesAfter(0, getNumPts((long) VaVectColors::REF),
                      (long) VaVectColors::REF);
      modDone((long) VaVectColors::REF);

      _mod_count--;

      if (_multi_hold && (_mod_count == 0))
        {
          assert(!i_held_em_global);

          vaFlushVectors();
          _multi_hold = 0;
        }
    }
}

void VaSemblancePickData::preNewNeighbors(VfDataset *dataset)
{
  if (manager()->activeDataset() == dataset)
    {
      int num_vis, i;
      for (num_vis = 0, i = VaVectColors::PIL;
           i <= VaVectColors::NXL;
           i++)
        {
          if (((VaSemblancePicks *) _picks)->getOverlay(i)->
              isVisible())
            {
              modIndicesBefore(0, getNumPts((long) i),
                               (long) i);

              num_vis++;
            }
        }

      if (num_vis > 0)
        _mod_count++;
    }
}

void VaSemblancePickData::postNewNeighbors(VfDataset *dataset)
{
  if (manager()->activeDataset() == dataset)
    {
      int num_vis, i;
      for (num_vis = 0, i = VaVectColors::PIL;
           i <= VaVectColors::NXL;
           i++)
        {
          if (((VaSemblancePicks *) _picks)->getOverlay(i)->
              isVisible())
            {
              num_vis++;
            }
        }

      if (num_vis > 0)
        {
          int i_held_em_global = 0, i_held_em_local = 0;

          if (!_multi_hold)
            {
              if (_mod_count > 1)
                {
                  vaHoldVectors();
                  _multi_hold = i_held_em_global = 1;
                }
              else if (num_vis > 1)
                {
                  vaHoldVectors();
                  i_held_em_local = 1;
                }
            }

          for (i = VaVectColors::PIL;
               (num_vis > 0) && (i <= VaVectColors::NXL);
               i++)
            {
              if (((VaSemblancePicks *) _picks)->
                  getOverlay(i)->isVisible())
                {
                  modIndicesAfter(0, getNumPts((long) i),
                                  (long) i);
                  modDone((long) i);
                  num_vis--;
                }
            }

          assert(num_vis == 0);

          _mod_count--;

          if (_multi_hold && (_mod_count == 0))
            {
              assert(!(i_held_em_global || i_held_em_local));

              vaFlushVectors();
              _multi_hold = 0;
            }
          else if (i_held_em_local)
            {
              vaFlushVectors();
            }
        }
    }
}

void VaSemblancePickData::get_ds_and_ifun(long id, VfDataset **ds, long *ifun)
{
  switch ((int) id)
    {
      case (int) BaseData::defaultId:
        *ds = manager()->activeDataset();
        *ifun = (*ds)->getActiveVelocityFunction();
        break;
      case VaVectColors::REF:
        *ds = manager()->activeDataset();
        *ifun = (*ds)->getReferenceVelocityFunction();
        break;
      case VaVectColors::CMP:
        /*
         * Using a pair of {} so I can define new variables.
         */
        {
          *ds = manager()->referenceDataset();
          VfDataset *ads = manager()->activeDataset();

          if (*ds == ads)
            {
              *ifun = -1L;
            }
          else
            {
              long act_fun = ads->getActiveVelocityFunction();

              if (act_fun == -1L)
                *ifun = -1L;
              else
                *ifun = (*ds)->findMatchingVelfun(
                                                  ads->getXloc(act_fun),
                                                  ads->getYloc(act_fun));
            }

          break;
        }
      case VaVectColors::AOL:
        *ds = manager()->activeDataset();
        *ifun = (*ds)->getActiveVelocityFunction();
        break;
      case VaVectColors::PIL:
        *ds = manager()->activeDataset();
        *ifun = (*ds)->findPrevXloc();
        break;
      case VaVectColors::NIL:
        *ds = manager()->activeDataset();
        *ifun = (*ds)->findNextXloc();
        break;
      case VaVectColors::PXL:
        *ds = manager()->activeDataset();
        *ifun = (*ds)->findPrevYloc();
        break;
      case VaVectColors::NXL:
        *ds = manager()->activeDataset();
        *ifun = (*ds)->findNextYloc();
        break;
      default:
        assert(0);
    }
}

/*
 * the xlate functions translate to the values needed for
 * plotting interval velocity, if necessary.  The interval
 * velocity staircase has a different number of points than
 * other velocity plots.
 */
int VaSemblancePickData::xlateNumPts(int num, long id)
{
  int retval;

  if ((_overlay_type == VTIN) && (id != BaseData::defaultId))
    {
      retval = (num < 2) ? 0 : 2 * num - 2;
    }
  else
    {
      retval = num;
    }

  return retval;
}

int VaSemblancePickData::xlateXindex(int i, long id)
{
  int retval;

  if ((_overlay_type == VTIN) && (id != BaseData::defaultId))
    {
      retval = (i + 2) / 2;
    }
  else
    {
      retval = i;
    }

  return retval;
}

int VaSemblancePickData::xlateYindex(int i, long id)
{
  int retval;

  if ((_overlay_type == VTIN) && (id != BaseData::defaultId))
    {
      retval = (i + 1) / 2;
    }
  else
    {
      retval = i;
    }

  return retval;
}

void VaSemblancePickData::xlateRange(int *i, int *num, long id)
{
  if ((_overlay_type == VTIN) && (id != BaseData::defaultId))
    {
      if (*i == 0)
        {
          (*i  )++;
          (*num)--;
        }

      *i    = 2 * (*i - 1);
      *num *= 2;
    }
  /*
   * Under these conditions, getNumPts is zero.
   */
  else if ((_overlay_type == VTNM) && ((int) id == VaVectColors::AOL))
    {
      *i   = 0;
      *num = 0;
    }
  else
    {
      /* i and num unchanged */
    }
}

/*
 * If a pick mod is done in a different velocity type, you
 * must update all the way to the end.
 */
void VaSemblancePickData::checkRange(int *i, int *num, long id, int type)
{
  int plotted_type;

  switch ((int) id)
    {
      case (int) BaseData::defaultId:
        plotted_type = VTNM;
        break;
      case VaVectColors::REF:
      case VaVectColors::CMP:
      case VaVectColors::AOL:
      case VaVectColors::PIL:
      case VaVectColors::NIL:
      case VaVectColors::PXL:
      case VaVectColors::NXL:
        plotted_type = _overlay_type;
        break;
      default:
        assert(0);
    }

  if (type != plotted_type)
    {
      VfDataset *ds;
      long ifun;
      get_ds_and_ifun(id, &ds, &ifun);

      *num = (int) ds->numPicks(ifun) - *i;
    }
}

int VaSemblancePickData::getNumPts(long id)
{
  int retval;

  VfDataset *ds;
  long ifun;
  get_ds_and_ifun(id, &ds, &ifun);

  if ( (ifun == -1)
       || (((int) id == VaVectColors::AOL) && (_overlay_type == VTNM)))
    {
      retval = 0;
    }
  else
    {
      retval = xlateNumPts((int) ds->numPicks(ifun), id); 
    }

  return retval;
}

float VaSemblancePickData::getX(int i, long id)
{
  VfDataset *ds;
  long ifun;
  get_ds_and_ifun(id, &ds, &ifun);
  assert(ifun != -1);

  return ds->getOrdinate(ifun, (long) xlateXindex(i, id),
                         (id == BaseData::defaultId) ? VTNM : _overlay_type);
}

float VaSemblancePickData::getY(int i, long id)
{
  VfDataset *ds;
  long ifun;
  get_ds_and_ifun(id, &ds, &ifun);
  assert(ifun != -1);

  return ds->getAbscissa(ifun, (long) xlateYindex(i, id),
                         (id == BaseData::defaultId) ? VTNM : _overlay_type);
}

int VaSemblancePickData::getAltMarkerColor(int i, long id)
{
  int retval;
  int soap;	/* show overlay active pick */

  switch ((int) id)
    {
      case (int) BaseData::defaultId:
        if (_hiding_active_pick)
          {
            retval = VaVectColors::DEF_COL;
          }
        else
          {
            VfDataset *ds = manager()->activeDataset();

            if (i == (int) ds->getActivePick(
                                             ds->getActiveVelocityFunction()))
              {
                retval = VaVectColors::ACT_COL;
              }
            else
              {
                retval = VaVectColors::DEF_COL;
              }
          }
        break;
      case VaVectColors::REF:
      case VaVectColors::CMP:
      case VaVectColors::AOL:
      case VaVectColors::PIL:
      case VaVectColors::NIL:
      case VaVectColors::PXL:
      case VaVectColors::NXL:
        soap = _picks->getColors()->getShowOverlayActivePick();
        if (!_hiding_active_pick
            && (( soap == VaVectColors::MARK_ALL)
                || ((soap == VaVectColors::MARK_ACT)
                    && ((int) id == VaVectColors::AOL))))
          {
            VfDataset *ds;
            long ifun;
            get_ds_and_ifun(id, &ds, &ifun);
            assert(ifun != -1);

            int act_pck = (int) ds->getActivePick(ifun);
            int cur_pck = xlateXindex(i, id);

            if (act_pck == cur_pck)
              retval =  VaVectColors::ACT_COL;
            else
              retval = 0;	/* not DEF_COL */
          }
        else
          {
            retval = 0;	/* not DEF_COL */
          }
        break;
      default:
        retval = -1;	/* warn me not */
        assert(0);
    }

  return retval;
}

void VaSemblancePickData::hideActivePick()
{
  assert(!vaIsHoldingVectors());
  VfDataset *ds = manager()->activeDataset();
  long act_fun = ds->getActiveVelocityFunction();

  if (act_fun != -1)
    {
      int act_pck = (int) ds->getActivePick(act_fun);

      if (act_pck != -1)
        {
          _hiding_active_pick = 1;

          modAttributes(act_pck, 1);

          int aol_indices = act_pck;
          int aol_num = 1;

          xlateRange(&aol_indices, &aol_num, VaVectColors::AOL);

          if (aol_num)
            modAttributes(aol_indices, aol_num, 0,
                          VaVectColors::AOL);

          _hiding_active_pick = 0;
        }
    }
}

void VaSemblancePickData::setOverlayType(int set)
{
  assert(!vaIsHoldingVectors());

  int num_vis, i;
  for (num_vis = i = 0; i < VaVectColors::SEL; i++)
    {
      if (((VaSemblancePicks *) _picks)->getOverlay(i)->isVisible())
        {
          modIndicesBefore(0, getNumPts((long) i), (long) i);
          num_vis++;
        }
    }

  _overlay_type = set;

  int need_hold = num_vis > 1;

  if (need_hold)
    vaHoldVectors();

  for (i = 0; (num_vis > 0) && (i < VaVectColors::SEL); i++)
    {
      if (((VaSemblancePicks *) _picks)->getOverlay(i)->isVisible())
        {
          modIndicesAfter(0, getNumPts((long) i), (long) i);
          modDone((long) i);
          num_vis--;
        }
    }

  assert(num_vis == 0);

  if (need_hold)
    vaFlushVectors();
}

void VaSemblancePickData::redrawActivePicks()
{
  VfDataset *ds;
  long ifun;
  int act_pck, num;

  for (int i = 0; i < VaVectColors::SEL; i++)
    if (((VaSemblancePicks *) _picks)->getOverlay(i)->
        isVisible())
      {
        get_ds_and_ifun((long) i, &ds, &ifun);

        if (ifun != -1)
          {
            act_pck = (int) ds->getActivePick(ifun);

            if (act_pck != -1)
              {
                num = 1;

                xlateRange(&act_pck, &num, (long) i);

                if (num > 0)
                  modAttributes(act_pck, num, 0,
                                (long) i);
              }
          }
      }
}

#define FALLBACK "mouse*VA_SEM_PICK: BTN#1: Edit or Insert Pick, BTN#2: Delete Pick\\nControl-BTN#1: Suppress Snap Pick if Enabled, Shift-BTN#2: Delete All Picks"

VaSemblancePicker::VaSemblancePicker(PlotBase *plot, VfManager *manager,
                                     VaPicks *picks, VectorLinkedList *vectors)
  : VaPicker(plot, "", "VA_SEM_PICK", FALLBACK,
             manager, picks, vectors)
{
    _snapOverrideOnCntl = False;

}

VaSemblancePicker::~VaSemblancePicker()
{
  /* do nothing */
}

void VaSemblancePicker::noModButtonOnePress(int x, int y)
{
  VaSemblancePicks *sembpicks = (VaSemblancePicks *)_picks;
  VaEtaPlot *eta = sembpicks->eta();


  float xWC = getPlot()->xWC(x);
  float yWC = getPlot()->yWC(y);

  VfDataset *ds   = _manager->activeDataset();
  long       ifun = ds->getActiveVelocityFunction();

  if (ifun == -1)
    {
      assert(!canEdit());
      doBeep();
      ignoreActions();
    }
  else
    {
      bracketTime(yWC);

      if(eta->isActivated()) return;

      if (_use_t_min && (yWC < _t_min))
        yWC = _t_min;

      if (_use_t_max && (yWC > _t_max))
        yWC = _t_max;

      if (canEdit())
        {
          if (_insert)
            {
              _picks->broadcastInsStrt(_index, xWC, yWC);
            }
          else
            {
				/*
				 * Remember PickWatch is a PickBase.
				 * Do not want PickBase changing in middle
				 * of action.
				 */
              Bool watch_ok = PlotBase::watchOK();
              if (watch_ok)
                PlotBase::setWatchOK(False);

              if ((int) ds->getActivePick(ifun) != _index)
                ds->setActivePick(ifun, (long) _index);

              if (watch_ok)
                PlotBase::setWatchOK(True );

              _picks->broadcastRepStrt(_index, xWC, yWC);
            }

          dpyInvVel(PlotImage::MOUSE_AUX, _index, xWC, yWC);
        }
      else
        {
          if (_insert)
            {
              doBeep();
            }
          else
            {
              Bool watch_ok = PlotBase::watchOK();
              if (watch_ok)
                PlotBase::setWatchOK(False);

              ds->setActivePick(ifun, (long) _index);

              if (watch_ok)
                PlotBase::setWatchOK(True );
            }

          ignoreActions();
        }
    }
}

void VaSemblancePicker::noModButtonOneMotion(int /*x1*/, int x2,
					     int /*y1*/, int y2)
{
  VaSemblancePicks *sembpicks = (VaSemblancePicks *)_picks;
  VaEtaPlot *eta = sembpicks->eta();

  if(eta->isActivated()) return;

  float xWC = getPlot()->xWC(x2);
  float yWC = getPlot()->yWC(y2);

  if (_use_t_min && (yWC < _t_min))
    yWC = _t_min;

  if (_use_t_max && (yWC > _t_max))
    yWC = _t_max;

  if (_insert)
    {
    _picks->broadcastInsDrag(_index, xWC, yWC);
    }
  else
    {
    _picks->broadcastRepDrag(_index, xWC, yWC);
    }

  dpyInvVel(PlotImage::MOUSE_AUX, _index, xWC, yWC);
}

void VaSemblancePicker::noModButtonOneRelease(int /*x1*/, int x2,
					      int /*y1*/, int y2)
{
  VaSemblancePicks *sembpicks = (VaSemblancePicks *)_picks;
  VaEtaPlot *eta = sembpicks->eta();
  VfDataset *ds = _manager->activeDataset();


  long ifun = ds->getActiveVelocityFunction();

  float xWC = getPlot()->xWC(x2);
  float yWC = getPlot()->yWC(y2);

  //If we want to make a semblance pick and do the eta put this
  //code at the bottom of this method but do not allow the
  //_use_t_min _use_t_max loop below to be executed because it
  //will mess things up when changing to eta picking after doing
  //semblance picking.
  if(eta->isActivated())
    {
      //We only allow a eta pick at a semblance pick location
      if(_insert)
        {
          doBeep();
          return;
        }
      PickWatch *pick_watch = ( PickWatch *) 0;
      ShellWatch *shell_watch = (ShellWatch *) 0;

      _picks->setWatch(1);
      pick_watch = new  PickWatch();
      shell_watch = new ShellWatch();

      //Make sure time of eta is same time as nmo pick
      yWC = ds->getAbscissa(ifun, (long) _index, VTNM);

      eta->receivePick(VaEtaPlot::INSERT_PICK, xWC, yWC,
                       _picks->getPlot()->getDisplayedXbin(),
                       _picks->getPlot()->getDisplayedYbin(), _index);
      _picks->setWatch(0);

      delete pick_watch;
      delete shell_watch;
   
      return;
    }

  if (_use_t_min && (yWC < _t_min))
    yWC = _t_min;

  if (_use_t_max && (yWC > _t_max))
    yWC = _t_max;

         


  if (_insert)
    {
      if (sembpicks->isSnapModeActive() && !_snapOverrideOnCntl) {

          float t_snap_max = yWC + sembpicks->getTimeHalfWindow();
          float t_snap_min = yWC - sembpicks->getTimeHalfWindow();
          float v_snap_max = xWC + sembpicks->getVelocityHalfWindow();
          float v_snap_min = xWC - sembpicks->getVelocityHalfWindow();

          // set bounds
          float semb_v_min = 
                    ((VaSemblancePicks *) _picks)->getSembGuiMinVel();
          float semb_v_max = 
                    ((VaSemblancePicks *) _picks)->getSembGuiMaxVel();
          float semb_t_min = 
                    ((VaSemblancePicks *) _picks)->getSembGuiMinTime();
          float semb_t_max = 
                    ((VaSemblancePicks *) _picks)->getSembGuiMaxTime();
          v_snap_min = (v_snap_min < semb_v_min) ?
                           semb_v_min : v_snap_min;
          v_snap_max = (v_snap_max > semb_v_max) ?
                           semb_v_max : v_snap_max;
          if (_use_t_min && (t_snap_min < _t_min)) {
              t_snap_min = _t_min;
          }
          else {
              t_snap_min = t_snap_min > semb_t_min ?
                               t_snap_min : semb_t_min;
          }
          if (_use_t_max && (t_snap_max > _t_max)) {
              t_snap_max = _t_max;
          }
          else {
              t_snap_max = t_snap_max < semb_t_max ?
                               t_snap_max : semb_t_max;
          }

          snapPickToLocalMax(v_snap_min, v_snap_max,
                             t_snap_min, t_snap_max,
                             xWC, yWC);
      }

      _picks->broadcastInsDone(             _index, xWC, yWC);
      ds-> insertPick         (ifun, (long) _index, yWC, xWC, VTNM);
    }
  else
    {
      _picks->broadcastRepDone(             _index, xWC, yWC);
      ds->replacePick         (ifun, (long) _index, yWC, xWC, VTNM);
    }

  dpyInvVel(PlotImage::MOUSE_AMP, _index, xWC, yWC);

}

void VaSemblancePicker::noModButtonTwoPress(int x, int y)
{
  float dist_mm;
  VaSemblancePicks *sembpicks = (VaSemblancePicks *)_picks;
  VaEtaPlot *eta = sembpicks->eta();
  Vector *vector = _vectors->closestVertex(x, y, &_index, getPlot(),
                                           &dist_mm);
  if(!vector)
    {
      doBeep();
      ignoreActions();
      return;
    }

  if(eta->isActivated()) return;

  //	if (!vector || (dist_mm > _manager->utilities()->getPickingTolerance())
  if (!canEdit())
    {
      doBeep();
      ignoreActions();
    }
}

void VaSemblancePicker::noModButtonTwoMotion(int /*x1*/, int /*x2*/,
					     int /*y1*/, int /*y2*/)
{
  /* do nothing */
}

void VaSemblancePicker::noModButtonTwoRelease(int /*x1*/, int x2,
					      int /*y1*/, int y2)
{
  VaSemblancePicks *sembpicks = (VaSemblancePicks *)_picks;
  VaEtaPlot *eta = sembpicks->eta();
  VfDataset *ds = _manager->activeDataset();
  long ifun = ds->getActiveVelocityFunction();
  assert(ifun != -1);


  if(eta->isActivated())
    {
      float xWC = getPlot()->xWC(x2);
      //If we need to make sure eta time is same as nmo pick
      //do the following. This however will prevent the user
      //from deleting an eta pick if the user has done the following:
      //Picked nmo, picked eta at that nmo, move the time of the
      //nmo pick, then try to delete the eta pick.
      //float yWC = ds->getAbscissa(ifun, (long) _index, VTNM);
      float yWC = getPlot()->yWC(y2);
      eta->receivePick(VaEtaPlot::DELETE_PICK, xWC, yWC,
                       _picks->getPlot()->getDisplayedXbin(),
                       _picks->getPlot()->getDisplayedYbin(), _index);
      return;
    }

  ds->removePick(ifun, (long) _index);

        
}

void VaSemblancePicker::shiftButtonTwoPress(int x, int y)
{
  float dist_mm;
  VaSemblancePicks *sembpicks = (VaSemblancePicks *)_picks;
  VaEtaPlot *eta = sembpicks->eta();
  Vector *vector = _vectors->closestVertex(x, y, &_index, getPlot(),
                                           &dist_mm);

  if(eta->isActivated()) return;

  if(vector == 0 || !canEdit())
    {
      doBeep();
      ignoreActions();
      return;
    }
}

void VaSemblancePicker::shiftButtonTwoMotion(int /*x1*/, int /*x2*/,
                                             int /*y1*/, int /*y2*/)
{
  // do nothing
}

void VaSemblancePicker::shiftButtonTwoRelease(int /*x1*/, int /*x2*/,
                                              int /*y1*/, int /*y2*/)
{
  VaSemblancePicks *sembpicks = (VaSemblancePicks *)_picks;
  VaEtaPlot *eta = sembpicks->eta();
  VfDataset *ds = _manager->activeDataset();
  long ifun = ds->getActiveVelocityFunction();
  assert(ifun != -1);

  if(eta->isActivated())
    {
      return;
    }

  ds->deleteAllVelocityFunctionPicks(ifun);
}

void VaSemblancePicker::cntlButtonOnePress(int x, int y)
{
    // set flag that reverses sense of semblance pick snapping
    _snapOverrideOnCntl = True;

    // pass call to not modified version
    noModButtonOnePress(x, y);
}

void VaSemblancePicker::cntlButtonOneMotion(int x1, int x2, int y1, int y2) {

    // pass call to not modified version
    noModButtonOneMotion(x1, x2, y1, y2);
}

void VaSemblancePicker::cntlButtonOneRelease(int x1, int x2, int y1, int y2) {

    // pass call to not modified version
    noModButtonOneRelease(x1, x2, y1, y2);
    //
    // unset flag that reverses sense of semblance pick snapping
    _snapOverrideOnCntl = False;
}

void VaSemblancePicker::dpyInvVel(int mode, int index, float v, float t)
{
  SeisPlot *sp = (SeisPlot *) getPlot();
  float inv;

  if (index > 0)
    {
      VfDataset *ds = _manager->activeDataset();
      long ifun = ds->getActiveVelocityFunction();
      assert(ifun != -1);

      float last_t = ds->getAbscissa(ifun, (long) (index - 1), VTNM);
      float last_v = ds->getOrdinate(ifun, (long) (index - 1), VTNM);

      if (t > last_t)
        {
          float term = (v * v * t - last_v * last_v * last_t)
            / (t - last_t);

          if (term > 0.0F)
            inv = (float) sqrt((double) term);
          else
            inv = -999.0F;
        }
      else
        {
          inv = -999.0F;
        }
    }
  else
    {
      inv = v;
    }

  sp->setLocationOutputType(mode, "Inv:", (double) inv);
}

void VaSemblancePicker::snapPickToLocalMax(float v_snap_min, float v_snap_max,
                                           float t_snap_min, float t_snap_max,
                                           float &xWC, float &yWC) {

    // pass message to picks class
    ((VaSemblancePicks *) _picks)->snapPickToLocalMax(v_snap_min, v_snap_max,
                                                      t_snap_min, t_snap_max,
                                                      xWC, yWC);
}

VaSemblancePicks::VaSemblancePicks(class VfManager *manager,
                                   class VfHorizons *horizons, 
                                   class VaPlot *plot,
                                   SeisPlot *sp, class VaVectColors *colors)
  : VaPicks(manager, horizons, plot, sp, colors, VaVectColors::SEL + 1),
    _velocity_half_win(0.0), _time_half_win(0.0), _do_snap_mode(False)
{
  _eta_vect = NULL;
  _eta_data = NULL;
  _eta      = NULL;

  _data = new VaSemblancePickData(_manager, this);

  _sem_vect = _editable_vectors->add(_data, BaseData::defaultId,
                                     _colors->semblanceActiveFuncColor      (),
                                     _colors->semblanceWidth                (),
                                     False                                    ,
                                     _colors->semblanceLineStyle            (),
                                     _colors->semblanceActiveFuncMarker     (),
                                     _colors->semblanceActiveFuncMarkerSize (),
                                 _colors->semblanceActiveFuncMarkerLineWidth());

  _sem_vect->allowAltMarkerColors(True);
  _sem_vect->setAltMarkerColor(VaVectColors::DEF_COL,
                               _colors->semblanceDefaultPickColor());
  _sem_vect->setAltMarkerColor(VaVectColors::ACT_COL,
                               _colors->semblanceActivePickColor ());

  if (_colors->getShowActiveFunc())
    _sem_vect->makeVisible();


  _show_overlay_markers = _colors->getShowOverlayMarkers();

  for (int i = 0; i < VaVectColors::SEL; i++)
    {
      switch (_show_overlay_markers)
        {
          case VaVectColors::OVER_LINE:
            _overlay[i] = _constant_vectors[i]->add(_data, i,
                                   _colors->getFuncColor  (i),
                                   _colors->semblanceWidth ( ),
                                   False                     ,
                                   Vector::SolidLine,
                                   Vector::NoMarker,
                                   _colors->semblanceOverlayMarkerSize (),
                                   _colors->semblanceOverlayMarkerLineWidth());
            break;
          case VaVectColors::OVER_BOTH:
            _overlay[i] = _constant_vectors[i]->add(_data, i,
                                   _colors->getFuncColor      (i),
                                   _colors->semblanceWidth    ( ),
                                   False                         ,
                                   Vector::SolidLine,
                                   _colors->semblanceOverlayMarker         (),
                                   _colors->semblanceOverlayMarkerSize     (),
                                   _colors->semblanceOverlayMarkerLineWidth());
            break;
          case VaVectColors::OVER_MARK:
            _overlay[i] = _constant_vectors[i]->add(_data, i,
                                   _colors->getFuncColor      (i),
                                   _colors->semblanceWidth    ( ),
                                   False                         ,
                                   Vector::NoLine,
                                   _colors->semblanceOverlayMarker         (),
                                   _colors->semblanceOverlayMarkerSize     (),
                                   _colors->semblanceOverlayMarkerLineWidth());
            break;
          default:
            assert(0);
        }

      _overlay[i]->allowAltMarkerColors(True);
      _overlay[i]->setAltMarkerColor(VaVectColors::ACT_COL,
                                     _colors->semblanceActivePickColor());

      if ( _show_func[i] && _colors->getEnableShowFuncs()
           && (_colors->getShowActiveFunc() || (i == VaVectColors::REF)))
        {
          _overlay[i]->makeVisible();
        }
    }


  _va_semblance_horizons_data = new VaSemblanceHorizonsData(manager,
                                                            _colors);

  _va_horizons= new VaHorizons(manager,
                       horizons, _va_semblance_horizons_data, _colors,
                       _constant_vectors[VaVectColors::SEL], _editable_vectors,
                       !_colors->getShowActiveFunc());

  /*
   * Add coupled cursor after all vector linked lists since
   * if cursor is in rbn mode, it must repair exposes last.
   */
  _xh_trans = new SemblanceCrossHairTranslator(this);

  int length, width;
  _colors->getCrossHairSize(&length, &width);
  _cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
                   length, width, _colors->getCrossHairVisibility());
}

VaSemblancePicks::~VaSemblancePicks()
{
  /*
   * _data is deleted in base class destructor because it
   * must outlast vectors.
	 */

  if (_picker)
    delete _picker;

  _cross_hairs.remove(top());

  delete _xh_trans;

  delete _va_horizons               ;
  delete _va_semblance_horizons_data;
}

void VaSemblancePicks::init(SeisPlot *sp)
{
  _picker = new VaSemblancePicker(sp, _manager, this, _editable_vectors);


  //Temporary
  _eta_data = new VaEtaOverlayData(eta()->getTempVfManager(), this,
                                   _manager, _colors, sp, getPlot());

  _eta_vect = _editable_vectors->add(_eta_data, BaseData::defaultId,
                               _colors->semblanceActiveFuncColor          (),
                               _colors->semblanceWidth                    (),
                               False                                        ,
                               Vector::DashedLine,
                               Vector::NoMarker,
                               5,
                               5);

  _eta_vect->setAutoPenLift(1);
  _eta_vect->allowAltMarkerColors(True);
  _eta_vect->setAltMarkerColor(VaVectColors::DEF_COL,
                               _colors->semblanceDefaultPickColor());
  _eta_vect->setAltMarkerColor(VaVectColors::ACT_COL,
                               _colors->semblanceActivePickColor ());

  if (_colors->getShowActiveFunc())
    _eta_vect->makeVisible();


}

void VaSemblancePicks::insStrt(int index, float x, float y)
{
  ((VaSemblancePickData *) _data)->hideActivePick();

  int numPts = _data->getNumPts();

  if (index == -1)
    {
      _rbn_data = new VectData(1, &x, &y);

      _rbn_vector = _rbn_vectors->add(_rbn_data,
                              _colors->semblanceRbnColor                 (),
                              _colors->semblanceWidth                    (),
                              False                                        ,
                              _colors->semblanceLineStyle                (),
                              _colors->semblanceActiveFuncMarker         (),
                              _colors->semblanceActiveFuncMarkerSize     (),
                              _colors->semblanceActiveFuncMarkerLineWidth());

      _rbn_vector->makeVisible();
    }
  else if (numPts)
    {
      float xs[3];
      float ys[3];

      if (index == 0)
        {
          xs[0] = _data->getX(0);
          ys[0] = _data->getY(0);
          xs[1] = x;
          ys[1] = y;
          _rbn_data = new VectData(2, xs, ys);
        }
      else if (index == numPts)
        {
          xs[0] = _data->getX(numPts - 1);
          ys[0] = _data->getY(numPts - 1);
          xs[1] = x;
          ys[1] = y;
          _rbn_data = new VectData(2, xs, ys);
        }
      else
        {
          xs[0] = _data->getX(index - 1);
          ys[0] = _data->getY(index - 1);
          xs[1] = x;
          ys[1] = y;
          xs[2] = _data->getX(index    );
          ys[2] = _data->getY(index    );
          _rbn_data = new VectData(3, xs, ys);
        }

      _rbn_vector = _rbn_vectors->add(_rbn_data,
                                      _colors->semblanceRbnColor(),
                                      _colors->semblanceWidth   (), True);
    }
  else
    {
      _rbn_data = (VectData *) NULL;
    }
}

void VaSemblancePicks::insDrag(int index, float x, float y)
{
  if (_rbn_data)
    {
      if (index == -1)
        _rbn_data->replace(0, 1, &x, &y);
      else
        _rbn_data->replace(1, 1, &x, &y);
    }
}

void VaSemblancePicks::insDone(int /*index*/, float /*x*/, float /*y*/)
{
  if (_rbn_data)
    {
      _rbn_vectors->remove(_rbn_vector);
      delete _rbn_data;
    }
}

void VaSemblancePicks::repStrt(int index, float x, float y)
{
  int numPts = _data->getNumPts();

  if (numPts)
    {
      float xs[3];
      float ys[3];

      if (numPts == 1)
        {
          _rbn_data = new VectData(1, &x, &y);
        }
      else if (index == 0)
        {
          xs[0] = _data->getX(1);
          ys[0] = _data->getY(1);
          xs[1] = x;
          ys[1] = y;
          _rbn_data = new VectData(2, xs, ys);
        }
      else if (index == numPts - 1)
        {
          xs[0] = _data->getX(numPts - 2);
          ys[0] = _data->getY(numPts - 2);
          xs[1] = x;
          ys[1] = y;
          _rbn_data = new VectData(2, xs, ys);
        }
      else
        {
          xs[0] = _data->getX(index - 1);
          ys[0] = _data->getY(index - 1);
          xs[1] = x;
          ys[1] = y;
          xs[2] = _data->getX(index + 1);
          ys[2] = _data->getY(index + 1);
          _rbn_data = new VectData(3, xs, ys);
        }

      _rbn_vector = _rbn_vectors->add(_rbn_data,
                                      _colors->semblanceRbnColor(),
                                      _colors->semblanceWidth   (), True);
    }
}

void VaSemblancePicks::repDrag(int /*index*/, float x, float y)
{
  int numPts = _data->getNumPts();

  if      (numPts > 1)
    _rbn_data->replace(1, 1, &x, &y);
  else if (numPts > 0)
    _rbn_data->replace(0, 1, &x, &y);
}

void VaSemblancePicks::repDone(int /*index*/, float /*x*/, float /*y*/)
{
  if (_data->getNumPts())
    {
      _rbn_vectors->remove(_rbn_vector);
      delete _rbn_data;
    }
}

void VaSemblancePicks::setShowFunc(int which, int set)
{
  if (which < VaVectColors::SEL)
    {
      if ( _colors->getEnableShowFuncs()
           && (_colors->getShowActiveFunc ()
               || (which == VaVectColors::REF) ))
        {
          if (set)
            {
              assert(!_show_func[which]
                     && !_overlay[which]->isVisible());
              _overlay[which]->makeVisible();
            }
          else
            {
              assert( _show_func[which]
                      &&  _overlay[which]->isVisible());
              _overlay[which]->makeInvisible();
            }
        }
      else
        {
          assert(!_overlay[which]->isVisible());
        }

      _show_func[which] = set;
    }
}

void VaSemblancePicks::setEnableShowFuncs(int set)
{
  int i;

  if (!set)
    _data->vaHoldVectors();

  if (set)
    {
      if (_show_func[VaVectColors::REF])
        _overlay[VaVectColors::REF]->makeVisible();
    }
  else
    {
      if (_show_func[VaVectColors::REF])
        _overlay[VaVectColors::REF]->makeInvisible();
    }

  if (_colors->getShowActiveFunc())
    {
      if (set)
        {
          for (i = VaVectColors::CMP; i < VaVectColors::SEL; i++)
            if (_show_func[i])
              _overlay[i]->makeVisible();
        }
      else
        {
          for (i = VaVectColors::CMP; i < VaVectColors::SEL; i++)
            if (_show_func[i])
              _overlay[i]->makeInvisible();
        }
    }
  else
    {
      for (i = VaVectColors::CMP; i < VaVectColors::SEL; i++)
        assert(!_overlay[i]->isVisible());
    }

  if (!set)
    _data->vaFlushVectors();
}

void VaSemblancePicks::setShowActiveFunc(int set)
{
  int i;

  if (_colors->getEnableShowFuncs())
    {
      if (set)
        {
          _sem_vect->makeVisible();
          _va_horizons->reveal  ();

          /*
           * Start at VaVectColors::CMP since VaVectColors::REF
           * is independent of ShowActiveFunc.
           */
          for (i = VaVectColors::CMP; i < VaVectColors::SEL; i++)
            if (_show_func[i])
              _overlay[i]->makeVisible();
        }
      else
        {
          _data->vaHoldVectors();

          _sem_vect->makeInvisible();
          _va_horizons->hide      ();

          for (i = VaVectColors::CMP; i < VaVectColors::SEL; i++)
            if (_show_func[i])
              _overlay[i]->makeInvisible();

          _data->vaFlushVectors();
        }
    }
  else
    {
      if (set)
        {
          _sem_vect->makeVisible();
          _va_horizons->reveal  ();
        }
      else
        {
          _data->vaHoldVectors();

          _sem_vect->makeInvisible();
          _va_horizons->hide      ();

          _data->vaFlushVectors();
        }

      for (i = VaVectColors::CMP; i < VaVectColors::SEL; i++)
        assert(!_overlay[i]->isVisible());
    }
}

void VaSemblancePicks::setShowOverlayMarkers(int set)
{
  int i;

  switch (3 * _show_overlay_markers + set)
    {
      case 0:	/* no change */
      case 4:
      case 8:
        assert(0);
        break;	/* not really needed */
      case 1:	/* OVER_LINE to OVER_BOTH */
        for (i = 0; i < VaVectColors::SEL; i++)
          _overlay[i]->setMarker(
                                 _colors->semblanceOverlayMarker         (),
                                 _colors->semblanceOverlayMarkerSize     (),
                                 _colors->semblanceOverlayMarkerLineWidth());
        break;
      case 2:	/* OVER_LINE to OVER_MARK */
        _data->vaHoldVectors();

        for (i = 0; i < VaVectColors::SEL; i++)
          {
            _overlay[i]->setMarker(
                                   _colors->semblanceOverlayMarker         (),
                                   _colors->semblanceOverlayMarkerSize     (),
                                   _colors->semblanceOverlayMarkerLineWidth());

            _overlay[i]->setStyle(Vector::NoLine);
          }

        _data->vaFlushVectors();
        break;
      case 3:	/* OVER_BOTH to OVER_LINE */
        _data->vaHoldVectors();

        for (i = 0; i < VaVectColors::SEL; i++)
          _overlay[i]->setMarker(
                                 Vector::NoMarker,
                                 _colors->semblanceOverlayMarkerSize     (),
                                 _colors->semblanceOverlayMarkerLineWidth());

        _data->vaFlushVectors();
        break;
      case 5:	/* OVER_BOTH to OVER_MARK */
        _data->vaHoldVectors();

        for (i = 0; i < VaVectColors::SEL; i++)
          _overlay[i]->setStyle(Vector::NoLine);

        _data->vaFlushVectors();
        break;
      case 6:	/* OVER_MARK to OVER_LINE */
        _data->vaHoldVectors();

        for (i = 0; i < VaVectColors::SEL; i++)
          {
            _overlay[i]->setMarker(
                                   Vector::NoMarker,
                                   _colors->semblanceOverlayMarkerSize     (),
                                   _colors->semblanceOverlayMarkerLineWidth());

            _overlay[i]->setStyle(Vector::SolidLine);
          }

        _data->vaFlushVectors();
        break;
      case 7:	/* OVER_MARK to OVER_BOTH */
        for (i = 0; i < VaVectColors::SEL; i++)
          _overlay[i]->setStyle(Vector::SolidLine);
        break;
      default:
        assert(0);
        break;	/* not really needed */
    }

  _show_overlay_markers = set;
}

void VaSemblancePicks::setOverlayType(int set)
{
  ((VaSemblancePickData *) _data)->setOverlayType(set);
}

void VaSemblancePicks::setShowOverlayActivePick(int /*set*/)
{
  ((VaSemblancePickData *) _data)->redrawActivePicks();
}

char *VaSemblancePicks::getClassName()
{
  static char *retval = VA_SEMBLANCE_PICKS;

  return retval;
}

Vector *VaSemblancePicks::getOverlay(int i)
{
  return _overlay[i];
}

VaPicker *VaSemblancePicks::newSeparateWindow(SeisPlot *sp)
{
  int length, width;
  _colors->getCrossHairSize(&length, &width);
  _cross_hairs.add(sp, _xh_trans, _colors->crossHairColor(),
                   length, width, _colors->getCrossHairVisibility());

  return new VaSemblancePicker(sp, _manager, this, _editable_vectors);
}



void VaSemblancePicks::showEtaOverlays()
{
  if(_eta_vect == NULL) return;
  _eta_vect->makeInvisible();
  _eta_vect->makeVisible();
}

void VaSemblancePicks::hideEtaOverlays()
{
  if(_eta_vect == NULL) return;
  _eta_vect->makeVisible();
  _eta_vect->makeInvisible();
}

void VaSemblancePicks::activateSnapMode(float vel_halfwin, float time_halfwin,
                                        float semb_t_min, float semb_t_max,
                                        float semb_v_min, float semb_v_max) {

    // activate snap mode and save parameters

    _do_snap_mode = True;
    _velocity_half_win = vel_halfwin;
    _time_half_win = time_halfwin;
    _semb_t_min = semb_t_min;
    _semb_t_max = semb_t_max;
    _semb_v_min = semb_v_min;
    _semb_v_max = semb_v_max;
    fprintf(stdout,"VaSemblancePicks::activateSnapMode():\n");
    fprintf(stdout,"    snap velocity = %f\n",_velocity_half_win);
    fprintf(stdout,"    snap time     = %f\n",_time_half_win);
}

void VaSemblancePicks::deactivateSnapMode() {

    // deactivate snap mode and reset parameters

    _do_snap_mode = False;
    _velocity_half_win = 0.0;
    _time_half_win = 0.0;
    fprintf(stdout,"VaSemblancePicks::deactivateSnapMode():\n");
    fprintf(stdout,"    snap velocity = %f\n",_velocity_half_win);
    fprintf(stdout,"    snap time     = %f\n",_time_half_win);
}

void VaSemblancePicks::snapPickToLocalMax(float v_snap_min, float v_snap_max,
                                          float t_snap_min, float t_snap_max,
                                          float &xWC, float &yWC) {

    // Snap a pick to max semblance value within search window, changing
    // the values of xWC and yWC.

    // pass message to plot class
    _plot->snapPickToLocalMax(v_snap_min, v_snap_max, t_snap_min, t_snap_max,
                              xWC, yWC);
}














VaSemblanceHorizonsData::VaSemblanceHorizonsData(VfManager *vf_manager,
                                                 VaVectColors *colors)
  : VaHorizonsData(vf_manager, colors), _num_picks_in_bin(0)
{
  /* just initializers */
}

VaSemblanceHorizonsData::~VaSemblanceHorizonsData()
{
  if (_num_picks_in_bin)
    delete [] _pick_time;
}

int VaSemblanceHorizonsData::getNumPts(long id)
{
  int retval;

  if (_num_picks_in_bin)
    delete [] _pick_time;

  VfDataset     *ds   = _vf_manager->activeDataset();
  long           ifun = ds->getActiveVelocityFunction();

  if (ifun == -1L)
    {
      retval = 0;
    }
  else
    {
      float xloc = ds->getXloc(ifun);
      float yloc = ds->getYloc(ifun);
      SortedHorizon *sh = _va_horizons_manager->getSortedHorizon(id);
      int interpolated;

      _num_picks_in_bin = sh->getNumPicksInBin(xloc, yloc,
                                               &_pick_time, &interpolated);

      if (interpolated)
        _line_style = (int) Vector::DashedLine;
      else
        _line_style = (int) Vector::SolidLine ;

      retval = 4 * _num_picks_in_bin;
    }

  return retval;
}

float VaSemblanceHorizonsData::getX(int i, long /*id*/)
{
  float retval;

  switch (i % 4)
    {
      case 0:
      case 3:
        retval = -1000.0F;
        break;
      case 1:
      case 2:
        retval = 30000.0F;
        break;
      default:
        assert(0);
    }

  return retval;
}

float VaSemblanceHorizonsData::getY(int i, long /*id*/)
{
  float retval;

  switch (i % 4)
    {
      case 0:
      case 1:
        retval = _pick_time[i / 4];
        break;
      case 2:
      case 3:
        retval = -1.0F;
        break;
      default:
        assert(0);
    }

  return retval;
}

int VaSemblanceHorizonsData::getMarkerType(int /*i*/, long /*id*/)
{
  return (int) Vector::NoMarker;
}

int VaSemblanceHorizonsData::getLineStyle(long /*id*/)
{
  return _line_style;
}

int VaSemblanceHorizonsData::usesMarkers()
{
  return 0;
}
