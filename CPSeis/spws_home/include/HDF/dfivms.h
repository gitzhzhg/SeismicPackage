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
/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/* $Id: dfivms.h,v 1.12 1996/01/18 05:02:09 sxu Exp $ */

#ifndef DFIVMS_H
#define DFIVMS_H

#define DFANIaddentry   _DFANIaddentry
#define DFANIgetann     _DFANIgetann
#define DFANIgetannlen  _DFANIgetannlen
#define DFANIlablist    _DFANIlablist
#define DFANIlocate     _DFANIlocate
#define DFANIopen       _DFANIopen
#define DFANIclear      _DFANIclear
#define DFANIputann     _DFANIputann
#define DFANclear       _DFANclear
#define DFANgetdesc     _DFANgetdesc
#define DFANgetdesclen  _DFANgetdesclen
#define DFANgetlabel    _DFANgetlabel
#define DFANgetlablen   _DFANgetlablen
#define DFANlablist     _DFANlablist
#define DFANlastref     _DFANlastref
#define DFANputdesc     _DFANputdesc
#define DFANputlabel    _DFANputlabel
#define DFANaddfid      _DFANaddfid
#define DFANgetfidlen   _DFANgetfidlen
#define DFANgetfid      _DFANgetfid
#define DFANgetfdslen   _DFANgetfdslen
#define DFANgetfds      _DFANgetfds
#define DFANaddfds      _DFANaddfds
#define DFCimcomp       _DFCimcomp
#define DFCrle          _DFCrle
#define DFCunimcomp     _DFCunimcomp
#define DFCunrle        _DFCunrle
#define DFDIgetgroup    _DFDIgetgroup
#define DFDIputgroup    _DFDIputgroup
#define DFIc2fstr       _DFIc2fstr
#define DFIcheck        _DFIcheck
#define DFIemptyDD      _DFIemptyDD
#define DFIerr          _DFIerr
#define DFIf2cstring    _DFIf2cstring
#define DFIfind         _DFIfind
#define DFIfreespace    _DFIfreespace
#define DFIgetspace     _DFIgetspace
#define DFImemcopy      _DFImemcopy
#define DFIseedDDs      _DFIseedDDs
#define DFIstrncpy      _DFIstrncpy
#define DFPgetpal       _DFPgetpal
#define DFPlastref      _DFPlastref
#define DFPnpals        _DFPnpals
#define DFPputpal       _DFPputpal
#define DFPaddpal       _DFPaddpal
#define DFPreadref      _DFPreadref
#define DFPrestart      _DFPrestart
#define DFPwriteref     _DFPwriteref
#define DFR8Iopen       _DFR8Iopen
#define DFR8Iputimage   _DFR8Iputimage
#define DFR8Iriginfo    _DFR8Iriginfo
#define DFR8addimage    _DFR8addimage
#define DFR8getdims     _DFR8getdims
#define DFR8getimage    _DFR8getimage
#define DFR8getrig      _DFR8getrig
#define DFR8lastref     _DFR8lastref
#define DFR8nimages     _DFR8nimages
#define DFR8putimage    _DFR8putimage
#define DFR8putrig      _DFR8putrig
#define DFR8readref     _DFR8readref
#define DFR8restart     _DFR8restart
#define DFR8setpalette  _DFR8setpalette
#define DFR8writeref    _DFR8writeref
#define DFSDIclear      _DFSDIclear
#define DFSDIgetdata    _DFSDIgetdata
#define DFSDIgetslice   _DFSDIgetslice
#define DFSDIopen       _DFSDIopen
#define DFSDIputdata    _DFSDIputdata
#define DFSDIputslice   _DFSDIputslice
#define DFSDIsdginfo    _DFSDIsdginfo
#define DFSDadddata     _DFSDadddata
#define DFSDclear       _DFSDclear
#define DFSDendslice    _DFSDendslice
#define DFSDgetNT       _DFSDgetNT
#define DFSDgetdata     _DFSDgetdata
#define DFSDgetdatalen  _DFSDgetdatalen
#define DFSDgetdatastrs _DFSDgetdatastrs
#define DFSDgetdimlen   _DFSDgetdimlen
#define DFSDgetdims     _DFSDgetdims
#define DFSDgetdimscale _DFSDgetdimscale
#define DFSDgetdimstrs  _DFSDgetdimstrs
#define DFSDgetrange    _DFSDgetrange
#define DFSDgetsdg      _DFSDgetsdg
#define DFSDgetslice    _DFSDgetslice
#define DFSDlastref     _DFSDlastref
#define DFSDndatasets   _DFSDndatasets
#define DFSDpre32       _DFSDpre32
#define DFSDputdata     _DFSDputdata
#define DFSDputsdg      _DFSDputsdg
#define DFSDputslice    _DFSDputslice
#define DFSDrestart     _DFSDrestart
#define DFSDsetNT       _DFSDsetNT
#define DFSDsetdatastrs _DFSDsetdatastrs
#define DFSDsetdims     _DFSDsetdims
#define DFSDsetdimscale _DFSDsetdimscale
#define DFSDsetdimstrs  _DFSDsetdimstrs
#define DFSDsetlengths  _DFSDsetlengths
#define DFSDsetorder    _DFSDsetorder
#define DFSDsetrange    _DFSDsetrange
#define DFSDsettype     _DFSDsettype
#define DFSDstartslice  _DFSDstartslice
#define DFSDreadref     _DFSDreadref
#define DFaccess        _DFaccess
#define DFIaccess       _DFIaccess
#define DFclose         _DFclose
#define DFdel           _DFdel
#define DFdescriptors   _DFdescriptors
#define DFdiget         _DFdiget
#define DFdiput         _DFdiput
#define DFdiread        _DFdiread
#define DFdisetup       _DFdisetup
#define DFdiwrite       _DFdiwrite
#define DFdup           _DFdup
#define DFerrno         _DFerrno
#define DFfind          _DFfind
#define DFgetcomp       _DFgetcomp
#define DFgetelement    _DFgetelement
#define DFishdf         _DFishdf
#define DFnewref        _DFnewref
#define DFnumber        _DFnumber
#define DFopen          _DFopen
#define DFputcomp       _DFputcomp
#define DFputelement    _DFputelement
#define DFread          _DFread
#define DFseek          _DFseek
#define DFsetfind       _DFsetfind
#define DFstart         _DFstart
#define DFstat          _DFstat
#define DFupdate        _DFupdate
#define DFwrite         _DFwrite
#define DFfindnextref   _DFfindnextref
#define DF24putimage    _DF24putimage
#define DF24addimage    _DF24addimage
#define DF24getdims     _DF24getdims
#define DF24getimage    _DF24getimage
#define DF24reqil       _DF24reqil
#define DF24setdims     _DF24setdims
#define DF24setil       _DF24setil
#define DF24restart     _DF24restart
#define DF24readref     _DF24readref
#define DFGRIgetdims    _DFGRIgetdims
#define DFGRIgetimlut   _DFGRIgetimlut
#define DFGRIopen       _DFGRIopen
#define DFGRIreqil      _DFGRIreqil
#define DFGRIriginfo    _DFGRIriginfo
#define DFGRaddimage    _DFGRaddimage
#define DFGRaddlut      _DFGRaddlut
#define DFGRaddrig      _DFGRaddrig
#define DFGRgetimage    _DFGRgetimage
#define DFGRgetimdims   _DFGRgetimdims
#define DFGRgetlut      _DFGRgetlut
#define DFGRgetlutdims  _DFGRgetlutdims
#define DFGRgetrig      _DFGRgetrig
#define DFGRreqimil     _DFGRreqimil
#define DFGRreqlutil    _DFGRreqlutil
#define DFGRsetcompress _DFGRsetcompress
#define DFGRsetimdims   _DFGRsetimdims
#define DFGRsetlut      _DFGRsetlut
#define DFGRsetlutdims  _DFGRsetlutdims
#define DFUfptoimage    _DFUfptoimage
#define Hclose          _Hclose
#define Hopen           _Hopen
#define Hnumber         _Hnumber
#define Vdelete         _Vdelete
#define Vnrefs          _Vnrefs
#define Vfind           _Vfind
#endif /* DFIVMS_H */
