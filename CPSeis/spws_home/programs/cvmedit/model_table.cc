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
//---------------------- model_table.cc ------------------------------//

//-------------modified version  May 9, 1994 -----Dapeng Wang------//

//   



#include "sl/sl_databox.hh"
#include "oprim/ll_base_data.hh"
#include "table/model_table.hh"
#include "oprim/modbase_data.hh"



//-------------------- constructor ----------------------------//



ModelTable :: ModelTable( ModBaseData *modelData, char *xTitle, 
                          char *yTitle, char *sTitle,
                          char *tTitle, char *uTitle ,long ident) 
            : TableBase( modelData ), _modelData(modelData),_ident(ident)
           
{

// set or read initial values here:

    _nmax   = MAXPTS;
    _numPts = modelData->getNumPts() ;
    if(_numPts > _nmax)  _numPts = MAXPTS;
    _nKey   = modelData->getnumkeys() + 1;
    _isSkey = _isTkey =_isUkey = 0;     
    _n = 0;

     if ( xTitle == " ") strncpy(_varName[_n], "X     ",   9);
     else                strncpy(_varName[_n], xTitle,    9);
    _data_type[_n] = 1;     
    _switch[_n] = 1;     
    _nchar[_n]  = 8;         
    _ndec[_n]   = 1;     
    _n ++ ;

     if ( yTitle == " ") strncpy(_varName[_n], "Y     ",   9);
     else                 strncpy(_varName[_n], yTitle,    9);
    _data_type[_n] = 1;     
    _switch[_n] = 1;     
    _nchar[_n]  = 8;         
    _ndec[_n]   = 3;         
    _n ++ ;

    if(modelData->getshdr() != 0)    
       {
        if ( sTitle == NULL) strncpy(_varName[_n], "SKey   ",  9);
        else                 strncpy(_varName[_n], sTitle,    9);
        _data_type[_n] = 1;     
        _switch[_n] = 1;     
        _nchar[_n]  = 8;         
        _ndec[_n]   = 2; 
        _isSkey = 1;
       _n ++ ;        
       } 
    if(modelData->getthdr() != 0)
       {
        if ( tTitle == " ") strncpy(_varName[_n], "TKey   ",  9);
        else                 strncpy(_varName[_n], tTitle,    9);
        _data_type[_n] = 1;     
        _switch[_n] = 1;     
        _nchar[_n]  = 8;         
        _ndec[_n]   = 2;         
        _isTkey = 1;
       _n ++ ;        
       } 

    if( modelData->isUserData() )
       {
        if ( uTitle == " ") strncpy(_varName[_n], "UserData",  9);
        else                 strncpy(_varName[_n], uTitle,     9);

        _data_type[_n] = 1;     
        _switch[_n] = 1;     
        _nchar[_n]  = 8;         
        _ndec[_n]   = 2;         
        _isUkey = 1;
      } 



     for (int i = 0; i< _numPts; i++)
        {
         _x[i] = modelData->getX(i);
         _y[i] = modelData->getY(i);
         if(_isSkey == 1) _sKey[i]     = modelData->getS(i);
         if(_isTkey == 1) _tKey[i]     = modelData->getT(i);
         if(_isUkey == 1) _userData[i] = modelData->getUser(i);
        }

}


void ModelTable :: replace( ModBaseData *newData )
{

  if(newData == _modelData || newData == NULL) return;
  if(_baseDatas->find(_modelData))
   { if (_modelData) removeData((BaseData *) _modelData);
   }
  _modelData  = newData;
  addData((BaseData *) _modelData);

  modDone( (BaseData *) newData, _ident);
}  

void ModelTable :: modDone(BaseData * /*data*/,long /*ident*/)  
{
  int i;

  ModBaseData *modelData  = (ModBaseData *) getModBaseData();

   _numPts =  modelData->getNumPts() ;
   if(_numPts > _nmax)
    { printf("table overflow in ModelTable: call R. Day\n");
      _numPts = _nmax; 
    }
   _nKey    = modelData->getnumkeys() + 1;

  for ( i = 0; i < _numPts ; i++)
      {
         _x[i] = modelData->getX(i);
         _y[i] = modelData->getY(i);
         if(_isSkey == 1) _sKey[i]     = modelData->getS(i);
         if(_isTkey == 1) _tKey[i]     = modelData->getT(i);
         if(_isUkey == 1) _userData[i] = modelData->getUser(i);
       }

        
  wbox_update(); 
}




//----------------------- destructor ------------------------//


ModelTable::~ModelTable(void)
{
}

//--------- constants and static variables ------------------//

static long zero = 0, one = 1; /*two = 2, P26 = 26, M26 = -26;*/


//------------------------ traps ----------------------------//


 static void trap( void *box,  long * /*ident*/,  long *index,   
                   char * /*text*/, long *nread,  char *endkey )            

{
  if(!strcmp("REDRAW", endkey)) return;   // box not valid.


   float        xx , yy , x1 , x2 , y1 , y2, x3, y3 ;
   float        ss,  tt, uu, s1, s2, s3, t1,t2,t3, u1,u2,u3;
   int          _npt;
   ModBaseData *modelData;
   ModelTable  *tableData;

   tableData  = (ModelTable*) SLDatabox::getUserData(box);
   modelData  = tableData->getModBaseData();
   _npt =  tableData->getNumPts();
//   int Index = (int)(max(*index - 1, 0));
   int Index =  (*index-1 > 0) ? *index-1: 0;

   //cout << "In TRAP index = " << *index << " Index= " << Index << "\n";
   //cout << "        text=   " << text << "nread = "<< *nread <<" _npt = "<< _npt << "\n" ;
  
   if(*nread == 0 ) return;

   if ( strcmp("INSERTED", endkey ) ==0 ) 
     {
      if ( _npt == 0)
         {
           xx = yy = ss = tt = uu = 0; 
           //cout << "      INsERT npt=0 xx = " << xx << "\n";
         }
      else if ( _npt == 1)
         {
          xx = modelData->getX(0);
          yy = modelData->getY(0);
          if(modelData->getshdr() != 0) ss = modelData->getS(0);
          if(modelData->getthdr() != 0) tt = modelData->getT(0);
          if(modelData->isUserData()  ) uu = modelData->getUser(0);
          //cout << "      insert npt=1 xx = " << xx << "\n";
         }
      else if ( *index == _npt +1 ) // extrapolation at end of array
         {
          x2 = modelData->getX( Index-2 ); 
          y2 = modelData->getY( Index-2 );
          x3 = modelData->getX( Index-1 );
          y3 = modelData->getY( Index-1 );
          xx = 2.*x3 - x2; 
          yy = 2.*y3 - y2;
          tableData->setX( Index, xx );
          tableData->setY( Index, yy );

          if(modelData->getshdr() != 0) 
             {
              s2 = modelData->getS( Index-2 );
              s3 = modelData->getS( Index-1 );
              ss     = 2.*s3 - s2;
              tableData->setS( Index, ss );
             }
           if(modelData->getthdr() != 0) 
             {
              t2 = modelData->getT( Index-2 );
              t3 = modelData->getT( Index-1 );
              tt = 2.*t3 - t2;
              tableData->setT( Index, tt );
             } 
           if(modelData->isUserData()  ) 
             {
              u2 = modelData->getUser( Index-2 );
              u3 = modelData->getUser( Index-1 );
              uu = 2.*u3 - u2;
              tableData->setU( Index, uu );
             } 
            //cout << "     extra at end xx = " << xx << "\n";
           } 

       else if(*index == 1 ) //extrapolation at beginning of array  
          {
            x2 = modelData->getX( Index ); 
            y2 = modelData->getY( Index );
            s2 = modelData->getS( Index );
            t2 = modelData->getT( Index );
            u2 = modelData->getUser( Index );
            x3 = modelData->getX( Index+1 );
            y3 = modelData->getY( Index+1 );
            s3 = modelData->getS( Index+1 );
            t3 = modelData->getT( Index+1 );
            u3 = modelData->getUser( Index+1 );
            xx = 2.*x2 - x3; 
            yy = 2.*y2 - y3;
            ss = 2.*s2 - s3;
            tt = 2.*t2 - t3;
            uu = 2.*u2 - u3;
            //cout << "     explat xx = " << xx << "\n";
           }
        else  // interpolation 
           {
            x1 = modelData->getX(  Index-1 );
            y1 = modelData->getY(  Index-1 );
            s1 = modelData->getS(  Index-1 );
            t1 = modelData->getT(  Index-1 );
            u1 = modelData->getUser(  Index-1 );
            x2 = modelData->getX(  Index );
            y2 = modelData->getY(  Index );
            s2 = modelData->getS(  Index );
            t2 = modelData->getT(  Index );
            u2 = modelData->getUser(  Index );
            xx = 0.5*( x1 + x2 ) ;
            yy = 0.5*( y1 + y2 ) ;
            ss = 0.5*( s1 + s2 ) ;
            tt = 0.5*( t1 + t2 ) ;
            uu = 0.5*( u1 + u2 ) ;
            //cout << "     interp xx = " << xx << "\n";
           }

      tableData->setX( Index, xx );
      tableData->setY( Index, yy );
      if(modelData->getshdr() != 0) tableData->setS( Index, ss );
      if(modelData->getthdr() != 0) tableData->setT( Index, tt );
      if(modelData->isUserData()  ) tableData->setU( Index, uu );

      modelData->insert (Index, 1, &xx, &yy, &ss, &tt, &uu); 

       //cout << "     777 xx = " << xx << "\n";
     }
   else if (strcmp("REMOVED",  endkey ) ==0 )
     {
      modelData->remove (Index, 1); 
       //cout << "     removed row(Index) = " << Index << "\n";
     }
   else // Replace 
     {
       //cout << "    Replace   xx = " << xx << "\n";
      xx = tableData->getX(Index);
      yy = tableData->getY(Index);
      ss = tableData->getS(Index);
      tt = tableData->getT(Index);
      uu = tableData->getU(Index);
      //if (*ident == 1) xx = tableData->getX(Index);
      //if (*ident == 2) yy = tableData->getY(Index);
      //if (*ident == 3) ss = tableData->getS(Index);
      //if (*ident == 4) tt = tableData->getT(Index);
      //if (*ident == 5) uu = tableData->getU(Index);
      modelData->replace (Index, 1, 1, &xx , &yy , &ss, &tt, &uu); 

     }

}

//---------------------- contents -------------------------------//

void ModelTable::contents(void)
{

//  wbox_message("   Model Horizon Location");

    ModBaseData *modelData;

    modelData  = this->getModBaseData();

    wbox_blank_line();

   int _nn = 2;
   wbox_blank_line();
   _n =  _numPts + 10;
   wbox_rega(&_numPts,   &_nmax,      0,    10  );
          //   N         NMAX      ROW   MAXROWS

      wbox_frega(trap, 1, _varName[0], &zero, _x,  &one, 0,  _nchar[0], _ndec[0]);
      wbox_frega(trap, 2, _varName[1], &zero, _y,  &one, 0,  _nchar[1], _ndec[1]);      

  if ( _isSkey == 1 )
     {
      wbox_frega(trap, 3, _varName[_nn], &zero, _sKey,  &one, 0,  _nchar[_nn], _ndec[_nn]);      
      _nn ++;
     }

   if( _isTkey == 1 )  
     {
      wbox_frega(trap, 4, _varName[_nn], &zero, _tKey,  &one, 0,  _nchar[_nn], _ndec[_nn]);      
      _nn ++;
     }

   if( _isUkey == 1 ) 
     {
      wbox_frega(trap, 5, _varName[_nn], &zero, _userData,  &one, 0,  _nchar[_nn], _ndec[_nn]);      
     }
 
}

