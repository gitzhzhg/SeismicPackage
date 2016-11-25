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

//---------------------- vector_table.cc ------------------------------//
//---------------------- vector_table.cc ------------------------------//
//---------------------- vector_table.cc ------------------------------//

//-----------    first version by April 21, 1994 ----------------------//

//        implementation file for the VectorTable class



#include "table/vector_table.hh"
#include "draw_mod.h"



//-------------------- constructor ----------------------------//


VectorTable :: VectorTable( Vector *vectorData ) : TableBase(vectorData->getData()),
              _vectorData(vectorData) 
            
{

// set or read initial values here:

    ModBaseData *modelData  = (ModBaseData *) vectorData->getData();
    _numPts  = modelData->getNumPts() ;
    _nKey    = modelData->getnumkeys() + 1;
    _width   = vectorData->getWidth();
    strncpy(_name , vectorData->getName(), 15);
    strncpy(_color, vectorData->getColor(), 15);
    _sKeyFlg = _tKeyFlg = _uKeyFlg = 0;

    _n = 0;
    strncpy(_varName[_n], "X:HD17  ",  9);
    _data_type[_n] = 1;     
    _switch[_n] = 1;     
    _nchar[_n]  = 6;         
    _ndec[_n]   = 0;     
    _n ++ ;

    strncpy(_varName[_n], "T:Sec   ",  9);
    _data_type[_n] = 1;     
    _switch[_n] = 1;     
    _nchar[_n]  = 6;         
    _ndec[_n]   = 3;         
    _n ++ ;

    if(modelData->getshdr() != 0)    
       {
        strncpy(_varName[_n], "Skey    ",  9);
        _data_type[_n] = 1;     
        _switch[_n] = 1;     
        _nchar[_n]  = 6;         
        _ndec[_n]   = 0; 
        _sKeyFlg = 1;
       _n ++ ;        
       } 
    if(modelData->getthdr() != 0)
       {
        strncpy(_varName[_n], "Tkey    ",  9);
        _data_type[_n] = 1;     
        _switch[_n] = 1;     
        _nchar[_n]  = 6;         
        _ndec[_n]   = 0;         
        _tKeyFlg = 1;
       _n ++ ;        
       } 
    if(modelData->getshdr() != 0)
       {
        strncpy(_varName[_n], "UserData",  9);
        _data_type[_n] = 1;     
        _switch[_n] = 1;     
        _nchar[_n]  = 6;         
        _ndec[_n]   = 2;         
        _uKeyFlg = 1;
       } 

     int ip1;
     for (int i = 0 ; i< _numPts ; i++ )
        {
         _x[i] = modelData->getX(i);
         _y[i] = modelData->getY(i);
         if(modelData->getshdr( ) != 0) _sKey[i] = modelData->getS(i);
         if(modelData->getthdr( ) != 0) _tKey[i] = modelData->getT(i);
         if(modelData->getUser(0) != 0) _userData[i] = modelData->getUser(i);
        }
}

void VectorTable :: modDone()  
{
  int i;

  _vectorData = this->getVectorData();

  ModBaseData *modelData  = (ModBaseData *) _vectorData->getData();

   _numPts  =  modelData->getNumPts()       ;
   _nKey    = modelData->getnumkeys() + 1   ;
   _width   = _vectorData->getWidth()       ;
   strcpy( _name , _vectorData->getName() ) ;
   strcpy( _color, _vectorData->getColor() );

  for ( i = 0; i < _numPts ; i++)
      {
         _x[i] = modelData->getX(i);
         _y[i] = modelData->getY(i);
         if(modelData->getshdr( ) != 0) _sKey[i] = modelData->getS(i);
         if(modelData->getthdr( ) != 0) _tKey[i] = modelData->getT(i);
         if(modelData->getUser(0) != 0) _userData[i] = modelData->getUser(i);
       }

        
  wbox_update(); 
}

//----------------------- destructor ------------------------//

VectorTable::~VectorTable(void)
{
}

//--------------- constants and static variables ------------------//

static long zero = 0, one = 1, two = 2, P26 = 26, M26 = -26;

//--------------------------- traps ----------------------------//


 static void trap( void *box,  long *ident,  long *index,   
                   char *text, long *nread,  char *endkey )            

{
  float xx , yy , x1 , x2 , y1 , y2, x3, y3 ;

  if(!strcmp("REDRAW", endkey)) return;   // box not valid.
//  cout << " ident=" << *ident << "  index=" 
//       << *index << "  nread=" << *nread << "  endkey=" 
//       << endkey << " \n  ";


   VectorTable  *tableData;
   Vector      *vectorData;
   BaseData   *baseData;

   tableData  = (VectorTable*)wbox_get_userdata(box);
   vectorData = tableData->getVectorData();
   ModBaseData *modelData  = (ModBaseData *) vectorData->getData();


  // int Index = max(*index - 1, 0);
   int Index;
   Index = (*index-1 <0) ? 0 : *index - 1;



//----------- Traps for insert, remove, and replace -------

  if      (*ident == 11 && strcmp(text, " ") != 0 ) 
     {
      vectorData->setName(text);     
      cout << "Name = " << tableData->getName() << endl<<"\n";
     } 
  else if (*ident == 12 && strcmp(text, " ") != 0) 
     {
      vectorData->setColor( text );        
      cout << "Color = " << tableData->getColor() << text << endl<<"\n";
     } 
  else if (*ident == 13 && strcmp(text, " ") != 0) 
     {   
      int wt = tableData->getWidth();
      vectorData->setWidth( wt );
      cout << "Width = " <<  tableData->getWidth()
           << vectorData->getWidth()  << endl<< "\n";
     } 
/*  else if (*ident == 14 && strcmp(text, " ") != 0) 
     {   
      vectorData->setStyle(tableData->getStyle(   ));
      cout << "Style = " <<  tableData->getStyle() << endl<< "\n";
     } 
  else if (*ident == 15 && strcmp(text, " ") != 0) 
     {   
      vectorData->setMarker(tableData->getMarker(), tableData->getMarkerSize(),
                            tableData->getMarkerLineWidth());
      cout << "Marker = " <<  tableData->getMarker() << endl<< "\n";
     } 
  else if (*ident == 16) 
     {   
      vectorData->setLabel(tableData->getLabel(), tableData->getFont());
      cout << "Label = " <<  tableData->getLabel() << endl<< "\n";
      cout << "Font  = " <<  tableData->getFont()  << endl<< "\n";
     } 

*/
 
  if ( *index == tableData->getNumPts() +1 )
     {
      x2 = modelData->getX( Index-2 ); 
      y2 = modelData->getY( Index-2 );
      x3 = modelData->getX( Index-1 );
      y3 = modelData->getY( Index-1 );
      xx = 2.*x3 - x2; 
      yy = 2.*y3 - y2;
      tableData->setx( Index, xx );
      tableData->sety( Index, yy );
      modelData->insert (Index, 1, &xx, &yy, NULL, NULL, NULL);
     } 
    

  if      (strcmp("INSERTED", endkey ) ==0 ) 
     {
      x2 = modelData->getX( Index ); 
      y2 = modelData->getY( Index );
      if(*index == 1) 
         {
          x3 = modelData->getX( Index+1 );
          y3 = modelData->getY( Index+1 );
          xx = 2.*x2 - x3; 
          yy = 2.*y2 - y3;
         }
      else 
         {
         x1 = modelData->getX(  Index-1 );
         y1 = modelData->getY(  Index-1 );
         xx = 0.5*( x1 + x2 ) ;
         yy = 0.5*( y1 + y2 ) ;
         }

      tableData->setx( Index, xx );
      tableData->sety( Index, yy );
      modelData->insert (Index, 1, &xx, &yy, NULL, NULL, NULL);
      cout << "insert x=" << xx  
           << " y=" << yy << endl<< "\n";
     }
  else if (strcmp("REMOVED",  endkey ) ==0 )
     {
      modelData->remove (Index, 1); 
      cout << "remove x=" << xx
           << " y=" << yy << endl<< "\n";
     }

  else if ( *nread > 0 ) // &&  *index < modelData->getNumPts())
     {
      xx = tableData->getx(Index);
      yy = tableData->gety(Index);
      modelData->replace (Index, 1, 1, &xx , &yy , NULL, NULL, NULL);

      cout << "replace  x=" << xx  << " y=" << yy 
           << endl<< "\n";
     }
 
}

//---------------------- contents -------------------------------//

void VectorTable::contents(void)
{

//  wbox_message("   Vector Horizon Location");


    wbox_blank_line();

// Non_editable horizon name and type disply:

   wbox_creg2 (trap, 11, "Horizon Name ", &two,  _name,  &one,  3,  5,  25,   0   );
   wbox_creg2 (trap, 12, "Horizon Color", &two,  _color, &one,  4,  5,  25,   0   );
   wbox_ireg2 (trap, 13, "Line Width",    &two, &_width, &one,  5,  5,  5,    0   );
           //  TRAP  ID   LABEL           SW1    VARI    SW2   Row COL NCHAR NDEC

   _n = _numPts + 10;
   wbox_blank_line();
   wbox_rega(&_numPts,   &_n,      0,    _n     );
          //   N         NMAX      ROW   MAXROWS

      wbox_frega(trap, 1, _varName[0], &zero, _x,  &P26, 0,  _nchar[0], _ndec[0]);
      wbox_frega(trap, 2, _varName[1], &zero, _y,  &P26, 0,  _nchar[1], _ndec[1]);      

  if ( _sKeyFlg == 1 )
     {
      wbox_frega(trap, 3, _varName[2], &zero, _y,  &P26, 0,  _nchar[2], _ndec[2]);      
     }

   if( _tKeyFlg == 1 )  
     {
      wbox_frega(trap, 4, _varName[3], &zero, _y,  &P26, 0,  _nchar[3], _ndec[3]);      
     }

   if( _uKeyFlg == 1 ) 
     {
      wbox_frega(trap, 5, _varName[4], &zero, _y,  &P26, 0,  _nchar[4], _ndec[4]);      
     }
}

// ---------- end ----------
