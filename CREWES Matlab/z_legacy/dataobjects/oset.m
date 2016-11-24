function object = oset(objin, item, value, arg4)
%
% object = objset( objin, item, value);
% object = objset( objin, item, value, datanum);
% object = objset( objin, item, value, dataname);
%
% OBJSET accepts an EarthObject (created by RANDOBJ, GRIDOBJ)
% and sets the values of one of it's items. If the
% item already exists, then its values are replaced; otherwise
% a new item is added to the object and its values inserted.
% New items are assumed to be data (as opposed to headers) and must be
% the same size and geometry as other data items already in the object. The
% identity of header fields in the object is fixed so new header information
% cannot be added; however, the existing headers can be changed (updated) at
% any point. A data item may be protected from accidental overwrite by setting its
% protect flag to 'on'. The first form of the command is used most often
% and sets items that are not specific to a particular dataset within the 
% object. Examples: (let myObj be an existing object)
%    myObj = objset(myObj, 'username', 'barney'); ... change the username to barney
%    yourObj = objset(myObj,'username','barney'); ... as above but create a new object
%    myObj = objset(myObj,' Leduc ',newGrid); ... a grid referred to by the matlab
%		variable "newGrid" is put in the object and named ' Leduc '
%    myObj = objset(myObj,'protect','on',' Leduc '); ... the ' Leduc ' grid is 
%		protected.
%    myObj = objset(myObj,'protect','off',' Leduc '); ... the ' Leduc ' grid is 
%		un-protected.
%    myObj = objset(myObj,'protect','on',3); ... the third grid is un-protected.
%
% objin = the variable name of the object to be updated
% item = a string specifying the item to be set (max of 30 characters)
% value = the value(s) of the item
% datanum = the sequential number of one of the data fields in the object
% dataname = string giving the name of one of the data fields in the object
%
% object = the output object. Normally this will be the same matrix as objin
%
% For more information type: help earthobj
%
%
% by G.F. Margrave, March 1993
% Updated November 1993
% Updated July 1997
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

%

object = objin; % start with output = input
[m,n]=size(object.data);
if( ischar(item) )
	item=strunpad(item);
end
if nargin < 4
	datanum=-999;
	dataname = [];
end
if nargin < 3
  error(' Three arguments required for objset');
end

if( nargin == 4)
if( ischar(arg4) ) 
	dataname = arg4;
	datanum = -999;
else
	dataname = [];
	datanum = arg4;
	if( datanum > n )
		error(' object has too few datafields for the input parameters');
	end
end
end

objectTypeSize=4;
dataTypeSize=4;

[valrows,valcols]=size(value);

datarows = object.datarows; % get number of rows in data
datacols = object.datacols; % get number of cols in data

% set object version number
if( strcmp('objvernum',item) )
	object.objvernum=value;
	return;
end

%set object creation date
if( strcmp('objcreate',item) )
	object.objcreate = value';
	return;
end

% set object modified date
if( strcmp('objmodified',item) )
	object.objmodified = value;
	return;
end

% set objtype
if strcmp('objtype',item)
     if length(value)>objectTypeSize
        error('invalid objtype length in objset');
     elseif( ~ischar(value) )
     	error('invalid objtype specification in objset');
     end
     object.objtype=value;
     object.objmodified=fix(clock);
     return;
end

%set datatype
if strcmp('datatype',item)
     if((length(value)~=dataTypeSize) &(min(size(value)) ~=1))
        error(' Improper value size in objset');
     end
     object.datatype = value;
     object.objmodified=fix(clock);
     return;
end

% set user name
if( strcmp('username',item) )
	object.username = value';
	object.objmodified=fix(clock);
	return;
end

% set datarows
if strcmp('datarows',item)
   if length(value) ~= 1
      error(' Improper value size in object ');
   end
   object.datarows= value;
   object.objmodified=fix(clock);
   return;
end

% set datacols
if strcmp('datacols',item)
   if length(value) ~= 1
      error(' Improper value size in object ');
   end
   object.datacols= value;
   object.objmodified=fix(clock);
   return;
end

% set delx
if strcmp('delx',item)
   if length(value) ~= 1
      error(' Improper value size in object ');
   end
   object.delx= value;
   object.objmodified=fix(clock);
   return;
end

% set dely
if strcmp('dely',item)
   if length(value) ~= 1
      error(' Improper value size in object ');
   end
   object.dely= value;
   object.objmodified=fix(clock);
   return;
end

% set xnot
if strcmp('xnot',item)
   if length(value) ~= 1
      error(' Improper value size in object ');
   end
   object.xnot= value;
   object.objmodified=fix(clock);
   return;
end

% set ynot
if strcmp('ynot',item)
   if length(value) ~= 1
      error(' Improper value size in object ');
   end
   object.ynot= value;
   object.objmodified=fix(clock);
   return;
end

if( strcmp(item,'globorigin') )
	if( length(value)~= 2)
		error('incorrect value size in objset');
	end
	object.globorigin(1)=value(1);
	object.globorigin(2)=value(2);
	object.objmodified=fix(clock);
	return;
end

if( strcmp(item,'xyscale') )
	if( length(value)>1 )
		error('incorrect value size in objset');
	end
	object.xyscale=value;
	object.objmodified=fix(clock);
	return;
end

if( strcmp(item,'xyrotate') )
	if( length(value)>1 )
		error('incorrect value size in objset');
	end
	object.xyrotate=value;
	object.objmodified=fix(clock);
	return;
end

%set x axis name
if strcmp('xaxisname',item)
	object.xaxisname=value';
	object.objmodified=fix(clock);
   	return;
end

%set y axis name
if strcmp('yaxisname',item)
	object.yaxisname=value';
	object.objmodified=fix(clock);
   	return;
end

% set north azimuth
if strcmp('north',item)
	if( length(value) ~= 1)
		error(' Improper value size in objset');
	end
	object.north=value;
	object.objmodified=fix(clock);
   	return;
end

% set east azimuth
if strcmp('east',item)
	if( length(value) ~= 1)
		error(' Improper value size in objset');
	end
	object.east=value;
	object.objmodified=fix(clock);
   	return;
end

%set name
if strcmp('name', item)
    	object.name = value';
     	object.objmodified=fix(clock);
   	return;
end

% set a data name 
if( strcmp(item,'dataname') )
        if( nargin < 4)
                error(' data field must be specified ');
        end
        if( isempty(dataname) )
          gg1=object.data(datanum).dataname;
          dat=getfield(object.data,{datanum},gg1);
          object.data=rmfield(object.data,gg1);
          gg1=value;
          object.data=setfield(object.data,{datanum},gg1,dat);
          object.data(datanum).dataname=value;
	  object.objmodified=fix(clock);
          return;
        else
          for j=1:n;
            if( strcmp(dataname,object.data(j).dataname) )
              gg1=object.data(j).dataname;
              object.data(j).dataname=value;
              dat=getfield(object.data,{j},gg1);
              object.data=setfield(object.data,{j},value,dat);
              object.data=rmfield(object.data,gg1);
              object.objmodified=fix(clock);
              return;
            end
          end
        end
end

% set a data creation date
if( strcmp(item,'datacreate') )
	if( nargin < 4)
		error(' data field must be specified ');
	end
	if( isempty(dataname) )
	  object.data(datanum).datacreate=value;
	  object.objmodified=value;
   	  return;
   	else
   	  for j=1:n
   	    if( strcmp(dataname,object.data(j).dataname) )
   	      object.data(j).datacreate=value;
   	      gg=object.data(j).dataname;
   	      return;
   	    end
   	  end
   	end
end

% set the data mod data
if( strcmp(item,'datamodified') )
        if( nargin < 4)
                error(' data field must be specified ');
        end
        if( isempty(dataname) )
          object.data(datanum).datamodified=value;
	  object.objmodified=value;
          return;
        else
          for j=1:n
            if ( strcmp(dataname,object.data(j).dataname) )
              object.data(j).datamodified=value;
              object.objmodified=value;
              return;
            end
          end
        end          
end

% set the data protect field
if( strcmp(item,'protect') )
        if( nargin < 4)
                error(' data field must be specified ');
        end
	if( ~strcmp(value,'on') & ~strcmp(value,'off') )
		error(' invalid protection specification');
	end
	if( strcmp(value,'on') ) value = 1;
	else value = 0; end
	if( isempty(dataname) )		
          object.data(datanum).protect = value;
          return;
        else
          for j=1:n
            if (strcmp(dataname,object.data(j).dataname) )
              object.data(j).protect = value;
              return;
            end
          end
        end
end

% if data names are the same, replace data only (unless protected)
gg = getfield(object.data(1),'dataname');
if( ~isempty(gg) )
	gg = size(getfield(object.data(1),gg));
else 
	gg = [0 0];
end
% [m,n]=size(object.data); hh=[]; wflag=[]; flag=[]; %$$$$$$$$$$$$$$
% faa=char(fieldnames(object)); [faam,faan]=size(faa); 
% fbb=char(fieldnames(object.data)); [fbbm,fbba]=size(fbb);

% check to see that new data name isn't same as existing fieldname
% for ja=1:faam;
%   aflag=strcmp( item,deblank(faa(ja,:)) );
%   if(aflag) break; end
% end
% 
% for jb=1:fbbm;
%   bflag=strcmp( item,deblank(fbb(jb,:)) );
%   if(bflag) break; end
% end
[m,n]=size(object.data);
hh=[];wflag=[];flag=[];
%search through data fields for match
for j=1:n; %$$$$$$$$$$$$$$
  if strcmp(item,object.data(j).dataname)
      hh=j;
  end
end

% check for matching data size 
if( (gg(1)==0 & gg(2)==0) | (valrows==gg(1) & valcols==gg(2)) ) 
  flag=1;
else
  error(' data size must be the same for all data items in object');
end 

% for j=1:n; %$$$$$$$$$$$$$$
%   if strcmp(item,object.data(j).dataname)
%       hh=j;
%   end
% end

if ( ~isempty(hh) & flag==1 ) % modify data in existing field
      if( object.data(hh).protect== 1 )
         error(' attempt to change protected data in objset');
      else     
	 dd = getfield(object.data,{hh},'dataname');
	 object.data = setfield(object.data,{hh},dd,value);
	 object.data(hh).datamodified = fix(clock);
	 object.data(hh).datacreate = fix(clock);
	 return;
      end
      %elseif( aflag==0 & bflag==0 & flag==1 ) % new data in new field
  else %new data in new field
	 if( gg(1)==0 & gg(2)==0 )
	 	n=0;
	 end
  	 object.data(n+1).protect = 0;
  	 object.data(n+1).dataname = item;
   	 object.data = setfield(object.data,{n+1},item,value);
  	 object.data(n+1).datacreate = fix(clock);
  	 object.data(n+1).datamodified = fix(clock);
   	 return;
end
  
% NOTE: do not need same data size for new fields, but for
% compatibility with existing functions, this requirement 
% is maintained