function itemValue = oget( object, item, arg3)

% itemValue = objget(object, item)
% itemvalue = objget(object, item, datanum)
% itemvalue = objget(object, item, dataname)
%
% OBJGET performs the inverse operation to OBJSET in that it
% retrieves data from an EarthObject (created by RANDOBJ, GRIDOBJ)
% Most invocations will use the first syntax. Examples:
%	objget(myobject,'name') ... returns the object's name
%	objget(myobject,'x') ... returns the object's x coordinates
%   objget(myobject,'fred') ... returns the data field named fred
%	objget(myobject,5) ... returns the fifth dataset in the object
%
%
% The second syntax is provided to obtain fields specific to of a dataset
% when only its name or number are known. For example, the name of the 5th
% dataset is obtained with:
%   objget(myobject,'dataname',5) ... will return the name of the 5th data grid
% If the returned name is 'peace river' for example, then its last 
% modification date can be obtained by either of:
%   objget(myobject,'datamodified',5); ... 'peace river' is the 5th dataset
%   objget(myobject,'datamodified','peace river'); ... ask for it my name
%
% object = the EarthObject to be interrogated
% item = a string identifying the item to be retrieved, or an integer
%        denoting the index of the item. This last case is useful if 
%        a query such as objget( object, 'fieldnames') has first been
%        made to determine the names of available fields in the object.
%        Any field can then be obtained by simply specifying it's number
%        (the first is number 1 etc...)
% datanum = an integer referring to one of the data fields in the object. 
%           Valid numbers are 1 through m where m is the number of data 
%           fields stored.  
% (and the number of rows returned by objget(myObject,'namesmatrix') )
% dataname = a string giving the name of a dataset stored in the object. 
%            Blanks are important: the string 'leduc' will not match 
%            ' leduc' or 'leduc '
%
% itemValue = the returned value of the item. The form of itemValue
%             may be any of scalar, vector, or matrix depending on what
%             was stored.
%
% by G.F. Margrave, March 1993.
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
  
[m,n]=size(object.data);
if( ischar(item) )
	item=strunpad(item);
end
if( nargin < 3)
	datanum=-999;
	dataname=[];
end

if( nargin == 3)
	if ischar(arg3)
		datanum=-999;
		dataname=arg3;
		ind=find(dataname==1);
		if(length(ind)>0)
			dataname=dataname(1:ind(1)-1);
		end
	else
		datanum=arg3;
		dataname=[];
	end
end
	
if strcmp(item,'objvernum')
	itemValue = object.objvernum;
	return;
end

if strcmp(item,'objcreate')
	itemValue = object.objcreate;
	return;
end

if strcmp(item,'objmodified')
	itemValue = object.objmodified;
	return;
end

if strcmp(item,'objtype')
        itemValue = object.objtype;
        return;
end

% now data type
if strcmp(item,'datatype')
   	itemValue = object.datatype;
    	return;
end

if strcmp(item,'username')
	itemValue = object.username;
	return;
end

datarows = object.datarows; % get number of rows in data
datacols = object.datacols; % get number of cols in data
datasize = datarows*datacols; % total number of data entries

% datarows
if strcmp(item,'datarows')
   itemValue = object.datarows;
   return;
end

%datacols
if strcmp(item,'datacols')
   itemValue = object.datacols;
   return;
end

%delx
if strcmp(item,'delx')
   itemValue = object.delx;
   return;
end

%dely
if strcmp(item,'dely')
   itemValue = object.dely;
   return;
end

%xnot
if strcmp(item,'xnot')
   itemValue = object.xnot;
   return;
end

%ynot
if strcmp(item,'ynot')
   itemValue = object.ynot;
   return;
end

% y
if ( strcmp(item,'y') & strcmp(object.objtype,'grid') )
   itemValue = xcoord(object.ynot,object.dely,object.datarows)';
   return;
end

% x
if ( strcmp(item,'x') & strcmp(object.objtype,'grid') )
   itemValue = xcoord(object.xnot,object.delx,object.datacols);
   return;
end

%globorigin
if strcmp(item,'globorigin')
	itemValue = [object.globorigin(1) object.globorigin(2)];
	return;
end

%xyscale
if strcmp(item,'xyscale')
	itemValue=object.xyscale;
	return;
end

%xyrotate
if strcmp(item,'xyrotate')
	itemValue=object.xyrotate;
	return;
end

%xaxisname
if strcmp(item,'xaxisname')
	itemValue=object.xaxisname;
	return;
end

%yaxisname
if strcmp(item,'yaxisname')
	itemValue=object.yaxisname;
	return;
end

%north
if strcmp(item,'north')
	itemValue = object.north;
	return;
end

%east
if strcmp(item,'east')
	itemValue = object.east;
	return;
end

%test for 'x' on a grid object
if strcmp(object.objtype,'grid')&(strcmp(item,'x'))
  % call a function from the Seismic_Toolbox
  	itemValue = xcoord( object.xnot,object.delx,object.datacols );
  	return;
end

%test for 'xglobal'
if strcmp(item,'xglobal')
	% get the transform parameters
	x1=object.globorigin(1);
	y1=object.globorigin(2);
	factor=object.xyscale;
	ang=object.xyrotate;
	
	% get the x coordinates
	xlocal = object.x;
	
	% branch if rotating
	if( abs(ang)> 100*eps )
		% need y too, if rotating
		ylocal = object.y;
		
		[xglob,yglob]=myrotate(xlocal,ylocal,ang);
		
		% translate and scale
		xglob= xglob*factor+x1;
	else
		xglob = xlocal*factor+x1;
	end
	
	itemValue=xglob;
	return;
end

%test for 'yglobal'
if strcmp(item,'yglobal')
	% get the transform parameters
	x1=object.globorigin(1);
	y1=object.globorigin(2);
	factor=object.xyscale;
	ang=object.xyrotate;
	
	% get the y coordinates
	ylocal = object.y;
	
	% branch if rotating
	if( abs(ang)> 100*eps )
		% need x too, if rotating
		xlocal = objget(object,'x');
		
		[xglob,yglob]=myrotate(xlocal,ylocal,ang);
		
		% translate and scale
		yglob= yglob*factor+y1;
	else
		yglob = ylocal*factor+y1;
	end
	
	itemValue=yglob;
	return;
end

%test for 'xyglobal'
if strcmp(item,'xyglobal')
	% get the transform parameters
	x1=object.globorigin(1);
	y1=object.globorigin(2);
	factor=object.xyscale;
	ang=object.xyrotate;
	
	% get the x&y coordinates
	xlocal = object.x;
	ylocal = object.y;
	
	% branch if rotating
	if( abs(ang)> 100*eps )
		
		[xglob,yglob]=myrotate(xlocal,ylocal,ang);
		
		% translate and scale
		xglob= xglob*factor+x1;
		yglob= yglob*factor+y1;
	else
		xglob= xglob*factor+x1;
		yglob = ylocal*factor+y1;
	end
	
	itemValue=[xglob yglob];
	return;
end

%test for 'xg' on a grid object
if strcmp(object.objtype,'grid') & (strcmp(item,'xg'))
  % call a function from the Seismic_Toolbox
  temp = xcoord( object.xnot,object.delx,object.datacols );
  itemValue = ones(object.datarows,1)*temp;
  return;
end
  
%test for 'y' on a grid object
if strcmp(oget(object,'objtype'),'grid')&(strcmp(item,'y'))
  % call a function from the Seismic_Toolbox
  itemValue = xcoord( object.ynot,object.dely,object.datarows);
  return;
end

%test for 'yg' on a grid object
if strcmp(object.objtype,'grid')&(strcmp(item,'yg'))
  % call a function from the Seismic_Toolbox
  temp = xcoord( object.ynot,object.dely,object.datarows );
  itemValue = temp(:)*ones(1,object.datacols);
  return;
end

%name
if strcmp(item,'name')
   itemValue = object.name;
   return;
end

% test for fieldnames
if strcmp(item,'fieldnames')
    jend=0; jstart=1;
    for j=1:n
    	[mm,nn] = size( object.data(j).dataname );
        name = object.data(j).dataname;
        jend = jend + nn;
        
        if j~=n
   		itemValue(jstart:jend) = name(1:nn);
   		itemValue(jend+1) = '|';
   		jstart = jend + 2; 
   	else
   		itemValue(jstart:jend) = name(1:nn);
   	end
   	jend = jstart-1;
    end
  return;
end

% test for namesmatrix or contents
if strcmp(item,'namesmatrix') | strcmp(item,'contents')
    for j=1:n
    	[mm(j),nn(j)] = size( object.data(j).dataname );
    end
    aa=find( nn==max(nn) );
    for j=1:n
      	name = strpad( object.data(j).dataname, object.data(aa).dataname );
      	itemValue(j,1:length(name))=name;
    end
  return;
end

%test for dataname
if( strcmp(item,'dataname') )
        itemValue = object.data(datanum).dataname;
	return;
end

%test for data create date
if( strcmp(item,'datacreate') )
	itemValue = object.data(datanum).datacreate;
	return;
end

%test for data mod date
if( strcmp(item,'datamodified') )
	if( datanum~=-999 )
		itemValue = object.data(datanum).datamodified;
		return;
	else
		for cc=1:n
			if( strcmp( dataname,object.data(cc).dataname) )
				itemValue = object.data(cc).datamodified;
				return;
			end
		end
	end			
end

%test for data protect flag
if( strcmp(item,'protect') )
	itemValue = object.data(datanum).protect;
	return;
end

% test for data values
for cc=1:n
	if( strcmp(item,object.data(cc).dataname) )
		itemValue = getfield(object.data,{cc},item);
		return;
	elseif( isa(item,'double') )
	   if( item>n | item<1 )
	   	error(' field number out of range');
	   else
		gg = getfield(object.data,{item},'dataname');
		itemValue = getfield(object.data,{item},gg);
		return;
	   end
	end
end

% if the requested item was not found return a null
itemValue=[];