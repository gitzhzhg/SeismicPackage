function itemValue = ocget( object, item, arg3)

% ocget is part of the Earth Object system and is used to get fields from
% container objects. The first column of a container object is identical to
% a gridded object up through 'username'.
%
% by G.F. Margrave, November 1993.
% Updated July 1997
%
% NOTE: OCGET will not function with version 4.x.
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

itemValue=[];

if( nargin < 3)
	datanum=-999;
	dataname=[];
	datatype=[];
end

if( nargin == 3)
	if ischar(arg3)
		datanum=-999;
		dataname=arg3;
		datatype=arg3;
	else
		datanum=arg3;
		dataname=[];
		datatype=[];
	end
end

[m,n]=size(object.data);
if( ischar(item) )
	item=strunpad(item);
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

%name
if strcmp(item,'name')
	itemValue = object.name;
  	return;
end

% test for fieldnamestypes                    
if strcmp(item,'fieldnamestypes')
    jend=0; jstart=1;
    for j=1:n
    	[mm,nn] = size( object.data(j).dataname );
    	name = object.data(j).dataname;
    	jend = jend + nn;
    	type = object.data(j).datadatatype;
    	
    	if j~=n
    		itemValue(jstart:jend) = name(1:nn);
    		itemValue(jend+1) = '_';
    		itemValue((jend+2):(jend+5)) = type;
    		itemValue((jend+6)) = '|';
    		jstart = jend + 7;
    	else
    		itemValue(jstart:jend) = name(1:nn);
    		itemValue(jend+1) = '_';
    		itemValue((jend+2):(jend+5)) = type;
    	end
    	jend = jstart - 1;
    	
    end
    itemValue = char(itemValue);
    return;
end	

% test for fieldnames				
if strcmp(item,'fieldnames')
    jend=0; jstart=1; inc=0; k=1:n;
    if( ~isempty(datatype) )
      k=[];
      for j=1:n
        if( strcmp(datatype,object.data(j).datadatatype) )
          inc=inc+1;
      	  k(inc)=j;
        end
      end
    end
    for j=k
         [mm,nn] = size( object.data(j).dataname );
         name = object.data(j).dataname;
         jend = jend + nn;
         if j~=max(k)
        	itemValue(jstart:jend) = name(1:nn);
   		itemValue(jend+1) = '|';
   		jstart = jend + 2;
   	 else
   		itemValue(jstart:jend)=name(1:nn);
   	 end
   	 jend = jstart -1;
    end   	
  itemValue=char(itemValue);
  return;
end 
    		
% test for namesmatrix
if strcmp(item,'namesmatrix')     
% initialize
    itemValue=[]; nn=0; inc=0; k=1:n;
    if( ~isempty(datatype) )
      k=[];
      for j=1:n
        if( strcmp(datatype,object.data(j).datadatatype) )
          inc=inc+1;
      	  k(inc)=j;
        end
      end
    end
    for j=k
      nam = object.data(j).dataname;
      itemValue = strvcat(itemValue,nam);
    end
  return;
end

% test for typesmatrix 
if strcmp(item,'typesmatrix')
    for j=1:n
        name = strpad( object.data(j).datadatatype,4 );
        itemValue(j,1:length(name))=name;
    end
   itemValue=char(itemValue);
   return;
end

% test for contents 
if strcmp(item,'contents')
	% slow method, but gives good result
 	totbytes = 0;
	[bk,ck] = size( char(fieldnames(object.data)) );
	tlen = ck + 10 + 4 + 1 + 4 + 8;
	itemValue(1:n+1,1:tlen) = ' ';
	for j=1:n
		gg = object.data(j).dataname;
        pp = getfield(object.data,{j},gg);
		%A=whos('pp'); 
		bytes=8*prod(size(pp));
		itemValue(j,1:length(gg)) = gg;
		itemValue(j,ck+20:ck+23) = object.data(j).datadatatype;
		itemValue(j,ck+25:ck+28) = object.data(j).dataobjtype;
		sizelen = length( num2str(bytes) ) - 1;
		itemValue(j,ck+30:ck+30+sizelen) = num2str(bytes);
		sizelen = ck + 30 + sizelen;
		itemValue(j,sizelen+2:sizelen+6) = 'bytes';
		totbytes = totbytes + bytes;
	end
	sizelen = length( num2str(totbytes) ) - 1;
	itemValue(n+1,1:12) = 'Total size: ';
	itemValue(n+1,13:13+sizelen) = num2str(totbytes);
	sizelen = 13 + sizelen;
	itemValue(n+1,sizelen+1:sizelen+6) = ' bytes';
	itemValue=deblank(char(itemValue));
	return;
end

%test for numobjects 
if( strcmp(item,'numobjects') )
	itemValue=n;
	return;
end

%test for dataname 
if( strcmp(item,'dataname') )
	if( isempty(dataname) )
	    if( datanum > n )
	    	error(' Object number out of range');
	    else
		itemValue = object.data(datanum).dataname;
		return;
	    end
	else
	    for j=1:n
		if( strcmp(object.data(j).dataname,dataname) )
		    itemValue = object.data(j).dataname;
		    return;
		 end
	    end
	end
end

%test for data create date 
if( strcmp(item,'datacreate') )
	error('Container objects do not keep creation date for subobjects');
end

%test for data mod date 
if( strcmp(item,'datamodified') )
	if( isempty(dataname) )
	    if( datanum > n )
	    	error(' Object number out of range');
	    else
		itemValue = object.data(datanum).datamodified;
		return;
	    end
	else
	    for j=1:n
		if( strcmp(object.data(j).dataname,dataname) )
		    itemValue = object.data(j).datamodified;
		    return;
		 end
	    end
	end
end	

%test for data protect flag 
if( strcmp(item,'protect') )
	if( isempty(dataname) )
	    if( datanum > n )
	    	error(' Object number out of range');
	    else
	    	if( object.data(datanum).protect == 1)
			itemValue = 'on';
			return;
		else
			itemValue = 'off';
			return;
		end		
	    end
	else
	    for j=1:n
		if( strcmp(object.data(j).dataname,dataname) )
		    if( object.data(j).protect == 1 )
		    	itemValue = 'on';
		    	return;
		    else
		    	itemValue = 'off';
		        return;
		    end
		end
	    end
	end
end	

% check for dataobjtype 
if( strcmp(item,'dataobjtype') )
	if( isempty(dataname) )
	    if( datanum > n )
	    	error(' Object number out of range');
	    else
		itemValue = object.data(datanum).dataobjtype;
		return;
	    end
	else
	    for j=1:n
		if( strcmp(object.data(j).dataname,dataname) )
		    itemValue = object.data(j).dataobjtype;
		    return;
		 end
	    end
	end

	% return null if we don't find it
	itemValue=[];
	return;
end

% check for datadatatype
if( strcmp(item,'datadatatype') )
	if( isempty(dataname) )
	    if( datanum > n )
	    	error(' Object number out of range');
	    else
		itemValue = object.data(datanum).datadatatype;
		return;
	    end
	else
	    for j=1:n
		if( strcmp(object.data(j).dataname,dataname) )
		    itemValue = object.data(j).datadatatype;
		    return;
		end
	    end
	end
end

% check for data
if( nargin == 2 )
      for j=1:n
  	if( strcmp(item,object.data(j).dataname) )
	        itemValue = getfield(object.data,{j},item);
	        return;
	elseif( isnumeric(item) )
	   if( item>n | item<1 )
	   	error(' field number out of range');
	   else
	   	gg = object.data(item).dataname;
	   	itemValue = getfield(object.data,{item},gg);
	   	return;
	   end
	end
      end
end

if( nargin == 3 )
     if( ischar(item) & ischar(datatype) )
       for j=1:n
         if( strcmp(item,object.data(j).dataname) )
             if( strcmp(datatype,object.data(j).datadatatype) )
   	       itemValue = getfield(object.data,{j},item);
   	       return;
   	     end
         end
       end
     elseif( ischar(item) & isempty(datatype) )
         itemValue = getfield(object.data,{datanum},item);
         return;
     elseif( ~ischar(item) & ischar(datatype) )
	 check = objget(object,'typesmatrix'); inc=1; ind=[];
	 for j=1:n
	   if( strcmp( check(j,:),datatype ) );
	     ind(inc) = j;
	     inc = inc + 1;
	   end
	 end
	 gg = object.data(ind(item)).dataname;
	 itemValue = getfield(object.data,{ind(item)},gg);
	 return;
     end 
end