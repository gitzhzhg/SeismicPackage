function object=ocset(objin,item,value,arg4)

% ocset is part of the Earth Object system and is used to set fields in 
% container objects. The first column of a container object is identical to a
% gridded object up through the 'username' field and this function therefore
% calls oset to set those fields.
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


object = objin; % start with output = input
if( ischar(item) )
	item=strunpad(item);
end

if nargin < 4
	if( ischar(item) )
		datanum=[];
		dataname = item;
		datatype = [];
	else
		datanum=item;
		dataname=[];
		datatype=[];
	end
end
if nargin < 3
  error(' Three arguments required for objset');
end

if( nargin == 4)
	if( isnumeric(arg4) )
		datanum=arg4;
		dataname=[];
		datatype = arg4;
	elseif( ischar(arg4) ) 
		dataname = arg4;
		datatype = arg4;
		datanum = -999;
	end
end

[rdat,cdat]=size(value); % save its size in case its an object
[m,n] = size(object.data); % find the number of subobjects in data

% set object version number
if( strcmp('objvernum',item) )
	object.objvernum=value;
	return;

% set the object creation date
elseif( strcmp(item,'objcreate') )
	object.objcreate=value;
	return;

% set the object modified date
elseif( strcmp('objmodified',item) )
	object.objmodified=value;
	return;

% set the object type
elseif( strcmp('objtype',item) )
	if( length(value) > 4 )
		error(' object type must be 4 characters in length');
	end
	object.objtype=value;
	return;

% set the data type
elseif( strcmp('datatype',item) )
	object.datatype=value;
	return;

% set the user name
elseif( strcmp('username',item) )
	object.username=value;
	return;

% set the object's name
elseif( strcmp('name',item) )
    object.name=value;	    
    object.objmodified=fix(clock);
    return;

% set a data name
elseif( strcmp(item,'dataname') )
 	if( nargin < 4)
                error(' data field must be specified ');
    end                           
     
    if( ~isempty(dataname) )
	   for j=1:n
	      if( strcmp(strunpad(dataname),strunpad(object.data(j).dataname)) )
	         if( object.data(j).protect==1 )
	            error(' attempt to change protected data');
	         else
	            ff=object.data(j).dataname;
	            gg=getfield(object.data,{j},ff);
	            object.data=setfield(object.data,{j},value,gg);
	            object.data=rmfield(object.data,ff);
	            object.data(j).dataname=value;
	            return;
	         end
	      end
	   end
	else
		 if( object.data(datanum).protect==1 )
		    error(' attempt to change protected data');
		 else
		    ff=object.data(datanum).dataname;
		    gg=getfield(object.data,{datanum},ff);
		    object.data=setfield(object.data,{datanum},value,gg);
		    object.data=rmfield(object.data,ff);
		    object.data(datanum).dataname=value;
		    return;  
		 end
	end
	object.objmodified=fix(clock);
	return;

% set a data modified date
elseif( strcmp(item,'datamodified') )
	if( nargin < 4)
		error(' data field must be specified ');
	end
	
	if( ~isempty(dataname) )
	   for j=1:n
	      if( strcmp(strunpad(dataname),strunpad(object.data(j).dataname)) )
	         if( object.data(j).protect==1 )
	            error(' attempt to change protected data');
	         else
	            object.data(j).datamodified=value;
	            return;
	         end
	      end
	   end
	else
		 if( object.data(datanum).protect==1 )
		    error(' attempt to change protected data');
		 else
		    object.data(datanum).datamodified=value;
		    return;  
		 end
	end

% set the data protect field 
elseif( strcmp(item,'protect') )
        if( nargin < 4)
                error(' data field must be specified ');
        end
	if( ~strcmp(value,'on') & ~strcmp(value,'off') )
		error(' invalid protection specification');
	end
	if( strcmp(value,'on') ) value = 1;
	else value = 0; end
	
	if( ~isempty(dataname) )
	   for j=1:n
	      if( strcmp(strunpad(dataname),strunpad(object.data(j).dataname)) )
	            object.data(j).protect=value;
	            return;
	      end
	   end
	else
	   object.data(datanum).protect=value;
           return;  
	end

% set the dataobjtype field 
elseif( strcmp(item,'dataobjtype') )
	if( nargin < 4)
		error( 'data field must be specified');
	end
	if( ~isempty(dataname) )
	   for j=1:n
	   	  if( strcmp(strunpad(dataname),strunpad(object.data(j).dataname)) )
	        gg=getfield(object.data,{j},'dataname');
	        hh=size( getfield(object.data,{j},gg) );
		if( isearthobj(value) )
	          object.data(j).dataobjtype = objget(value,'objtype');
 	        elseif( hh(2)==1 & hh(1)==1)
 		  object.data(j).dataobjtype = 'sclr';
 	        elseif( hh(1)>1 & hh(2)==1 )
 		  object.data(j).dataobjtype = 'cvec';
 	        elseif( hh(1)==1 & hh(2)>1 )
 		  object.data(j).dataobjtype = 'rvec';
 	        elseif( hh(2)>1 & hh(1)>1 )
 		  object.data(j).dataobjtype = 'mtrx';
 		end
 		return;
 	      end
 	   end
 	else
 		gg=getfield(object.data,{datanum},'dataname');
	        hh=size( getfield(object.data,{datanum},gg) );
 	 	if( isearthobj(value) )
	          object.data(datanum).dataobjtype = objget(value,'objtype');
 	        elseif( hh(2)==1 & hh(1)==1)
 		  object.data(datanum).dataobjtype = 'sclr';
 	        elseif( hh(1)>1 & hh(2)==1 )
 		  object.data(datanum).dataobjtype = 'cvec';
 	        elseif( hh(1)==1 & hh(2)>1 )
 		  object.data(datanum).dataobjtype = 'rvec';
 	        elseif( hh(2)>1 & hh(1)>1 )
 		  object.data(datanum).dataobjtype = 'mtrx';
 		end
 		return;
        end

% set the dataDataType field 
elseif( strcmp(item,'datadatatype') ) 
   if( nargin < 4)
        error(' data field must be specified ');
   end
   if( length(value)~=4 )
   	error(' datatypes are 4 characters in length ');
   end
   if( ~isempty(dataname) )
      	for j=1:n
 			if( strcmp(strunpad(dataname),strunpad(object.data(j).dataname)) )
     			object.data(j).datadatatype=value;
   			end
   		end
   else
   	object.data(datanum).datadatatype=value;
   end
   return;
end 
	
% At this point, we must either be setting a new dataobject or updating an existing
% one. Thus we must walk down the object until either: a) we come to a dataobject
% whos name matches item or b) we come to the end of the object in which case we
% are adding a new dataobject.

[m,n]=size(object.data);

% check for existing name and update   
if( ~isempty(dataname) ) 
   for j=1:n
      if( strcmp(strunpad(item),strunpad(object.data(j).dataname)) |...
      		 isempty(object.data(j).dataname) )
      	 if( object.data(j).protect==1 )
            error(' attempt to change protected data');
         elseif( ~isempty(value) )
            object.data(j).dataname = item;
 	    object.data(j).protect = 0;
 	    if isearthobj(value)
		object.data(j).dataobjtype = objget(value,'objtype');
     	    elseif( rdat==1 & cdat==1)
        	object.data(j).dataobjtype = 'sclr';
      	    elseif( rdat>1 & cdat==1 )
       		object.data(j).dataobjtype = 'cvec';
      	    elseif( rdat==1 & cdat>1 )
      		object.data(j).dataobjtype = 'rvec';
            elseif( rdat>1 & cdat>1 )
       		object.data(j).dataobjtype = 'mtrx';
            end
            if( isempty(datatype) )
              if( isearthobj(value) )
                object.data(j).datadatatype = objget(value,'datatype');
              elseif( ischar(value) )
                object.data(j).datadatatype = 'strg';
              else
                object.data(j).datadatatype = 'smpl';
              end
            else
              object.data(j).datadatatype= datatype;
            end
            object.data(j).datamodified = fix(clock);
 	    object.data = setfield(object.data,{j},item,value);
       	    return;
       	 elseif( isempty(value) & ~isempty(object.data(j).dataname) )
       	      object.data(j) = [];
       	      object.data = rmfield(object.data,item);
       	      return;
       	 end
      end
   end
end

% add a new data structure 
faa=char(fieldnames(object)); [faam,faan]=size(faa);
fbb=char(fieldnames(object.data));[fbbm,fbba]=size(fbb);
% check to see that new data name isn't same as existing fieldname
for j=1:faam
	aflag(j)=strcmp( strunpad(item),strunpad(faa(j,:)) );
end
for j=1:fbbm
	bflag(j)=strcmp( strunpad(item),strunpad(fbb(j,:)) );
end
if( ~isempty(dataname) )
   if( ~any(aflag) | (~any(bflag) & ~isempty(value)) )% add new dataobject 
      object.data(n+1).dataname=item;
      object.data(n+1).protect = 0;
      
      if isearthobj(value)
	object.data(n+1).dataobjtype = objget(value,'objtype');
      elseif( rdat==1 & cdat==1)
        object.data(n+1).dataobjtype = 'sclr';
      elseif( rdat>1 & cdat==1 )
        object.data(n+1).dataobjtype = 'cvec';
      elseif( rdat==1 & cdat>1 )
      	object.data(n+1).dataobjtype = 'rvec';
      elseif( rdat>1 & cdat>1 )
        object.data(n+1).dataobjtype = 'mtrx';
      end
      
      if( isempty(datatype) )
        if( isearthobj(value) )
          object.data(n+1).datadatatype = objget(value,'datatype');
        elseif( ischar(value) )
          object.data(n+1).datadatatype = 'strg';
        else
          object.data(n+1).datadatatype = 'smpl';
        end
      elseif( ~isempty(datatype) & ischar(datatype) )
        object.data(n+1).datadatatype = datatype;  
      end   
      object.data(n+1).datamodified = fix(clock);
      gg=getfield(object.data,{n+1},'dataname');
      object.data=setfield(object.data,{n+1},gg,value);
      return;
   end
end