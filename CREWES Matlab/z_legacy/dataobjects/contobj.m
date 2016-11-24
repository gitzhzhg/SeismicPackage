function E = contobj(objname,subobj,subobjname,datatype,subobjdatatype)

% E= contobj( name, subobj,subobjname,datatype,subobjdatatype)
% E= contobj( name, subobj,subobjname,subobjdatatype)
% E= contobj( name, subobj,subobjname)
% E= contobj( name, datatype)
%
%
% CONTOBJ creates a container EarthObject designed to store other 
% EarthObjects. The last syntactical form creates an empty container object.
% The returned matrix, E, should NEVER be used directly in 
% computations because it is designed as a general data storage 
% object. Instead, use the appropriate OBJGET and OBJSET calls to 
% get and set the items in E.
%
% name = string vector specifying the object's name 
%	    requirement: length(name) <= length(data)
% subobj = another object to be contained in this one
% subobjname = the name of the subobj
%     ************ default = objget(subobj,'name') ************
%
% datatype = string vector specifying the data type of the container object
%     ******* default = '    ' *********
%
% subobjdatatype = string vector specifying the data type of the subobject.
%     ********* length(subobjdatatype) = 4 ******************** 
%     ********* default = objget(subobject,'datatype') unless the
%	  subobject is a simple scalar, matrix or vector. In that case the
%         default depends on size(subobject)
%		size(subobject)		default
%		    [1,1]		 'sclr'
%		    [n,1]		 'cvec'
%		    [1,n]		 'rvec'
%		    [m,n]		 'mtrx'
%
% E =  the returned EarthObject
%
% by G.F. Margrave, March 1993
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

if nargin < 2
     error('You must supply at least three arguments');
end

% declare E
 E= buildcontobj;
 
% set defaults
if nargin < 4 
     datatype = '    ';
end
if nargin < 3
   datatype = subobj;
   subobj = [];
   subobjname = [];
end 
[m,n]=size(subobj);
if nargin < 5
	if( isearthobj(subobj) )
	          E.data.dataobjtype = objget(subobj,'objtype');
 	elseif( m==1 & n==1)
 		  E.data.dataobjtype = 'sclr';
 	elseif( n>1 & m==1 )
 		  E.data.dataobjtype = 'rvec';
 	elseif( n==1 & m>1 )
 		  E.data.dataobjtype = 'cvec';
 	elseif( n>1 & m>1 )
 		  E.data.dataobjtype = 'mtrx';
 	end
 end
 
% set object version number
 E.objvernum=1.0;
%set object creation date
 E.objcreate=fix(clock);
% fix object modification date
E.objmodified=fix(clock);
% set user field
 E.username='matlab';
% set flag for contobj
 E.objtype='cont';
% set data type
 if( length(datatype)~=4 )
 	error(' data types must be 4 characters in length');
 else
 	E.datatype=datatype;
 end
% set object's name
 E.name=objname;

% set data modified values
 E.data.datamodified=fix(clock);
  
% set data name
 E.data.dataname=subobjname;
 
% set protection value (default=0)
 E.data.protect=0;
 
% set datadatatype
if( ischar(subobj) )
 datatype='strg';
else
 datatype='smpl';
end
 E.data.datadatatype=datatype;
   
% set the data
if( ~isempty(subobj) )
 	E.data=setfield(E.data,subobjname,subobj);
end