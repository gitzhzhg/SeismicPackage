function E = objrand(objname,dataname,data,x,y,datatype)

% E= objrand( name, dataname, data, x, y, datatype)
% E= objrand( name, dataname, data, x, y)
% E= objrand( name, dataname, data, x)
% E= objrand( name, dataname, data)
% E= objrand( name, datatype)
%
%
% OBJRAND creates an EarthObject designed to store 
% earth science data sampled at arbitrary (x,y) locations. 
% The returned matrix, E, should NEVER be used directly in 
% computations because it is designed as a general data storage 
% object. Instead, use the appropriate OBJGET and OBJSET calls to 
% get and set the items in E.
%
% name = string vector specifying the object's name 
%	    requirement: length(name) <= length(data)
% data = vector containing the data
% x = vector containing the x coordinates of the data
%				 ******* default= no field set *********
% y = vector containing the y coordinates of the data
%				 ******* default= no field set *********
% datatype = string vector specifying the data type
%                    requirement: length(datatype) == 4 
%     ******* default = '    ' *********
%
% E =  the returned EarthObject
%
% by G.F. Margrave, March 1993
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
     error('You must supply at least two arguments');
 end
% set defaults
 if nargin < 6 
     datatype = '    ';
 end
 if nargin<5
     y = [];
 end
 if nargin<4
     x = [];
 end  
if nargin < 3
 datatype=dataname;
 data=[];
 dataname=[];
end

% make sure data is a vector
if( min(size(data)) > 1 )
	error(' Data must be a vector not a matrix');
end
if( ~ischar(dataname) & ~ischar(datatype) )
	error(' second argument must be a string ');
end

E=randobj(objname,data,dataname,x,y,datatype);