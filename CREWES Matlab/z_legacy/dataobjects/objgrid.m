function E = objgrid(name,dataname,data,delx,dely,xnot,ynot,datatype)

% E = objgrid( name, dataname, data, delx, dely, xnot, ynot, datatype)
% E = objgrid( name, dataname, data,  delx, dely, xnot, ynot)
% E = objgrid( name, dataname, data, delx, dely, xnot)
% E = objgrid( name, dataname, data, delx, dely)
% E = objgrid( name, dataname, data, delx)
% E = objgrid( name, dataname, data )
%
% OBJGRID creates an EarthObject designed to store earth science
% data sampled at regular grid locations. The matrix dimensions
% of 'data' define the grid while 'delx' and 'dely' specify the
% physical distance between columns and rows respectively. 
% The returned EarthObject, E, should NEVER be used directly in 
% computations because it is designed as a general data storage 
% object. Instead, use the appropriate OBJGET and OBJSET calls to 
% get and set the items in E.
%
% name = string vector specifying the object's name 
%					               requirement: length(name) <= length(data(:))
% data = matrix containing the gridded data
% dataname = string of ten chars or less giving the name of the data
%        ************ default = 'data' *************
% delx = physical distance between columns in the matrix
%        ************ default = 1.0 ****************
% dely = physical distance between rows in the matrix
%        ************ default = 1.0 ****************
% xnot = first column coordinate
%        ************ default = 0.0 ****************
% ynot = first row coordinate
%        ************ default = 0.0 ****************
% datatype = string vector specifying the data type
%                    requirement: length(datatype) == 4
%     ******* default = '    ' *********
% E = the returned EarthObject
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

if nargin < 8
   datatype='    ';
end
if nargin < 7
   ynot = 0.0;
end
if nargin < 6
   xnot = 0.0;
end
if nargin < 5
   dely = 1.0;
end
if nargin < 4
   delx = 1.0;
end
if nargin < 3
error('objgrid must have at least 3 arguments');	
end
if( ~ischar(name) )
	error('first argument must be a string');
elseif( ~ischar(dataname) )
	error('second argument must be a string');
end


E= gridobj(name,data,dataname,delx,dely,xnot,ynot,datatype);