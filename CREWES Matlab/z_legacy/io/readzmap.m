function obj = readzmap( filename, arg2, arg3 )
% new_object = readzmap('filename')
% new_object = readzmap('filename','grid name');
% new_object = readzmap('filename','grid name','new object name');
% new_object = readzmap('filename', existing_object)
% new_object = readzmap('filename',existing_object, 'grid name');
%
% READZMAP reads a grid from a zmap (zycor) ascii file. The file should have
% a standard zmap header and contain the z values of a single grid. The
% result is a Matlab grid object as created by GRIDOBJ. The object is
% suitable for input into mapview and gridtool. The command can be executed
% in the five different ways shown above. In the first case, only the file
% name is needed and the object and grid get default names. In the second
% case, the grid is given a name (which must be 30 characters or less.  The
% third case names both the grid and the object. The fourth possibility
% allows the grid to be added to an existing object and will abort if the new
% grid has a different size (rows & columns) from those already in the
% object. In the last case, the new grid is added to an existing object using
% the provided name.
%
% by G.F. Margrave, October 1993
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
% parse the input arguments
gridName = [];
newName = [];
exobj = [];
if( nargin == 2 )
	if( isstr(arg2) )
		gridName = arg2;
	else
		exobj = arg2;
	end
end
if( nargin == 3 )
	if( isstr(arg2) )
		gridName = arg2;
		newName = arg3;
	else
		exobj = arg2;
		gridName = arg3;
	end
end
if( length(gridName)>10 )
	error(' grid name must not exced 10 characters in length ');
end
% read past the first five lines
[fid,message] = fopen(filename);
if( fid < 0 )
	disp(' Unable to open file because:');
	disp( message );
	error(' file I/O error in readzmap');
end
fgetl(fid);
fgetl(fid);
fgetl(fid);
fgetl(fid);
fgetl(fid);
% read the next line and get the name strong out of it
s=fgetl(fid);
name = sscanf(s,'%s',1);
name=name(2:length(name)); % get rid of leading @ symbol
if(isempty(newName)) newName = name; end
% read the next line and get the znil value
s=fgetl(fid);
znil = sscanf(s,'%*f %*c %f');
% read the next line for the grid geometry 
s=fgetl(fid);
g=sscanf(s,'%f %*c %f %*c %f %*c %f %*c %f %*c %f');
nrows=g(1);
ncols=g(2);
xmin=g(3);
xmax=g(4);
ymin=g(5);
ymax=g(6);
delx = (xmax-xmin)/(ncols-1);
dely = (ymax-ymin)/(nrows-1);
% read past the next 2 lines
s=fgetl(fid);
s=fgetl(fid);
% now read in the matrix of z values
z=fscanf(fid,'%g',[nrows,ncols]);
% search for znils
ind = find(z==znil);
z(ind) = NaN*ones(size(ind));
% build an object
if(isempty(exobj))
	if(isempty(gridName)) gridName = 'Zmap 1'; end
	obj = gridobj(newName,z,gridName,delx,dely,xmin,ymin);
	
else
	[m,n]=size(exobj);
	if(isempty(gridName))
		gridName = sprintf('Zmap %d',n-1);
	end
	
	obj = objset( exobj, gridName, z);
	
end