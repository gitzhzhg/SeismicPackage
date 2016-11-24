function smat = component_merge( z, x, y )
%
% function smat = component_merge( z, x, y )
%
% This function merges three vectors or matrices into a single matrix.
% Traces are assumed to be in the columns.
%   eg. z(:,1) = vertical component trace 1
%
%   z    = matrix containing component one traces (eg. vertical)
%   x    = matrix containing component two traces(eg. inline)
%   y    = matrix containing component three traces (eg. crossline)
%   smat = matrix containing merged x,y,z data
%
% Where smat(:,1) = z(:,1)
%       smat(:,2) = x(:,1)
%       smat(:,3) = y(:,1)
%       smat(:,4) = z(:,2)
%       smat(:,5) = x(:,2)
%          :      =  :
%
% by K.W. Hall, Nov. 2015
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

%Check inputs
if ~isequal(nargin,3)
    error('CREWES:component_split','Incorrect number of input arguments')
end

%Check inputs
[z_nsamp,z_ntr] = size(z);
[x_nsamp,x_ntr] = size(x);
[y_nsamp,y_ntr] = size(y);

if isequal(z_nsamp,1) %row vector input
    z = z';
    x = x';
    y = y';
    [z_nsamp,z_ntr] = size(z);
    [x_nsamp,x_ntr] = size(x);
    [y_nsamp,y_ntr] = size(y);
end
    
if ~isequal(z_nsamp,x_nsamp) || ~isequal(z_nsamp,y_nsamp)
    error('CREWES:component_merge',...
       'Incorrect number of samples: z,x,y must be the same size')
end
    
if ~isequal(z_ntr,x_ntr) || ~isequal(z_ntr,y_ntr)
    error('CREWES:component_merge',...
       'Incorrect number of traces: z,x,y must be the same size')
end

%allocate memory
ntr = 3*z_ntr;
smat = zeros(z_nsamp,ntr);

%merge traces
smat(:,1:3:ntr) = z;
smat(:,2:3:ntr) = x;
smat(:,3:3:ntr) = y;

end %end function component_merge