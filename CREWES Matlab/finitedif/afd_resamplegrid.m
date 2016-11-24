function [section,x,z,dxz] = afd_resamplegrid( oldsection,oldx,oldz,newdxz)
%AFD_RESAMPLEGRID is a program to convert sections that do not have equal
%gridspacing in both the x and z directions, ie. dx is not equal to dz.
%this program also will start both x and z at the zero as this is required
%for the finite differencing toolbox.  If you would like to have these
%vectors start at a different location simply add the starting point to the
%vector ie. z=z+400;
%
%[section,x,z,dxz] = afd_resamplegrid( oldsection,oldx,oldz,newdxz)
%
%   The variables that are required for this funtion are:
%        oldsection... this is the section that you would like to have
%                      resampled.
%        oldx... This is the inline vector of the section you would like to
%                have resampled.  dx will be calculated from this vector.
%        oldz... This is the depth vector of the section you would like to
%                have resampled.  dz will be calculated from this vector.
%        newdxz... This is the sampling rate that you would like in both 
%                  the depth and the inline directions.  ie. dz=dx=newdxz
%                  ***********default=10***********
%
%  The variables that are returned by this funtion are:
%        section... this is the section that is resampled.
%        x... This is the inline vector of the resampled section.  It will 
%                start at zero and continue to the end of the section.
%        z... This is the depth vector of the resampled section.  It will 
%                start at zero and continue to the end of the section.
%        dxz... This is the sampling rate that is used in both the depth 
%                and the inline directions.  ie. dz=dx=newdxz 
%
% By Heather J.E. Lloyd, August 2009
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

if nargin<3
    disp('At least oldsection, oldx and oldz must be supplied to use this function');
    return
end
if nargin==3
    dxz=10;
else
    dxz=newdxz;
end
szoldx=size(oldx);
if szoldx(1)>szoldx(2);
    nx=(oldx(1):dxz:oldx(end))';
    x=(0:dxz:(oldx(end)-oldx(1)))';
else
    x=0:dxz:(oldx(end)-oldx(1));
    nx=(oldx(1):dxz:oldx(end));
end

szoldz=size(oldz);
if szoldz(1)>szoldz(2);
    nz=(oldz(1):dxz:oldz(end))';
    z=(0:dxz:(oldz(end)-oldz(1)))';
else
    z=0:dxz:(oldz(end)-oldz(1));
    nz=(oldz(1):dxz:oldz(end));
end
section=interp2(oldx,oldz,oldsection,nx,nz);

end