function [xvect,yvect,zvect] = gen3Arr(nx,ny,nz,xzero,yzero,zzero)
%function [xvect,yvect,zvect] = gen3Arr(nx,ny,nz,xzero,yzero,zzero)
%Generate x, y, and z vectors that will form an (nx,ny,nz) array, starting at
%   xzero,yzero,zzero and incrementing by 1
%The input parameters are
%nx      .... No. of points in the X-direction
%ny      .... No. of points in the Y-direction
%nz      .... No. of points in the Z-direction
%xzero   .... Starting point of the X-aray
%yzero   .... Starting point of the Y-aray
%zzero   .... Starting point of the Z-aray
%The output parameters are
%xvect   .... For any no. n in range
%yvect   .... xvect(n), yvect(n), zvect(n)
%zvect   .... is a unique point
%
% P.M. Manning, Dec 2011
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
    %xzero, zzero       Increments are all 1
xvect = zeros(1,nx*ny*nz);
yvect = xvect;
zvect = xvect;
ind = 1;
z = zzero;
for iz = 1:nz
    y = yzero;
    for iy = 1:ny
        x = xzero;
        for ix = 1:nx
            xvect(ind) = x;
            yvect(ind) = y;
            zvect(ind) = z;
            ind = ind+1;
            x = x+1;
        end
        y = y+1;
    end
    z = z+1;
end