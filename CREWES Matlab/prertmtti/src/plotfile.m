function plotfile(imagefile, nx, nz, dx, dz)

% plotfile: plot the migration file (binary format)
%
%   imagefile: migration file (binary format)
%
%	nx: the size of x direction
%
%   nz: the size of z direction
%
%   dx: the space interval of x direction
%
%   dz: the space interval of z direction
%
%   Xiang Du, Nov. 2008
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

fid=fopen(imagefile,'r');
data=fread(fid,[nz nx],'float');
fclose(fid);

dmax = max(max(data));
dmin = min(min(data));
clim = [dmin dmax];

clim=clim*0.5;

x=[0:dx:(nx-1)*dx];
z=[0:dz:(nz-1)*dz];

imagesc(x,z, data, clim);
colormap(gray);