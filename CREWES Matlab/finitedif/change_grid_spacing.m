% CHANGE_GRID_SPACING ... example script to interpolate a velocity model
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

% interpolate different grid spacing in velocity matrix

% grid spacing 20 m = change to 10m (halve)

load vmatrix;

x=1:1:300;
y=1:1:150;
y=y';

x1=0.5:0.5:300;
y1=0.5:0.5:150;
y1=y1';

vnewgrid = interp2(x,y,vmatrix,x1,y1);

% grid spacing 20m = change to 5 m (quarter)

load vmatrix;

x=1:1:300;
y=1:1:150;
y=y';

x2=0.25:0.25:300;
y2=0.25:0.25:150;
y2=y2';

vnewgrid2=interp2(x,y,vmatrix,x2,y2);

for j=1:3
    vnewgrid2(j,:)=vnewgrid2(4,:);
    vnewgrid2(:,j)=vnewgrid2(:,4);
end