function h=drawvint(z,v,kol,ls)
% DRAWVINT: draw interval velocity as a piecewise constant function
%
% h=drawvint(z,v,kol,ls)
%
% draw an interval velocity function
%
% v= interval velocity vector
% z= depth vector
% kol = color
% default 'b'
% ls = linestyle to use
% default '-'
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


hax=gca;

if(nargin<4)
	ls='-';
end

if(nargin<3)
	kol='b';
end

dz=mean(diff(z));

z=z(:);
v=v(:);
z2=zeros(2*length(z),1);
z2(1:2:length(z2))=z;
z2(2:2:length(z2)-1)=z(2:length(z));
z2(length(z2))=z(length(z))+dz;

v2=zeros(2*length(z),1);
v2(1:2:length(v2))=v;
v2(2:2:length(v2))=v;

h=line(v2,z2,'color',kol,'linestyle',ls);