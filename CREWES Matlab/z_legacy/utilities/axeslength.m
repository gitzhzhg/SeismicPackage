function [xlen,ylen]=axeslength(hax,unit)
% [xlen,ylen]=axeslength(hax,unit)
% [xlen,ylen]=axeslength(hax)
%
% AXESLENGTH returns x-axis and y-axis lengths for a
% constant aspect ratio. Lengths are returned in user
% or default specified units.
%
% hax	= axes handle, for example gca
% unit	= 'inches', 'centimeters', 'pixels' etc.
%=============== Default = 'centimeters' ===============
%
% xlen	= length of x-axis in units
% ylen	= length of y-axis in units
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

%
if nargin<2
	unit='centimeters';
end
	
	au=get(hax,'units');
	a=get(hax,'DataAspectRatio');
	a=a(1);
	set(hax,'units',unit);
	pos=get(hax,'position');
	
	if(isnan(a))
		xlen=pos(3);
		ylen=pos(4);
	elseif( a< pos(3)/pos(4) )
		xlen=pos(4)*a;
		ylen=pos(4);
	else
		ylen=pos(3)/a;
		xlen=pos(3);
	end
	
	set(hax,'units',au);