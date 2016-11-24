function [w,tw] = rickerw(fpeak,dt)
% rickerw(fpeak,dt)
%
% rickerw: ricker wavelet
%
% 	fpeak:    the peak frequency
%
%	dt:       time sampling
%
%	w:        ricker wavelet
%
%	tw:       time sequence
%
%   Xiang Du, May   2007, $1.0
%             Sept. 2008, $1.1
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

%             Nov.  2008, $1.2

 nw=2.2/fpeak/dt;
 nw=2*floor(nw/2)+1;
 nc=floor(nw/2);
 w = zeros(nw,1);

 k=[1:1:nw]';

 alpha = (nc-k+1).*fpeak*dt*pi;
 beta=alpha.^2;
 w = (1.-beta.*2).*exp(-beta);

if nargout>1;
  tw = -(nc+1-[1:1:nw])*dt;
end