function trout=maxima(trin,mask)
% trout= maxima(trin,mask)
% 
% MAXIMA takes an input trace and a Hilbert mask (see HMASK)
% and uses spline interpolation to refine the estimates of 
% the local maxima.
%
% trin= input trace
% mask= Hilbert mask as computed by HMASK. mask is zero every
%       where except at samples corresponding to local maxima
%       of the Hilbert envelope of trin, where it takes the value
%       of 1.0
% trout= vector of spline interpolated maximum amplitudes, one for
%       each unit spike in mask
%
% by G.F. Margrave, July 1991
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
iex=find(mask==1);
 for i=1:length(iex)
   xs=iex(i)-2:iex(i)+2;
   xi=iex(i)-2:.2:iex(i)+2;
	  ind=find(xs<=length(trin));
	  xs=xs(ind);
   ai=spline(xs,trin(xs),xi);
   trout(i)=max(ai);
 end