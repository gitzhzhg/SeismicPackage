function [mask,htrin]=hmask(trin,thresh,flag)

% [mask,htrin]=hmask(trin,thresh,flag)
% [mask,htrin]=hmask(trin)
%
% Compute the Hilbert Reflectivity Mask of trin.
% HMASK returns a trace which is zero everywhere except at the samples
% corresponding to peaks of abs(hilbert(trin)) or peaks or abs(trin) where
% it is 1.0
%
% trin= input trace
% mask= output hilbert mask trace
% htrin= complex hilbert transform of trin. (e.g. hilbert(trin))
% thresh = threshold significance for peaks. Peaks on the Hilbert
%	envelope are ignored if they are smaller than thresh times
%	the maximum peak.
%	********** default = .05 *********
% flag = 0 .... pick peaks of the hilbert envelope=abs(hilbert(trin))
%      = 1 .... pick peaks of abs(trin)
% *********** default =0 ***************
% 
% by G.F. Margrave, July 1991, 2014
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

% $Id: hmask.m,v 1.4 2004/07/30 22:03:45 kwhall Exp $

if(nargin<2)
		thresh=.05;
end
if(nargin<3)
    flag=0;
end

% compute hilbert transform and envelope
htrin=hilbert(trin).';
if(flag==0)
    env=abs(htrin);
elseif(flag==1)
    env=abs(trin);
end
% build hilbert mask
iex=findex(env);
 mask=zeros(size(env));
 if length(iex)>=1,
  mask(iex)=ones(size(iex));
 end
 mm=max(env);
 iz=find(env<mm*thresh);
 mask(iz)=zeros(size(iz));