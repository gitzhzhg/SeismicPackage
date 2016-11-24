function [mask,htrin]=hmask(trin,thresh)

% [mask,htrin]=hmask(trin,thresh)
% [mask,htrin]=hmask(trin)
%
% Compute the Hilbert Reflectivity Mask of trin.
% HMASK returns a trace which is zero everywhere except at the
% samples corresponding to peaks of abs(hilbert(trin)) where it
% is 1.0
%
% trin= input trace
% mask= output hilbert mask trace
% htrin= complex hilbert transform of trin. (e.g. hilbert(trin))
% thresh = threshold significance for peaks. Peaks on the Hilbert
%	envelope are ignored if they are smaller than thresh times
%	the maximum peak.
%	********** default = .05 *********
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

% $Id: hmask.m,v 1.4 2004/07/30 22:04:16 kwhall Exp $

if(nargin<2)
		thresh=.05;
end

% compute hilbert transform and envelope

% htrin=hilbert(padpow2(trin)).';
 htrin = hilbert(trin).';
 env=abs(htrin);
% build hilbert mask
 iex=findex(env);
 mask=zeros(size(env));
 if length(iex)>1,
  mask(iex)=ones(size(iex));
 else
  mask(iex)=1.0;
 end
 mm=max(env);
 iz=find(env<mm*thresh);
 mask(iz)=zeros(size(iz));