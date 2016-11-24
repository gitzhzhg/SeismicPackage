function [trout,t]=igabor_old(tvs,fcol)
% IGABOR: inverse Gabor transform without synthesis windowing
%
% [trout,t]=igabor_old(tvs,fcol)
% 
% IGABOR performs an inverse Gabor transform of a Gabor spectrum. This is
% implemented with a synthesis window of unity. For a gaussian
% synthesis window use IGABOR_SYN. The algorithm is a simple sum
% along each column (collapsing the row dimension) of the Gabor
% spectrum followed by an ordinary IFFT.
%
% tvs= input time variant spectrum or Gabor spectrum. This is typically
%      created by FGABOR.
% fcol= frequency coordinate vector for the columns of tvs
% trout= output time series
% t= time coordinate vector for trout
%
% by G.F. Margrave, May 2001
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


[nwin,nf]=size(tvs);

%loop over windows
%for k=1:nwin
%    if(k==1)
%        [trout,t]=ifftrl(tvs(k,:),fcol);
%    else
%        trout=trout+ifftrl(tvs(k,:),fcol);
%    end
%end

% the one liner achieves the same thing as the loop because the fft and sum
% commute
[trout,t]=ifftrl(sum(tvs),fcol);

%trout=trout(:)/nwin;
trout=trout(:);