function ampspec=tntamp(fnot,f,m)
% TNTAMP: create an amplitude spectrum for an impulsive source 
%
% ampspec=tntamp(fnot,f)
%
% TNTAMP returns an amplitude spectrum appropriate for an impulsive
% source. The spectrum has the shape: ampspec=(1-gaus)./(1+(f/fnot).^m);
% where gauss=exp(-(f/fnot).^2).
%
% fnot ... the dominant frequency
% f ... a frequency sample vector such as that created by fftrl.
% m ... exponent in the denoninator controlling the spectral shape. Make m 
%       larger for a sharper spectral rolloff at high frequencies.
%    ******* default = 2 *******. 
%
% by G.F. Margrave, May 1991
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

 if(nargin<3); m=2; end
 gaus=exp(-(f/fnot).^2);
 ampspec=(1-gaus)./(1+(f/fnot).^m);