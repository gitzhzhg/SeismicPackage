function coef=cos_taper(sp,ep,samp)
% COS_TAPER: used by KIRK_MIG
%  coefficients of cos taper
%
%  coef=cos_taper(sp,ep,samp)
%
%  coef:  coefficient with length (start-end)/samp + 1
%  sp:    start point to begin taper
%  ep:    end point to stop taper
%  samp:  the sample interval; default is 1.
%
%  By Xinxiang Li, CREWES project, U of C.
%  FEB. 1996
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
dd=[];
if nargin < 2 error('At least two input arguments needed!'); end
if nargin < 3 samp = 1. ; end
if samp < 0 samp = -samp ; end
len = abs(ep-sp)/samp;
len = len+1;
if len <= 1   coef = [1.0]; end
if len > 1
   coef=(1:len)*0.;
   dd = 1.0/(len-1)*pi*0.5;
   for i = 1:len
       coef(i) = cos((i-1)*dd);
   end
end
clear len,dd;
    