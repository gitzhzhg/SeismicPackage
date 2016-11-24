function sigout=bcarsmooth(sigin,len)
% sigout=bcarsmooth(sigin,len)
%
% BCARSMOOTH smooths the input signal with a boxcar of specified length
%
% sigin  = signal to be smoothed
% len    = length of boxcar smoothing operator, must be an integer 
% sigout = signal after smoothing
%
% H.J.E. Lloyd November 2013
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
len=round(len);
if isodd(len)
    len2=(len+1)/2;
else
    len2=len/2;
end
len3=(len2*2)+len;
bcar=zeros(1,len3);
bcar(len2:len2+len)=1/(len+1);
sig=[sigin(1)*ones(len3,1);sigin(:);sigin(end)*ones(len3,1)];
sigo=convz(sig,bcar);
sigout=sigo(len3+1:end-len3);