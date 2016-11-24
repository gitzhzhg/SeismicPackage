function trace=bltifft(wspec,w,rr)
%trace=bltifft(wspec,w,rr);
%
%bltifft returns time-domain data given a positive sided,
%band-limited spectrum.
%
%trace...t domain output.
%wspec...input w domain spectrum (positive frequencies only).
%w...frequency axis.
%rr...number of output times.
%
%R. J. Ferguson, 2009
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

%***check input***
w=w(:);
dw=w(2)-w(1);
if (mean(w)~=mean(abs(w)));error('positive frequencies only');end
[rws cws]=size(wspec);
%*****************

%***transform from x-w to x-t***
wstart=round(w(1)/dw)+1;%index of first live temporal frequency
wend=round(w(rws)/dw)+1;%index of last live temporal frequency
temp=zeros(rr,cws);
temp(wstart:wend,:)=wspec;
temp(round(rr/2)+2:rr,:)=flipud(conj(temp(2:round(rr/2),:)));
trace=real(ifft(temp));
%*******************************