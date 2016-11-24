function [spectrum,f]=multitaper(s,t)
% multitaper: estimate the spectrum of a short seismic trace using multitaper method
%
% [spectrum,f]=multitaper(s,t)
%
% s ... seismic trace
% t ... time coordinate for s
% spectrum ... amplitude spectral estimate
% f ... frequency coordinate for spectrum
%
%
% by Peng Cheng, 2012-2013
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

if(length(s)~=length(t))
    error('s and t must be the same length')
end

nt=length(t);

[sp,f]=fftrl(s,t);
s_est=sp;
k=5;

[e,v]=dpss(nt,4);

spec=zeros(length(sp),2);

for m=1:5
    [spec(:,m),f]=fftrl((s.*e(:,m)),t);
end

spec_est=sum(abs(spec).^2,2)/5;
weight=zeros(size(spec_est));
var=sum(s.^2);
L=20;
for m=1:L
    for n=1:5
        weight(:,n)=(sqrt(v(n))*spec_est)./(v(n)*spec_est+var*(1-v(n)));
    end
    spec_est=sum(abs(spec.*weight).^2,2)./sum(weight,2);
end
tf=isnan(spec_est);
for n=1:length(tf)
    if(tf(n))
        spec_est(n)=0;
    end   
end
spectrum=sqrt(spec_est);

spectrum=spectrum*norm(sp)/norm(spectrum);