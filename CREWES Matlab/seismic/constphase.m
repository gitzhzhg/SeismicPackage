function phi=constphase(s1,s2)
% CONSTPHASE: find the best constant phase rotation to match one signal to another
%
% phi=constphase(s1,s2)
%
% Given two signals s1 and s2, this function finds the constant phase
% rotation which, when applied to s1, minimizes the L2 norm of the
% difference: s2-phase_rotated(s1) . ("Constant phase" means the phase
% is the same for all frequencies.) 
%
% s1 ... input time series to be rotated
% s2 ... input time series to match to rotated s1
% phi ... best constant phase angle in degrees. To rotate s1 to look like
%           s2, use s1r=phsrot(s1,phi)
%
% 
% by G.F. Margrave, Nov. 2005
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

%compute hilbert transform of s1
s1p=imag(hilbert(s1));

%compute statistics
sig1=sum(s1.^2);
sig11p=sum(s1.*s1p);
sig1p=sum(s1p.^2);
sig21=sum(s2.*s1);
sig21p=sum(s2.*s1p);
sig2=sum(s2.^2);

%polynomial coefficients
c4=-4*sig11p^2-(sig1-sig1p)^2;
c3=2*sig21*(sig1-sig1p)+4*sig11p*sig21p;
c2=(sig1-sig1p)^2-sig21^2+4*sig11p^2-sig21p^2;
c1=-2*sig21*(sig1-sig1p)-2*sig11p*sig21p;
c0=sig21^2-sig11p^2;

%solve for roots
r=roots([c4 c3 c2 c1 c0]);

%ok, look for real roots less than 1
ind=find(abs(imag(r))>100*eps);
r(ind)=[];%kill imaginary roots

%test both roots and their possible conjugates
a=r(1);
b=real(sqrt(1-a^2));
tests=zeros(1,2);
%tests(1)=a*sig1+b*sig11p-a^2*sig11p/b-a*sig1p-sig21+a*sig21p/b;
tests(1)=a*b*sig1+b^2*sig11p-a^2*sig11p-a*b*sig1p-b*sig21+a*sig21p;
b2=-b;
tests(2)=a*b2*sig1+b2^2*sig11p-a^2*sig11p-a*b2*sig1p-b2*sig21+a*sig21p;

ind=find(abs(tests)==min(abs(tests)));
if(ind==1) 
    phi1=-atan2(b,a)*180/pi;
else
    phi1=-atan2(b2,a)*180/pi;
end
phi2=phi1+180;
%need to check both phi1 and phi2 because one corresponds to a minimum and
%the other a maximum of the error norm.
s1r1=phsrot(s1,phi1);
s1r2=phsrot(s1,phi2);
tests2(1)=norm(s2-s1r1);
tests2(2)=norm(s2-s1r2);
ind=find(tests2==min(abs(tests2)));
if(ind==1) 
    phi=phi1;
else
    phi=phi2;
end
if(phi>180)phi=phi-360;end