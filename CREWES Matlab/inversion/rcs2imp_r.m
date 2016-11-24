function imp=rcs2imp_r(rcs,znot,flag)
% imp=rcs2imp_r(rcs,znot,flag)
% imp=rcs2imp_r(rcs,znot)
%
% RCS2IMP_R computes impedence from RCS by recursion. It is
% more accurate but slower than RCS2IMP
% 
% rcs ... input column vector of reflection coefficients
% znot ... scalar value of first impedence
% flag ... if 1, then a shift of 1/2 a sample interval is
%	applied. Otherwise, no action. This is useful to more
%	properly invert the rc computation in imp2rcs
% ******** default = 0 ***************
%
% G.F. Margrave May 1995
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
if(nargin<3) flag=0; end
n=length(rcs);
imp=zeros(size(rcs));
imp(1)=znot;
for k=2:n
	imp(k)=imp(k-1)*(1+rcs(k-1))/(1-rcs(k-1));
end
if(flag==1)
	%make a time coordinate vector(arbitrary)
	t=xcoord(0.,.001,rcs);
	%fft
	[Imp,f]=fftrl(imp,t);
	%design phase shifter
	phs=cos(pi*.001*f)+i*sin(pi*.001*f);
	%apply
	Imp=Imp.*phs;
	%inverse fft
	[imp,t]=ifftrl(Imp,f);
end