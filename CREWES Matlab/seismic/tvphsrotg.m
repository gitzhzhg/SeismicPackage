function trout=tvphsrotg(trin,phi,t,twin,tinc)
% TVPHSROTG Constant-phase rotate a trace with a Gabor method
% trout=tvphsrotg(trin,phi,t,twin,tinc)
%
% TVPHSROTG performs a time varient constant phase rotation of the input trace
% through an vector of angles (phi) in degrees.
%
% trin= input trace
% phi= vector of phase rotation angles in degrees created by tvconstphase
% t= time coordinate vector for trin
% twin= width (seconds) of the Gaussian window
% tinc= temporal shift (seconds) between windows
%
% trout= phase rotated trace
%
% by G.F. Margrave and H.J.E. Lloyd Jan 2012
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
tmin=t(1);
t=t-tmin;
dt=t(2)-t(1);
%determine number of windows. tinc will be adjusted to make the
% last window precisely centered on tmax
tmax=t(end);
nwin=tmax/tinc+1; %this will generally be fractional
nwin=round(nwin);
tinc=tmax/(nwin-1); %redefine tinc
tout=zeros(nwin,1);
phs=tout;
[gwin,norm_factor,tinc2,nwin]=gaussian_upou(t,tmin,twin,tinc);

if(length(phi)~=nwin)
    nwin=length(phi);
end

tout=(0:nwin-1)*tinc+tmin;
trmat=zeros(length(trin),nwin);
%loop over windows
wbar=waitbar(0,'Please Wait...');
for k=1:nwin
    %build the gaussian
    tnot=(k-1)*tinc;
    gwin=gaussian_upou(t,tnot,twin,tinc,norm_factor);
    %build the gaussian
    tnot=(k-1)*tinc;
    tout(k)=tnot;
    gwin=exp(-((t-tnot)/twin).^2)/(sqrt(pi)*twin/tinc);
    %window and measure phase
    s1w=trin.*gwin;
    trmat(:,k)=phsrot(s1w,phi(k));
    waitbar(k/nwin,wbar);
end
delete(wbar);
trout=sum(trmat,2);
% a=trin'*trout/(trout'*trout);
% trout=a*trout;