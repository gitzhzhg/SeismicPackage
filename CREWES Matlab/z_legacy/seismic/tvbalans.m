function trcout=tvbalans(trcin,t,trcref,tref,twin,tinc)
% TVBALANS balances the traces to match the amplitude of the well using
% gaussian time windows
%
% trcout=tvbalans(trcin,t,trcref,tend,twin,tinc)
%
% trcin = seismic data matrix (one trace per column)
% t     = time vector for trcin
% trcref   = reference trace to match amplitudes to
% tref = time vector from trcref
% twin= width (seconds) of any Gaussian window
% tinc= temporal shift (seconds) between windows
%
% trcout= phase rotated trace
%
% by G.F. Margrave and H.J.E. Lloyd July 2012
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

%test for row vector input
if(size(trcin,1)==1)
    trcin=trcin(:);
end

%adjust traces
dt=t(2)-t(1);
if(abs(dt-(tref(2)-tref(1)))>.000000001)
    error('Reference trace and input trace must have same sample rate')
end
if(tref(end)>t(end))
    %truncate end of the reference trace
    nend=round((t(end)-tref(1))/dt)+1;
    trcref(nend:end)=[];
end

iuse=near(t,tref(1),tref(end));%points to trcin samples that line up with trcref
   
tmin=t(iuse(1));
tmax=t(iuse(end));

%determine number of windows. tinc will be adjusted to make the
% last window precisely centered on tmax
nwin=round((tmax-tmin)/tinc)+1; %number of windows
tinc=tmax/(nwin-1); %redefine tinc

%wbar=waitbar(0,'Please Wait...');

%make the amplitude model
amp_model=zeros(nwin,1);
t_model=zeros(nwin,1);
trcout=zeros(size(trcin));
for kk=1:size(trcin,2)%loop over traces
    for k=1:nwin%loop over windows
        %build the gaussian
        tnot=(k-1)*tinc+tmin;
        t_model(k)=tnot;
        gwin=exp(-((t(iuse)-tnot)/twin).^2);
        amp_model(k)=norm(gwin.*trcref,2)/norm(gwin.*trcin(iuse,kk),2);
        %waitbar(k/nwin,wbar);
    end
    %expand the amplitude model
    multiplier=interpextrap(t_model,amp_model,t,0);
    trcout(:,kk)=trcin(:,kk).*multiplier(:);
end
%delete(wbar);


