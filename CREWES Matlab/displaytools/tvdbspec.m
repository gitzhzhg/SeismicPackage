function hh=tvdbspec(t,seis,tnots,twins,tpad,dname,haxe)
% TVDBSPEC: displays average spectra in 3 time windows plus the total spectrum
%
% hh=tvdbspec(t,seis,tnots,twins,tpad,dname,haxe)
%
% The spectral analysis of a nonstationary seismic dataset (all real data
% are nonstationary) is facilitated by this function. Amplitude
% spectra are computed in three temporal windows and for the entire trace for
% on each trace. An mwindow is used to select the data in each time window.
% Then these spectra are averaged ove the seismic matrix for each
% window. The result is four spectra which show the average for each window
% and the total trace. These are then plotted on a decibel scale where the
% decibels are with respect to the maximum on the total spectrum.
% 
% t ... time coordinate for seis
% seis ... seismic matrix, one trace per column
% Note: length(t) must equal size(seis,1)
% tnots ... vector of 3 times defining start times for 3 spectral windows
% twins ... vector of 3 window lengths paired with tnots. May be a scalar
%           if all windows are the same
% tpad ... length to which windows will be padded with zeros (smoother spectra)
% dname ... string giving a name for this data.
% ********* default is '' *********
% haxe ... handle of axes to plot in
%   ******* default is gca *******
%
% G.F. Margrave, Devon Canada, July 2016
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

if(nargin<7)
    haxe=gca;
end
if(nargin<6)
    dname='';
end

if(length(twins)==1)
    twins=twins*ones(1,3);
end
dt=t(2)-t(1);
ntpad=round(tpad/dt);
ntpad=2^nextpow2(ntpad);

iw1=near(t,tnots(1),tnots(1)+twins(1));
mw1=mwindow(length(iw1));
%window 2
iw2=near(t,tnots(2),tnots(2)+twins(2));
mw2=mwindow(length(iw2));
%window 3
iw3=near(t,tnots(3),tnots(3)+twins(3));
mw3=mwindow(length(iw3));
%compute spectra
[nt,ntraces]=size(seis);
for k=1:ntraces
    [S,f]=fftrl(seis(:,k),t,10,2^nextpow2(nt));
    [S1,f1]=fftrl(seis(iw1,k).*mw1,t(iw1),10,ntpad);
    [S2,f2]=fftrl(seis(iw2,k).*mw2,t(iw2),10,ntpad);
    [S3,f3]=fftrl(seis(iw3,k).*mw3,t(iw3),10,ntpad);
    if(k==1)
        A=abs(S);
        A1=abs(S1);
        A2=abs(S2);
        A3=abs(S3);
    else
        A=A+abs(S);
        A1=A1+abs(S1);
        A2=A2+abs(S2);
        A3=A3+abs(S3);
    end
end
A=A/ntraces;
A1=A1/ntraces;
A2=A2/ntraces;
A3=A3/ntraces;
Amax=max(A);



axes(haxe)

hh=plot(f,todb(A,Amax),f1,todb(A1,Amax),f2,todb(A2,Amax),f3,todb(A3,Amax));
xlabel('frequency (Hz)');ylabel('decibels')
legend('Total spectrum',[num2str(tnots(1)) ' to ' num2str(tnots(1)+twins(1)) ' sec'],...
    [num2str(tnots(2)) ' to ' num2str(tnots(2)+twins(2)) ' sec'],...
    [num2str(tnots(3)) ' to ' num2str(tnots(3)+twins(3)) ' sec']);
if(isempty(dname))
    title('Time-variant spectra')
else
    title([dname ' time-variant spectra'])
end
grid