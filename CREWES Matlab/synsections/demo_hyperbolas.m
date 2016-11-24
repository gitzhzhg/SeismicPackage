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

msg='Enter hyperbola spacing parameter as an integer between 1 and 50. <cr> to end: ';
ndelx=round(input(msg));
while(~isempty(ndelx))
	v=2000;dx=5;dt=.004;%basic model parameters
	x=0:dx:3000;%x axis
	t=0:dt:1.5;%t axis
	xcntr=max(x)/2;
	seis5=zeros(length(t),length(x));%allocate seismic matrix
	seis5=event_diph2(seis5,t,x,v,0,500,1000,ndelx,0,.1);
    disp('Working ...')
	seis5=event_diph2(seis5,t,x,v,500,xcntr-500,1000,ndelx,-45,.1);
    disp('Working ...')
	seis5=event_diph2(seis5,t,x,v,xcntr-500,xcntr+500,500,ndelx,0,.1);
    disp('Working ...')
	seis5=event_diph2(seis5,t,x,v,xcntr+500,max(x)-500,500,ndelx,45,.1);
    disp('Still at it ...')
	seis5=event_diph2(seis5,t,x,v,max(x)-500,max(x),1000,ndelx,0,.1);
    disp('Wrapping up ...')
	[w,tw]=ricker(dt,40,.2);%make ricker wavelet
	seis5=sectconv(seis5,t,w,tw);%apply wavelet
    plotimage(seis5,t,x);
    title(['Hyperbola spacing parameter ' int2str(ndelx)])
	ndelx=round(input(msg));
end