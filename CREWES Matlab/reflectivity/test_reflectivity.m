% Test reflectivity modeling with a 2 layer model:
% AKA: Pissing into the wind:
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
clear;
% Two layer model:

    vp=[2.0;2.5];                   % km/s
    vs=[1.2;1.4];
    Qp=[10000;10000];
    Qs=Qp;
    thickness=[1;1];
    rho=[1.5;1.8];                  % g/cm^3
    model=[thickness rho vp Qp vs Qs];
%
%xrec=0:0.25:10; % x cordiantes for receivers  in km:
%
    xrec=0:0.1:10;                  % x cordiantes for receivers  in km
    Tmax=8;                         % Desired record length in seconds:
    dt=0.004;                       % Time sample rate, seconds:
    fmin=0;     fmax=80;            % Frequency band for ref. modeling
    smin=0;                         % Minimum slowness:
    ds=0.01/fmax*max(xrec);         % Slowness increment:
    smax=round(1.2/vs(1)/ds)*ds;    % Maximum slowness:
    tau=50; % for complex freq to supress wrap around
    per_s=100*(1-1/1.2);per_f=40; % parameter for Hanning window:
    direct=0;
    mult=0;
    z_source=0.005; % km
%
    param=[Tmax dt smin smax ds fmin fmax tau z_source per_s per_f ...
           direct mult];  
%  
% A minimum phase source wavelet with a dominant frequency of 40 Hz:
%
 [wlt,tw] =wavemin(dt,40,2*Tmax-dt); 
 [spw fw]=fftrl(wlt,tw);
 index1=fmin/(fw(2)-fw(1))+1;
 index2=fmax/(fw(2)-fw(1))+1;
 spw_z=spw(index1:index2);   
 spw_x=zeros(size(spw_z));
 spw_y=zeros(size(spw_z)); % create a source with Z components only
%
% Parameters for bandpass filter: [fmin(1) fmin(2) fmax(1) fmax(2)]:
%
    bpfilt= [5 2 60 10]; 
 
% Calculate wave fields with the reflectivity codes
% the final result is truncated to Tmax/2 in time length
%
    [uR,uZ,t] = reflectivity(model,param,xrec,[spw_x spw_y spw_z],bpfilt);
%
% Raytrace some expected events:
%
    z=[0;thickness(1)];
% PP from first reflector:
%
    [tpp1,p,L]=traceray_pp(vp,z,z_source,0,z(2),xrec,.001,-1,8);
    [tps1,p,L]=traceray_ps(vp,z,vs,z,z_source,0,z(2),xrec,.001,-1,8);
    [tss1,p,L]=traceray_pp(vs,z,z_source,0,z(2),xrec,.001,-1,8);
%
% Plot the results:
%
    figure;
    plotseis(uR,t,xrec,1,[4 max(abs(uZ(:)))],2,1,[0 0 1]);
    ylabel('Time (seconds)');
    xlabel('Offset (km)');
    title('Radial Component');
    set(gca,'ygrid','on');
    bigfig;bigfont;whitefig
    figure;
    plotseis(uR,t,xrec,1,[4 max(abs(uZ(:)))],2,1,[0 0 1]);
    ylabel('Time (seconds)');
    xlabel('Offset(km)');
    title('Radial Component with Raytraced Traveltimes');
    set(gca,'ygrid','on');
    hold
    h1=plot(xrec,tpp1,'m',xrec,tps1,'r',xrec,tss1,'g');
    bigfig;bigfont;whitefig
    legend(h1,'pp','ps','ss')
    figure;
    plotseis(uZ,t,xrec,1,[4 max(abs(uZ(:)))],2,1,[0 0 1]);
    ylabel('Time (seconds)');
    xlabel('Offset (km)');
    title('Vertical Component');
    set(gca,'ygrid','on');
    bigfig;bigfont;whitefig
    figure;
    plotseis(uZ,t,xrec,1,[4 max(abs(uZ(:)))],2,1,[0 0 1]);
    ylabel('Time (seconds)');
    xlabel('Offset (km)');
    title('Vertical Component with Raytraced Traveltimes');
    set(gca,'ygrid','on');
    hold
    h1=plot(xrec,tpp1,'m',xrec,tps1,'r',xrec,tss1,'g');
    bigfig;bigfont;whitefig
    legend(h1,'pp','ps','ss')