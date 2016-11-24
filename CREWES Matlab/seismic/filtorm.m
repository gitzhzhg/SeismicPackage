function so=filtorm(s,t,f1,f2,f3,f4,flag,tlen,stab)
% FILTORM ... apply and Ormsby bandpass filter to a seismic gather
%
% seiso=filtorm(s,t,f1,f2,f3,f4,flag,tlen,stab)
%
% FILTORM calculates a zero or minimum phase Ormsby wavelet and uses this
% to filter the input seismic data by convolution. The Ormsby wavelet is
% defined by a theretical formula that requires four input frequecies that
% are constrained by 0<f1<f2<f3<f4<fnyq where fnyq=.5/(t(2)-t(1)) is the
% Nyquist frequency. The passband of the filter is f2-f3 while f1 and f2
% defined the width of frequency band tapers. Thus this is fundamentally a
% bandpass filter and high-pass or low-pass filters can only be
% approcximated. The output gather is truncated after convolution to be the
% same size as the input. No taper is applied.
% 
% s  ... input trace or gather. One trace per column
% t  ... time coordinate for s
% f1 ... low frequency stop
% f2 ... lowest frequency to pass unattenuated
% f3 ... highest frequency to pass attenuated`
% f4 ... high frequency stop
% flag ... 0 for zero phase, 1 for minimum phase
% ********* default flag= 0**********
% tlen ... length of the Ormsgy wavelet in seconds
% ********* default tlen=max([.2 t(end)/8]); *********
% stab ... stability constant used for minimum phase. See tomin.m
% ********* default stab = 0.0001 **********
%
% so ... output filtered trace or gather, the same size as the input
% 
% by G.F. Margrave, 2015
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

if(nargin<9)
    stab=.0001;
end
if(nargin<7)
    flag=0;
end
if(nargin<8)
   tlen=max([.2 t(end)/8]);
end

dt=t(2)-t(1);

w=ormsby(f1,f2,f3,f4,tlen,dt);
if(flag==1)
    w=tomin(w,stab);
    so=convm(s,w);
else
    so=convz(s,w);
end