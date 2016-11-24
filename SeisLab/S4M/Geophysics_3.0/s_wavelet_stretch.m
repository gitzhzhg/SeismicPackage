function wavelets=s_wavelet_stretch(wavelet,angles,varargin)
% Stretch a wavelet to simulate the effect of NMO 
% Stretching is achieved by changing times from t ==> t/cos(angle)
%
% Written by: E. Rietsch: February 20, 2002
% Last updated: February 24, 2004: selection of angle of incidence of reference wavelet
%
%           wavelets=s_wavelet_stretch(wavelet,angles)
% INPUT
% wavelet   original, unstretched wavelet
% angles    angles of incidence (in degrees); the wavelet is assumed to be
%           associated with angles(1).
% OUTPUT
% wavelets  stretched wavelets; there are as many wavelets as there are angles
%           of incidence
% varargin  one or more cell arrays; the first element of each cell array is a keyword,
%           the other elements are parameters. Presently, keywords are:
%     'angle'  angle associated with the input wavelet.
%           Default: {'angle',0}
%
% EXAMPLE
%       wavelet=s_create_wavelet;
%       wavelets=s_wavelet_stretch(wavelet,0:10:60);
%       wavelets.name='Stretched wavelets';
%
%       lfigure
%       subplot(1,2,1)
%          s_wplot(wavelets,{'figure','old'},{'annotation','angle'})
%       subplot(1,2,2)
%          s_spectrum(wavelets,{'average','no'},{'figure','old'})
%          mytitle('Spectra of the stretched wavelets')




%       Set default values of imput parameters
param.angle=0;

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

temp=wavelet;

fct=cos(param.angle*pi/180) ./cos(pi*angles/180);

temp.step=wavelet.step*fct(1);
temp.first=wavelet.first*fct(1);
temp.last=wavelet.last*fct(1);
wavelets=s_resample(temp,wavelet.step,{'option','wavelet'});

for ii=2:length(angles)
   temp.step=wavelet.step*fct(ii);
   temp.first=wavelet.first*fct(ii);
   temp.last=wavelet.last*fct(ii);
   wavelets=s_append(wavelets,s_resample(temp,wavelet.step,{'option','wavelet'}));
end

wavelets=s_header(wavelets,'add_ne','angle',angles,'degree','Angle of incidence');
%wavelets=s_rm_trace_nulls(wavelets)
