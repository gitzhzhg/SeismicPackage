function inverse=s_min_phase_inverse(seismic,wlength,varargin)
% Compute the minimum-phase_inverse of a wavelet of a data set; it is assumed
% that the autocorrelation of "seismic" is the scaled autocorrelation of the
% wavelet. Hence, "seismic" can also be a wavelet.
% Written by: E. Rietsch: July 29, 2004
% Last updated: 
%
%           inverse=s_min_phase_inverse(seismic,length,varargin)
% INPUT
% seismic     seismic data
% wlength      length (ms) of the desired inverse
% varargin    one or more cell arrays; the first element of each cell array is 
%             a keyword, the other elements are parameters. Presently, keywords are:
%       'wnoise'    fraction of "white noise" to add to autocorrelation
%                   Default: {'wnoise',0.01}
%       'window' type of window to use.  Possible values are (not case-sensitive): 	
%                    'Hamming', 'Hanning', 'Nuttall',  'Papoulis', 'Harris',
% 	             'Rect',    'Triang',  'Bartlett', 'BartHann', 'Blackman'
% 	             'Gauss',   'Parzen',  'Kaiser',   'Dolph',    'Hanna',
% 	             'Nutbess', 'spline', 'none',''
%                    (the empty string means no window).
%                Default: {'window','Hanning'}

% OUTPUT
% inverse    inverse of the wavelet

%       Set defaults for input arguments
param.window='Hanning';
param.wnoise=0.01;

%       Decode and assign input arguments
param=assign_input(param,varargin);

if strcmpi(param.window,'none')
   param.window='Rect';
end

%       Compute autocorrelations of the seismic traces and stack them
ac=s_correlate(seismic,seismic,{'lags',0,wlength},{'option','corresponding'});
ac=ac.traces;
nac=length(ac);

inverse.type='seismic';
inverse.tag='wavelet';
inverse.name='inverse wavelet';
inverse.first=0;
inverse.last=(nac-1)*seismic.step;
inverse.step=seismic.step;
inverse.units=seismic.units;

if size(seismic.traces,2) > 1
   ac=sum(ac,2);
elseif isfield(seismic,'headers')   
   inverse.header_info=seismic.header_info;
   inverse.headers=seismic.headers;
end

rhs=zeros(nac,1);
rhs(1)=1;
ac(1)=ac(1)*(1+param.wnoise);
inverse.traces=levinson4toeplitz(ac,rhs);
