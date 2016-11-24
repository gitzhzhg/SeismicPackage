function seismic=s_window(seismic,window,varargin)
% Function applies window/taper to all traces of a seismic dataset.
%
% Written by: E. Rietsch: March 21, 2004
% Last updated: October 11, 2006: add keyword-controlled parameters
%
%           seismic=s_window(seismic,window,varargin)
% INPUT
% seismic  seismic data set
% window   string specifying the type of window
%          Possible window types are (not case-sensitive):
%    'Bartlett'
%    'Blackman'
%    'cosine'     rectangular window with cosine taper (input keyword 'parameter' 
%                 used to define length of taper as a fraction of the total
%                 window length; 0 <= parameter <= 0.5)
%    'Dolph'
%    'Gauss'      an optional keyword 'parameter' sets both end values. 
%                 Default: parameter=0.005
%    'Hamming'    raised cosine window
%    'Hanning'    cosine-shaped window
%    'Harris',
%    'Kaiser'
%    'none'       same as 'rect'
%    'Nutbess'
%    'Nuttall
%    'Papoulis'
%    'Parzen'
%    'rect',      
%    'sine'       rectangular window with sine taper (input keyword 'parameter' 
%                 used to define length of taper as a fraction of the total
%                 window length; 0 <= parameter <= 0.5)
%    'trapezoid'  rectangular window with linear taper (input keyword 'parameter' 
%                 used to define length of taper as a fraction of the total
%                 window length; 0 <= parameter <= 0.5)
%    'triang'     same as 'Bartlett' or as 'trapezoid' with parameter=0.5
% varargin  one or more cell arrays; the first element of each cell array is a
%           keyword, the other elements are parameters. Presently, keywords are:
%    'option'   possible values are -1, 0, 1
%          This parameter allows one to specify if the full window should
%          be used (option=0) or the first half (option=-1) or the 
%          last half (option=1).
%          A window with option 1 might be applied to a maximum-phase wavelet; 
%                        option 0 might be applied to a zero-phase wavelet;
%                        option -1 might be applied to a minimum-phase wavelet.
%          Default: {'option',0}
%    'wparam'   parameter for some of the windows; 
%          Default: {'wparam',0.25}   for "cosine", "sine", and "trapezoid" window
%                   {'wparam',0.005}  for "Gauss" window
%    'taperlength' alternative way to input taper length for "cosine", "sine", 
%          and "trapezoid" window. Length of the taper; the parameter "wparam" 
%          is computed from it as wparam=taperlength/length_of_seismic_traces
%          Default: {'taperlength',[]}    This means that the value associated 
%                                         with keyword "wparam" will be used.
% OUTPUT
% seismic  input data set with window applied to each trace
%
% EXAMPLE
%        seismic=s_convert(ones(101,1),0,4);
%        seismic.name='Constant';
%        s_wplot(s_window(seismic,'parzen',{'option',-1}))
%        s_wplot(s_window(seismic,'cosine',{'option', 0}))
%        s_wplot(s_window(seismic,'parzen',{'option', 1}))

%	Set default parameters
param.option=0;
param.wparam=[];
param.taperlength=[];
param.add_window2name=true;   % Add window name to dataset name

%	Decode input arguments
param=assign_input(param,varargin);

[nsamp,ntr]=size(seismic.traces);

wndws={'BartHann','Bartlett','Blackman','cosine','Dolph','Gauss','Hamming', ...
       'Hanning','Harris','Kaiser','none','Nutbess','Nuttall','Papoulis',  ...
       'Parzen','Rect','sine','spline','trapezoid','triang'};                 ...

if ~isempty(param.taperlength) && ismember(window,{'cosine','sine','trapezoid'})
   param.wparam=param.taperlength/((nsamp-1)*seismic.step);
end

idx=find(ismember(lower(wndws),lower(window)));
if isempty(idx)
   disp([' Unknown window type: ',window])
   disp(' Possible types are:')
   disp(cell2str(wndws,','));
   error('Abnormal termination')
end

%       Create the window for the three options
if param.option == 0
   wndw=mywindow(nsamp,wndws{idx},param.wparam);
elseif param.option > 0
   wndw=mywindow(2*nsamp-1,wndws{idx},param.wparam);
   wndw=wndw(1:nsamp);
else
   wndw=mywindow(2*nsamp-1,wndws{idx},param.wparam);
   wndw=wndw(nsamp:end);
end

%       Apply window to seismic traces
for ii=1:ntr
   seismic.traces(:,ii)=seismic.traces(:,ii).*wndw;
end
if isyes(param.add_window2name)
   seismic.name=[seismic.name,' (',wndws{idx},')'];
end

%    Append history field
htext=[wndws{idx},' applied to seismic'];
seismic=s_history(seismic,'append',htext);
