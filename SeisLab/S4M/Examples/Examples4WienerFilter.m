% Examples4WienerFilter
% The script illustrates the use of function "s_wiener_filter".

keep WF
presets
global S4M    %#ok

%       Create a zero-phase wavelet and apply a 90-degree phase rotation
step=2;
wavelet0=s_create_wavelet({'step',step},{'wlength',400});
wavelet0.name='zero-phase wavelet';
wavelet90=s_phase_rotation(wavelet0,90);
wavelet90.name='90-degree-phase wavelet';

s_compare(wavelet0,wavelet90,{'peak_fill2','gray'},{'wiggle_color2','gray'})
s_spectrum(wavelet90,{'plot','both'},{'bestshift',true})

%%       A single-trace example
%       Design a Wiener filter that converts wavelet0 into wavelet90
filter=s_wiener_filter(wavelet0,wavelet90,{'flength',100});
s_wplot(filter)

%       Filter the zero-phase wavelet and give the filtered wavelet the same 
%       start and end times as the zero-phase wavelet
wavelet_wiener=s_convolve(wavelet0,filter);
wavelet_wiener=s_select(wavelet_wiener,{'times',wavelet90.first,wavelet90.last});
wavelet_wiener.name='Wiener-filterd zero-phase wavelet';

%       Compute the differenc between the 90-degree waqvelet and the
%       filtered zero-phase wavelet
difference=wavelet90-wavelet_wiener.traces;
difference.name='difference';

%       Display the result
lfigure
subplot(1,3,1)
   aux=s_wplot(wavelet90,{'times',-100,100},{'figure','old'},{'deflection',0.95});
   mytitle(wavelet90.name,{'fontsize',10})

subplot(1,3,2)
   s_wplot(wavelet_wiener,{'times',-100,100},{'figure','old'}, ...
                          {'scale',aux.scale},{'deflection',0.95})
   
subplot(1,3,3)
   s_wplot(difference,{'times',-100,100},{'figure','old'}, ...
                         {'scale',aux.scale},{'deflection',0.95})
   mytitle('Difference',{'fontsize',10})

mysuptitle('Comparison of 90-degree wavelet and Wiener-filtered zero-degree wavelet',{'color','blue'})


%%       A multi-trace example
%       Create noisy data
randn('state',999')      % Initialize tha random-number generator to a fixed state    
refl=s_convert(randn(251,10),100,step);
synth0=s_convolve(refl,wavelet0);
synth90=s_convolve(refl,wavelet90);
synth0=s_noise(synth0,{'ratio',0.1},{'ormsby',0,5,20,125},{'output','seismic'},{'seed',1111});
synth90=s_noise(synth90,{'ratio',0.1},{'ormsby',0,5,20,125},{'output','seismic'},{'seed',2222});


%       Compute a single Wiener filter that converts "synth0" into "synth90"
filter1=s_wiener_filter(synth0,synth90,{'flength',100},{'option','dataset'},{'wnoise',0.1});
filter1.name='filter 1';

%       Compute an individual Wiener filter for each seismic trace that 
%       converts a specific trace of "synth0" into the corresponding 
%      trace of "synth90"; these filters are similar but not identical
filter10=s_wiener_filter(synth0,synth90,{'flength',100},{'option','tracewise'},{'wnoise',0.1});
filter10.name='filter 10';

lfigure
subplot(1,3,1)
aux=s_wplot(filter1,{'times',-80,80},{'figure','old'},{'deflection',1});

subplot(1,3,2)
   s_wplot(filter10,{'times',-80,80},{'figure','old'}, ...
                          {'scale',aux.scale},{'deflection',1})
                      
 subplot(1,3,3)
   s_wplot(s_stack(filter10),{'times',-80,80},{'figure','old'}, ...
                          {'scale',aux.scale},{'deflection',1})
   mytitle('Stack of "Filter 10"')
   
   
   
