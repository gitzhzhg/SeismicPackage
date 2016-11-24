% Seismic_examples3
%       Example of the usage of "s_spectrum" 

keep WF
presets
global S4M   %#ok


%%       Create a minimum-phase wavelet
wavelet=-s_create_wavelet({'type','min-phase'});

%	Compute and display its amplitude and phase spectrum 
%       (using the actual zero-time of the wavelet)
s_spectrum(wavelet,{'plot','amp'},{'frequencies',0,80},{'padding',128}, ...
                   {'timezero','actual'})

%%	Create a two-panel display showing the two different displays of 
%%      the phase spectrum. 
%       The "best" zero-time option uses the time of the peak of the instantaneous 
%       amplitude of the signal as zero-time. This is generally more appropriate
%       for wavelets estimated from seismic data where the correct zero time may
%       be uncertain (hence, it is the default).
%       the "actual" zero-time option uses the actual zero-time of the signal to
%       to compute the phase. This is generally only used for theoretically
%       determined wavelets. 
lfigure
subplot(1,2,1)
s_spectrum(wavelet,{'plot','phase'},{'timezero','best'},{'figure','old'})
mytitle('best')
subplot(1,2,2)
s_spectrum(wavelet,{'plot','phase'},{'timezero','actual'},{'figure','old'})
mytitle('actual')
mysuptitle('Two options for the phase plot using parameter "timezero"',{'color','blue'})


%%	Create a wavelet from seismic data and display its spectrum 
seismic=s_data;
s_spectrum(seismic,wavelet,{'scale','log'})
mytitle('Logarithmic-scale plot of two spectra')
