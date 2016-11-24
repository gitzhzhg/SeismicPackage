% Log2seismic_examples1
% Compute reflection coefficients from a well log and convolve them with a
% wavelet to create a synthetic seismic trace.

keep WF
presets

%%     Get a well log
wlog=l_data;

%%     Create a wavelet
step=2;
wavelet=s_create_wavelet({'type','min-phase'},{'step',step});

%%     Compute reflectivity as a seimic dataset from the well log and plot it
refl=s_log2reflectivity(wlog,step);
s_wplot(refl,{'interpol','linear'},{'quality','spikes'})

%%     Compute and display synthetic seismic trace
seis=s_convolve(wavelet,refl);
seis.name='Minimum-phase synthetic';

s_wplot(seis);

%%     Plot five copies of the synthetic seismic trace
s_wplot(seis,{'traces',ones(1,5)},{'title','Five copies of the synthtic'})
