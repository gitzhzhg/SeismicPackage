% Seismic_log_examples1
%	Illustrate the effect of depth-to-time conversion errors using
%       log-related and seismic functions

keep WF
presets
global S4M   %#ok

step=1;
wlength=60;

%	Create a Ricker wavelet
wavelet=s_create_wavelet({'type','Ricker'},{'frequencies',30}, ...
        {'step',step},{'wlength',wlength});

%	Create a synthetic well log
wlog=l_data;

%	Integrate sonic to get two-way time (adds new curve with mnemonic 'twt')
wlog=l_depth2time(wlog);

%	Extract vector with two-way time from the log
twt=l_gc(wlog,'twt');

%	Define a sinusoidal depth-time conversion error function and add
%       it as log curve with mnemonic 'error'
dt_error=sin(30*twt/1000);
wlog=l_curve(wlog,'add','dt_error',dt_error,'ms','Depth-time error');

%	For each of the amplitudes "a" defined below
%	1. Create a new log where the two-way time 'twt" is replaced by twt + a*dt_error
%       2. Switch the depth coordinate of the well log to two-way time
%       3. Resample the well log to the uniform sample interval "step" (here 1 ms)
%       4. Compute reflection coefficients
%       5a. For the first "a" compute a synthetic by convolving the 
%           reflectivity with the wavelet
%       5b. For all other values of "a" append the modified reflectivity to 
%           the previously computed ones
%       6. Estimate/extract the wavelet
%       7. Save estimated wavelets in a seismic structure vector

a=[0,2,4,6,8,10];
for ii=1:length(a)
   wlog1=l_curve_math(wlog,'replace',['twt=twt+',num2str(a(ii)),'*dt_error']);
   wlog1=l_switch_depth(wlog1,'twt');
   wlog1=l_resample(wlog1,step);
   refl=s_log2reflectivity(wlog1,step);
   if ii == 1
	%  Compute a synthetic using the unmodified refleclection coefficients (a=0)
      reflect=s_rm_trace_nulls(refl);
      seismic=s_convolve(wavelet,reflect);
   else
      reflect=s_append(reflect,refl);
   end

   estimated_wavelet=s_wavextra(seismic,refl,{'wlength',wlength}, ...
      {'logshifts',0},{'wnoise',0.001},{'dc_constraint',1},{'scale','no'});
   wavelets(ii)=estimated_wavelet;  %#ok
end

%	Combine the individual wavelets of the structure vector into one data set
wavelets=s_dsvector2ds(wavelets);

%	For display purposes, add to the reflectivity a header with the amplitude 
%       of the depth-time error of the reflectivity that was used to compute it.
reflect=s_header(reflect,'add','dt_error',a,'ms','Maximum stretch/squeeze of well log');

%	For display purposes, add to the wavelet a header with the amplitude of 
%       the depth-time error of the reflectivity that was used to compute it.
wavelets=s_header(wavelets,'add','dt_error',a,'ms','Maximum stretch/squeeze of well log');

%	Compute attributes of the wavelet (such as RMS amplitude, maximom vlue, etc.)
%       and add them to the wavelet headers
wavelets=s_attributes(wavelets);

%	Get the header with maxima of the wavelet
maxima=s_gh(wavelets,'max');

%	Get the header with the cross-correlation between seismic and 
%       synthetic computed during wavelet estimation in "s_wavextra". 
cc=s_gh(wavelets,'cc_wavelet');

%	Plot all reflection coefficient series; use header 'dt_error' 
%       for trace annotation
s_wplot(reflect,{'annotation','dt_error'},{'quality','spikes'})

%	Plot blow-up of the reflection coefficient series
s_wplot(reflect,{'times',450,750},{'annotation','dt_error'},{'quality','spikes'})

%	Plot seismic headers 'max' (maximum wavelet amplitude) and 'cc_wavelet'
%       (cross-correlation between seismic and synthetic) above the wavelets
aux=s_plot_with_header(wavelets,{'headers2plot','max','cc_wavelet'},{'deflection',1}, ...
   {'scale','no'},{'annotation','dt_error'},{'aindex',1:10});
legend(aux.legend_handle,'Maximum amplitude of wavelet','Correlation coefficient',3)
mysuptitle('Maximum wavelet amplitude vs. correlation coefficient')

%	Compute and display the spectra of the individual wavelets
s_spectrum(wavelets,{'average','no'},{'frequencies',0,80})
