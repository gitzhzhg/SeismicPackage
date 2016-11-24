% Seismic_examples1
%       Example of the usage of seismic functions 

keep WF
presets
global S4M    %#ok

%       Create 50 reflection coefficient series from random Gaussian noise
reflect=s_convert(randn(251,50),0,4);

%	Create a spike plot of the reflection coefficients
s_wplot(reflect,{'quality','spikes'},{'title','Reflection coefficients'})

%       Create a Ricker wavelet
wavelet=s_create_wavelet({'type','Ricker'});

%       Convolve reflection coefficient series with wavelet
synthetic=s_convolve(reflect,wavelet);

%       Add a header (CDP) to the synthetic
synthetic=ds_header(synthetic,'add','CDP',101:150,'n/a','CDP number');

%       Select the time range from 0 to 1000 ms
synthetic=s_select(synthetic,{'times',0,1000});

%       Create a color plot of the synthetic
s_cplot(synthetic);

%       Write the synthetic to a SEG-Y file
filename=fullfile(tempdir,'test.sgy');
write_segy_file(synthetic,filename);

%       Read a subset of the SEG-Y file (include traces with even CDP's less than 121)
seismic=read_segy_file(filename,{'traces','CDP < 121  &  mod(CDP,2) == 0'});

%       Make a wiggle-trace plot of the seismic data set (specify peak fills 
%       and trough fills other than the default and annotate traces by CDP number)
s_wplot(seismic,{'trough_fill','gray'},{'peak_fill','red'},{'annotation','CDP'})

%       Apply an Ormsby band-pass filter with corner frequencies 0, 10, 
%       20, 40 Hz to the seismic 
fseismic=s_filter(seismic,{'ormsby',0,10,20,40});

%       Plot the filtered seismic on top of the unfiltered seismic
s_compare(seismic,fseismic)
mytitle('Original (black) vs. filtered (red) seismic')

%       Plot the spectra of the original seismic, the filtered seismic, and the wavelet.
s_spectrum(seismic,fseismic,wavelet)
