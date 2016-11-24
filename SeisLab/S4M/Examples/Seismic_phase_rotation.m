% Seismic_phase_rotation
%       Example of the usage of "s_phase_rotation"

keep WF
presets
global S4M    %#ok

%%      Phase rotation
%       Create a zero-pase wavelet and a minimum-phase wavelet
step=1;
waveletz=s_create_wavelet({'wlength',400},{'step',step});
waveletz=add_header(waveletz,0,{'type','n/a','Typ'});

waveletm=s_create_wavelet({'wlength',400},{'type','min-phase'},{'step',step});
waveletm=add_header(waveletm,1,{'type','n/a','Typ'});

%       Combine the two wavelets into one dataset
both=s_append(waveletz,waveletm,{'null',0});
both.name='zero-phase & min-phase';

%       Apply phase rotations to the two wavelets
wavelets=s_phase_rotation(both,0:30:180);
s_wplot(wavelets,{'annotation','phase'},{'aindex',1:111},{'deflection',1})

%       Convert the dataset into a two-entry dataset vector. The first
%       entry contains the phase-rotates zero-phase wavelet, the second the
%       phase-rotated minimum-phase wavelet. The header "type" is used to
%       distinguish the two wavelets
vwav=s_ds2dsvector(wavelets,'type');
vwav(1).name='phase-rotated zero-phase wavelets';
vwav(2).name='phase-rotated minimum-phase wavelets';
s_wplot(vwav(2),{'deflection',1},{'annotation','phase'});

zphase=s_add_header4phase(vwav(1));
mphase=s_add_header4phase(vwav(2));

%       Display the new headers
disp('Phase shift and time shift of the phase-rotated zero-phase wavelets:')
disp(s_gh(zphase,'phase_shift'))
disp(s_gh(zphase,'time_shift'))

disp('Phase shift and time shift of the phase-rotated minimum-phase wavelets:')
disp(s_gh(mphase,'phase_shift'))
disp(s_gh(mphase,'time_shift'))

%   Looking at the time shift (41 ms) and phase shift (-42 degrees) for the
%   original minimum-phase signal one can expect that a zero phase signal 
%   shifted by 41 ms and phase shifted by -42 degrees has a shape similar 
%   to a minimum-phase wavelet. This is illustrated in the following:

temp=s_shift(waveletz,{'shifts',41});
temp=s_phase_rotation(temp,-42);
temp.name='phase & time shifted zero-phase wavelet';
s_compare(waveletm,temp,{'times',-100,200},{'peak_fill2','gray'}, ...
          {'wiggle_color2','gray'})
mytitle({'Ormsby (min-phase, 10-20-40-60 Hz) [black] vs.'; 
         'phase & time shifted zero-phase wavelet [gray]'});


