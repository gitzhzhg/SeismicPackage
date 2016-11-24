function seismic=s_data_3d
% Generate test dataset consisting of 12 traces of filtered random Gaussian
% noise (1000 ms long, 4 ms sample interval)
% The dataset has one header('CDP')
%
% Written by: E. Rietsch: October 22, 2006
% Last updated: July 14, 2007: Changed filter
%
%           seismic=s_data_3d

run_presets_if_needed

%     Remove higher frequencies from the inline and cross-line directions
randn('state',99999)
mat=fft2(randn(12,20,251));
mat(3:11,:,:)=0;
mat(:,3:19,:)=0;

mat=real(ifft2(mat));
mat=permute(mat,[3,1,2]);
seismic=s_convert(reshape(mat,251,12,[]),0,4);
seismic=s_filter(seismic,{'ormsby',10,15,30,60});

[iline,xline]=meshgrid(1001:1020,2601:2612);
seismic=ds_header(seismic,'add','iline_no',iline(:)','n/a','Inline number');
seismic=ds_header(seismic,'add','xline_no',xline(:)','n/a','Cross-line number');

seismic.name='3-D test data';

seismic=s_history(seismic,'add','Synthetic filtered noise');
