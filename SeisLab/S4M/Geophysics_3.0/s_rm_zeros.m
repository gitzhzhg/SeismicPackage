function seismic=s_rm_zeros(seismic)
% Function removes common zeros at the beginning and end of traces and
% adjusts start and end times
%
% Written by: E. Rietsch
% Last updated: October 19, 2003: Input and output argument are the same
%
%             seismic=s_rm_zeros(seismic)
% INPUT
% seismic     seismic data set
% OUTPUT
% seismic     seismic after removal of common leading and trailing zeros

nsamp=size(seismic.traces,1);

test=max(abs(seismic.traces),[],2);
index=find(test > 0);
if isempty(index)
   error(' Traces are identically zero')
end
seismic.traces=seismic.traces(index(1):index(end),:);
seismic.first=seismic.first+(index(1)-1)*seismic.step;
seismic.last=seismic.last-(nsamp-index(end))*seismic.step;

seismic=s_history(seismic,'append', ...
       [num2str(nsamp-size(seismic.traces,1)),' leading/trailing zeros removed']);
