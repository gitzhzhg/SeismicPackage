function dsout=copy_fields(dsin,dsout)
% Function copies fields in dataset "dsin" to "dsout" if they do not 
% exist in dataset "dsout".
% A field "null" is added if fields "traces" or "curves" contain NaNs.
%
% Written by: E. Rietsch: August 1, 2006
% Last updated:
%
%          dsout=copy_fields(dsin,dsout)
% INPUT
% dsin     dataset from which to copy fields
% dsout    dataset to which to copy fields
% OUPUT
% dsout    second input dataset with additional fields from first data set


if nargin == 1 || isempty(dsout)
   dsout=dsin;
    return
end

fieldsin=fieldnames(dsin);
fieldsout=[fieldnames(dsout);{'null'}];   % "null" is added, so that it 
                                              %  will not be copied
index=find(~ismember(fieldsin,fieldsout));

for ii=1:length(index)
   dsout.(fieldsin{index(ii)})=dsin.(fieldsin{index(ii)});
%  dsout=setfield(dsout,fieldsin{index(ii)},getfield(dsin,fieldsin{index(ii)}));
end

switch dsin.type

case 'well_log'
%       Check for null values in log curves
   if ~isfield(dsout,'null')  &&  any(any(isnan(dsout.curves(:,2:end))))
      dsout.null=NaN;
   end

case 'seismic'
%       Check for null values in dsmic traces
   if ~isfield(dsout,'null')  &&  any(isnan(dsout.traces(:)))
      dsout.null=NaN;
   end

otherwise
   alert(['Unknown dataset type: ',seisin.type])
   
end
