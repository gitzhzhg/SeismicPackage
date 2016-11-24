function seismic1=s_ds2dsvector(seismic,header)
% Convert dataset into individual a vector of one-trace or multi-trace datasets 
% based on a header value. The result is a structure matrix of
% seismic data sets.
% See also: s_dsvector2ds
%
% Written by: E. Rietsch: September 18, 2006
% Last updated: August 27, 2007: Handle case where dataset has no headers; change name
%
%           seismic1=s_ds2sdvector(seismic,header)
% INPUT
% seismic   seismic dataset
% header    optional; mnemonic of a header; all traces with the same header 
%           value are collected in an entry of the data set vector.
%           That header value is added to the dataset name as (header = headervalue)
%           Default: header='trace_no'
% OUTPUT
% seismic1  structure vector of one-trace datasets; thus selecting the third
%           trace
%              temp=s_select(seismic,{'traces',3}), 
%	    where "seismic" is the input dataset can now be achieved by
%              temp=seismic1(3)
%           where "seismic1" is the output data set
%
% EXAMPLE
%           seismic=s_data;
%           seismic1=s_ds2dsvector(seismic)
%           s_compare(seismic1(2),s_select(seismic,{'traces',2})) % Must be the same

if ~istype(seismic,'seismic')
   error('First input argument must be a seismic data set.')
elseif length(seismic) > 1
   error('First input argument must not be a vector of seismic data sets.')
end

if nargin == 1  ||  strcmpi(header,'trace_no')  ||  ~isfield(seismic,'headers')
   ntr=size(seismic.traces,2);
   for ii=ntr:-1:1
      seismic1(ii)=s_select(seismic,{'traces',ii}); %#ok Array does not grow since it is filled from the end
   end
   return
end

headervals=s_gh(seismic,header);
uheadervals=unique(headervals);
nvect=length(uheadervals);

temp=seismic;

for ii=nvect:-1:1
   bool=ismember(headervals,uheadervals(ii));
   temp.traces=seismic.traces(:,bool);
   temp.headers=seismic.headers(:,bool);
   temp.name=[seismic.name,' (',header,'=',num2str(uheadervals(ii)),')'];
   seismic1(ii)=temp;    %#ok Array does not grow since it is filled from the end
end
