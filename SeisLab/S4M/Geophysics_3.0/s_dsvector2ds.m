function seismic1=s_dsvector2ds(seismic)
% Combine the traces of a dataset vector into a single seismic dataset.
%
% See also: s_ds2dsvector
%      
% Written by: E. Rietsch: September 19, 2006
% Last updated: February 5, 2008: add a header to the output dataset to indicate
%                                 from which vector element a trace comes
%
%           seismic1=s_dsvector2ds(seismic)
% INPUT
% seismic   seismic dataset vector
% OUTPUT
% seismic1  single seismic dataset; 
%           the field "name" of the output dataset is set to "seismic(1).name"
%           provided it is the same for all datasets;
%           if not, is set to the variable name of the input dataset vector, 
%           if it is not empty, or to "Seismic from DS vector" if it is empty.
%           A new trace header, "vector_index", indicates from which vector
%           element a particular trace comes.
%           Another header, "orig_trace_no", indicates the number of the trace
%           in that vector element.
%
% EXAMPLE
%           seismic=s_data;
%           seismic1=s_ds2dsvector(seismic);   % Create dataset vector
%           seismic2=s_dsvector2ds(seismic1);  % Convert dataset vector back to single dataset
%           s_compare(seismic,seismic2)        % Must be the same

% UPDATE HISTORY
%           May 2, 2007: Improved handling of new dataset name.


param.null=NaN;

nds=length(seismic);

%        Add header identifying the index of the dataset vector
if isfield(seismic(1),'headers')
   for ii=1:nds
      ntr=size(seismic(ii).traces,2);
      seismic(ii)=ds_add_header(seismic(ii),ii,{'vector_index','n/a','Index of original vector'});
      seismic(ii)=ds_add_header(seismic(ii),1:ntr,{'orig_trace_no','n/a','Original trace number'});
   end
else
   [seismic.header_info]=deal({'vector_index','n/a','Index of original vector'});
   [seismic.header]=deal(1);
   for ii=1:nds
      ntr=size(seismic(ii).traces,2);
      seismic(ii).header=[ii*ones(1,ntr);1:ntr];
   end
end

seismic1=seismic(1);

%% Try straight-forward concatenation
try
%   problem='differing start times';
   first=[seismic.first];
   if any(diff(first) ~= 0)
      error('Incompatible start times.')
   end

%   problem='differing end times';
   last=[seismic.last];
   if any(diff(last) ~= 0)
      error('Incompatible end times.')
   end

%   problem='incompatible headers';
   seismic1.headers=[seismic.headers];
   
%   problem='incompatible seismic traces';
   seismic1.traces=[seismic.traces];
   header_mnem=[seismic.header_info];
   header_mnem=header_mnem(:,1:3:end);
   if length(unique(header_mnem)) ~= size(seismic1.header_info,1)
      disp(' Input datasets do not have the same header mnemonics.')
      drawnow
      error('Abnormal termination.')
   end
   
%%       Staight-forward concatenation was not possible  
catch         
   seismic1=add_header(seismic1,1,{'vector_index','n/a','Index of original vector'});
   for ii=2:size(seismic(:));
      seismic1=s_append(seismic1,seismic(ii),{'null',param.null});
   end
end


%%    Do final clean-up

%       Establish a name for the dataset
name={seismic.name};
seismic1.name=unique(name);
if length(name) > 1
   if ~isempty(inputname(1))
      seismic1.name=inputname(1);
   else
      seismic1.name='Seismic from DS vector';
   end
else
   seismic1.name=name{1};
end

if ~isnull(seismic1)
   if any(isnan(seismic1.traces(:)))
      if strcmp(class(seismic1.traces),'single')
         seismic1.null=single(NaN);
      else
         seismic1.null=NaN;
      end
   end
end
