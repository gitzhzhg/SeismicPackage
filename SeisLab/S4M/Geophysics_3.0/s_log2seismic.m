function seismic=s_log2seismic(wlog,mnems)
% Function converts log curves whose mnemonics are listed in the second input
% argument into a seismic data set. The first column of the log structure must
% be equidistantly sampled. If its units are in depth the "time units" of the
% resulting seismic-format log are in depth as well. In this case a warning 
% will be issued; however, if the log is intended for visual comparison with
% seismic data plotted with depth as the vertical axis this warning should be
% disregarded.
%
% Written by: E. Rietsch: August 8, 2002
% Last updated: July 22, 2009: Add "null" field to seismic structure
%
%	    seismic=log2seismic(wlog,mnems)
% INPUT
% wlog      log structure
% nmems     cell array with one or more header mnemonics
%           if there is only one header mnemonic, it can be a simple string
%           if no header mnemonics are given, all curves in the log structure
%           (with the exception of the first column (depth)) are converted
% OUTPUT
% seismic   seismic data set; the dataset has an additional field "trace_info"
%           which contains, for the selected curves, the information in field "curve_info"
%           Hence, one can establish later which trace represents which curve of
%           the input dataset
%
% UPDATE HISTORY
%           April 1, 2005: New seismic structure fields added.


global S4M      

%     Check input data
if ~isfield(wlog,'step') || wlog.step == 0
   error(' Input log must be uniformly sampled.')
end

if nargin == 1
   mnems=wlog.curve_info(2:end,1)';
else
   if ~iscell(mnems)
      mnems={mnems};
   end
end

%     Check if curves with these mnemonics are present
idx=find(ismember(lower(wlog.curve_info(:,1)),lower(mnems)));
if length(idx) < length(mnems)
   disp(' The following curves were requested:')
   disp(mnems)
   disp(' The following requested curves were found:')
   disp(wlog.curve_info(idx,1)')
   disp(' The following curves are available:')
   disp(wlog.curve_info(:,1)')
   error(' Abnormal termination')
end

seismic.type='seismic';
seismic.tag='well log';
seismic.name=wlog.name;
seismic.first=wlog.first;
seismic.step=wlog.step;
seismic.last=wlog.last;
seismic.units=wlog.curve_info{1,2};
seismic.traces=wlog.curves(:,idx);
seismic.trace_info=wlog.curve_info(idx,:);

if ismember(seismic.units,{'m','ft'})
   disp([' Warning: Seismic "time units" are in ',seismic.units,'.'])
end

if isnull(wlog)
   if any(isnan(seismic.traces(:)))
      seismic.null=NaN;
   end
else
   seismic.null=[];
end

%     Create S4M.history field
if isempty(S4M.history) || S4M.history
   seismic=s_history(seismic,'add',['Curves converted:',cell2str(seismic.trace_info(:,1))]);
end


