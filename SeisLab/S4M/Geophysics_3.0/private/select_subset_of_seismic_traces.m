function seismic=select_subset_of_seismic_traces(seismic,timerange,tracerange)
% Select a subset of a seismic dataset. For internal use; no error checking
%
% Written by: E. Rietsch: July 25, 2007
% Last updated:
%
%             seismic=select_subset_of_seismic_traces(seismic,timerange,tracerange)
% INPUT
% seismic     seismic data set
% timerange   vector or cell vector with first and last time to select; all 
%             traces are used if it is empty
% tracerange  can be 
%             a vector of trace numbers 
%             or 
%             a string with a logical expression involving header values
%             to include. The "pseudo-header" 'trace_no' can also
%             be used.
%             or
%             a two-element cell vector. In this case the first entry is
%             a header mnemonic and the second a vector with requires values
%             of that header mnemonic (see "s_select")
%             
% OUTPUT
% seismic     subset of the seismic dataset.
%
% EXAMPLES    
%             seismic=s_data;
%             seismic1=select_subset_of_seismic_traces(seismic,[],2:2:10)
%             seismic2=select_subset_of_seismic_traces(seismic,[500,800],'cdp > 105')
%             seismic3=select_subset_of_seismic_traces(seismic,{500,1000},{'trace_no',2})

global S4M

if ~isempty(tracerange)  || ~isempty(timerange)
   history=S4M.history;       % Preserve value of global variable S4M.history
   S4M.history=false;

   if isnumeric(tracerange)  ||  ischar(tracerange)
      argument={'traces',tracerange};
   elseif iscell(tracerange)
      argument=[{'traces'},tracerange];
   else
      error('Argument "tracerange" must be numeric, a string, or a cell array.')
   end

   if isempty(timerange)
      seismic=s_select(seismic,{'traces',tracerange});

   elseif iscell(timerange)
      seismic=s_select(seismic,argument,{'times',timerange{1},timerange{2}});

   else
      seismic=s_select(seismic,argument,{'times',timerange(1),timerange(2)});

   end

   S4M.history=history;		% Restore original value of history field
end
