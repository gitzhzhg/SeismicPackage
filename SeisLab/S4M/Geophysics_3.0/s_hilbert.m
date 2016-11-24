function seismic=s_hilbert(seismic,varargin)
% Function computes Hilbert transform or instantaneous amplitude of seismic data
%
% Written by: E. Rietsch: October 10, 2000
% Last updated: November 22, 2006: use "isnull"
%
%             seismic=s_hilbert(seismic,varargin)
% INPUT
% seismic     seismic structure
% varargin    one or more cell arrays; the first element of each cell array is a keyword,
%             the other elements are parameters. Presently, keywords are:
%   'output'  Type of output; possible values are: 
%             'hilbert'  (Hilbert transform of the seismic input data)
%             'complex' (seismic+i*hilbert(seismic) )
%             'amplitude' (instantaneous amplitude)
%             Default: {'output','amplitude'}
% OUTPUT
% seismic     seismic structure after the transformation specified via 'output'
%
% EXAMPLE
%              seismic=s_data;
%              hseismic=s_hilbert(seismic,{'output','complex'});
%              s_compare(real(hseismic),imag(hseismic))
%              mytitle('Original data (black) and their Hilbert transform (red)')


%       Set default values
param.output='amplitude';

%       Replace default values by actual input arguments
param=assign_input(param,varargin);

if isnull(seismic)
   disp(' Null values in seismic removed or replaced by zeros via "s_rm_trace_nulls".')
   seismic=s_rm_trace_nulls(seismic);
end

switch param.output

case 'amplitude'
   seismic.traces=abs(myhilbert(seismic.traces));
   seismic.name=['Instantaneous amplitude of "',seismic.name,'"'];
   htext='Instantaneous amplitude';

case 'complex'
   seismic.traces=myhilbert(seismic.traces);
   seismic.name=['Complex trace of "',seismic.name,'"'];
   htext='Complex trace';

case 'hilbert'
   seismic.traces=imag(myhilbert(seismic.traces));
   seismic.name=['Hilbert transform of "',seismic.name,'"'];
   htext='Hilbert transform';

otherwise
   error(['Unknown output request "',param.output,'"'])
   
end

%    Append history field
if isfield(seismic,'history')
   seismic=s_history(seismic,'append',htext);
end
