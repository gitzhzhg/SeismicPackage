function seismic=s_filter(seismic,varargin)
% Function filters seismic input data
%
% Written by: E. Rietsch: May, 2001
% Last updated: December 5, 2005: check for trace nulls and remove them
%
%            seismic=s_filter(seismic,varargin)
% INPUT
% seismic    seismic structure
% varargin   one or more cell arrays; the first element of each cell array is a keyword,
%            the other elements are parameters. Presently, keywords are:
%        'ormsby'    Ormsby filter with corner frequencies f1,f2,f3,f4. The 
%              corner frequencies must satisfy the condition 0 <= f1 <= f2 <= f3 <= f4.
%              General form: {'ormsby',f1,f2,f3,f4}     or
%                            {'ormsby',[f1,f2,f3,f4]}
%              no default
% OUTPUT
% seismic     seismic structure after filtering
%
% EXAMPLE
%       seismic=s_data;
%       fseismic=s_filter(seismic,{'ormsby',0,10,15,30});
%       fseismic.name='Filtered test data';
%       s_compare(seismic,fseismic)
%       s_spectrum(seismic,fseismic,{'frequencies',0,80})

%    	Set default values
param.ormsby=[];

%   	Decode and assign input arguments
param=assign_input(param,varargin,'s_filter');

if isnull(seismic)
   seismic=s_rm_trace_nulls(seismic);
end

fields=fieldnames(param);
idx=ismember_ordered(fields,{'ormsby'});
if ~isempty(idx)
   option=fields{idx(1)};
else
   error(' No filter type specified')
end


switch option

case 'ormsby'

if isempty(param.ormsby)
   error(' No filter parameters specified')
end

%       Retrieve and check corner frequencies of Ormsby filter
if ~iscell(param.ormsby)
   freq=param.ormsby;
else
   freq=cell2num(param.ormsby);
end
ftext=num2str(reshape(freq,1,[]));

if any(diff(freq) < 0)
   error([' The corner frequencies for the Ormsby filter are not monotonic: ',ftext]);
end

%       Handle big data sets by dividing them in smaller chunks
ntr=size(seismic.traces,2);
if ntr > 1000
   ia=1;
   for ii=1:ceil(ntr/1000)
      ie=min(ia+999,ntr);
      seismic.traces(:,ia:ie)=ormsby_filter(seismic.traces(:,ia:ie),seismic.step,freq);
      ia=ie+1;
   end
else
   seismic.traces=ormsby_filter(seismic.traces,seismic.step,freq);
end
ftext=[' with corner frequencies ',ftext];


otherwise
disp([' Unknown FILTER option: ',option])

end	% End of switch block


seismic.name=[seismic.name,' - filtered'];

%       Append history field
htext=[option, ftext];
seismic=s_history(seismic,'append',htext);
