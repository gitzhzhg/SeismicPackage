function bool=isheader(seismic,mnem)
% Output is true if iheder with mnemonic "mnem" exists
%
% Written by: E. Rietsch: July 10, 2009
% Last updated:
%
% INPUT
% seismic   seismic dataset or vector of seismic datasets
% mnem      header mnemonic
% OUTPUT
% bool      vector of logical variables (same length as vector of seismic
%           datasets)
%
% EXAMPLE
%           seismic=s_data;
%           isheader(seismic,'cdp')

global S4M

if ~istype(seismic,'seismic')
   error('First input argument must be a seismic dataset or a dataset vector.')
end

nds=length(seismic);

%  Check if the datset has headers
if ~isfield(seismic(1),'header_info')
   bool(1:nds)=false;
   return
end

if nds == 1
   if S4M.case_sensitive
      bool=any(ismember(seismic.header_info(:,1),mnem));
   else
      bool=any(ismember(lower(seismic.header_info(:,1)),lower(mnem)));
   end
   
else
   bool=true(1,nds);
   if S4M.case_sensitive
      for ii=1:nds
         bool(ii)=any(ismember(seismic(ii).header_info(:,1),mnem));
      end
   else
      for ii=1:nds
         bool(ii)=any(ismember(lower(seismic(ii).header_info(:,1)),lower(mnem)));
      end
   end
end
