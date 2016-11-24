function seismic1=s_multi2one_trace(seismic,header)
% Function converts the traces of a data set into individual one-trace datasets.
% The result is a structure vector of data sets.
%
%           OBSOLETE!, please use "s_ds2dsvector" instead.
%
% Written by: E. Rietsch: August 23, 2005
% Last updated: July 26, 2006: Pre-allocate structure array
%
%           seismic1=s_multi2one_trace(seismic,header)
% INPUT
% seismic   seismic dataset
% header    optional; mnemonic of header whose value is to be added to 
%           the dataset name as (header = headervalue)
% OUTPUT
% seismic1  structure vector of one-trace datasets; thus selecting the third
%           trace
%              temp=s_select(seismic,{'traces',3}), 
%	    where "seismic" is the input dataset can now be achieved by
%              temp=seismic1(3)
%           where "seismic1" is the output data set
%
% SEE ALSO
%           s_one2multi_trace

alert('OBSOLETE!, please use "s_ds2dsvector" instead.')

ntr=size(seismic.traces,2);
%seismic1=struct('name',cell(ntr,1));  % Pre-allocate structure array
if nargin == 2
   values=s_gh(seismic,header);

   for ii=ntr:-1:1    
      temp=s_select(seismic,{'traces',ii});
      temp.name=[temp.name,' (',header,'=',num2str(values(ii)),')'];
      seismic1(ii)=temp;  %#ok   Loop started from last element
   end

else
   for ii=ntr:-1:1   
      seismic1(ii)=s_select(seismic,{'traces',ii}); %#ok   Loop started from last element
   end
end
