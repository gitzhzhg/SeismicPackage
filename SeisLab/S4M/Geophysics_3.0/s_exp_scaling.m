function [seismic,aux]=s_exp_scaling(seismic,varargin)
% Function applies an exponential scale factor to each trace of "seismic'.
%	s(t) ==> exp(-c*t)*s(t)
%
% Written by: E. Rietsch: January 9, 2004
% Last updated: October 29, 2005: Append info to history field
%
%             [seismic,aux]=s_exp_scaling(seismic,varargin)
% INPUT
% seismic   seismic data set
% varargin  one or more cell arrays; the first element of each cell array is a
%           keyword, the other elements are parameters. Presently, keywords are:
%       'coefficient'    coefficient "c" in  exp(-c*t); no default
%       'amp100'         reduction in amplitude after 100 ms
%                        no default;
%       'amp1000'        reduction in amplitude after 1000 ms
%                        no default; the three parameters are checked in the order
%                        "coefficient", "amp100", "amp1000"
% OUTPUT
% seismic   scaled seismic data set
% aux       structure with field "coefficient"; coefficient "c" in  exp(-c*t)
%                                "decayps"      decay per sample, i.e. exp(-c*step)

% Set default parameters
param.final_amp=0.1;
param.coefficient=[];
param.amp100=[];
param.amp1000=[];

%        Decode input arguments and assign values to defaults structure
param=assign_input(param,varargin);

ntr=size(seismic.traces,2);

if isnull(seismic)
   for ii=1:ntr
      idx=find(~isnan(seismic.traces(:,ii)));
      nsamp1=idx(end)-idx(1)+1;
      seismic.traces(1:nsamp1,ii)=seismic.traces(idx(1):idx(end),ii);
      seismic.traces(nsamp1+1:end,ii)=NaN;
   end
end

test=~isnan(seismic.traces);
index=find(any(test,2));
seismic.traces=seismic.traces(1:index(end),:);
seismic.last=seismic.first+(index(end)-1)*seismic.step;

nsamp=size(seismic.traces,1);

if ~isempty(param.coefficient)
   coeff=param.coefficient;

elseif ~isempty(param.amp100)
   coeff=-log(param.amp100)/100;

elseif ~isempty(param.amp1000)
   coeff=-log(param.amp1000)/1000;

else
   tlength=seismic.last-seismic.first;
   coeff=-log(param.final_amp)/tlength;

end

fact=exp(-coeff*(0:nsamp-1)'*seismic.step);

for ii=1:ntr
   seismic.traces(:,ii)=seismic.traces(:,ii).*fact;
end

aux.coefficient=coeff;
aux.decayps=exp(-coeff*seismic.step);

%	Append line to history field of "seismic"
seismic=s_history(seismic,'append',['Scale coefficient: ',num2str(coeff)]);
