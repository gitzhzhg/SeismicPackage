function scale_factors=s_scale_seismic_data_sets(seis1,seis2,varargin)
% Compute scale factors to apply to "seis2" so that its amplitudes 
% approximately match "seis1". The distribution of scalers can then be used  
% to compute a probability distribution relating amplitudes in "seis2" to 
% those in "seis1"
% Written by: E. Rietsch: July 11, 2003
% Last updated: September 23, 2003: remove all zeros before performing the comparison
%
%        scale_factors=s_scale_seismic_data_sets(seis1,seis2,varargin)
% INPUT
% seis1  reference seismic data set
% seis2  seismic data set to be scaled
% varargin    one or more cell arrays; the first element of each cell array is a
%             keyword,the other elements are parameters. Presently, keywords are:
%       'alpha'   parameter(s) for alpha-trimming; 
%             samples (max(nsamp*alpha1,1) to nsamp*(1-alpha2) are used
%             if only one value is given then alpha1=alpha2=alpha
%             (alpha1=alpha2 = 0   ==> mean, 
%              alpha1=alpha2 = 0.5 ==> median)
%             Default: {'alpha', 0.25,0.25})
%       'sample_size'  sample size to choose
%             Default: {'sample_size',value}   where
%             value=min(length(size1.traces(:)),length(size2.traces(:)), ...
%                   max(length(size1.traces(:)),length(size2.traces(:))/sqrt(scalers))
%       'scalers'   number of scale factors to compute
%             Default: {'scales',500}
%       'seed'  seed for random-number generator
%             Default: {'seed',1111}
% OUTPUT
% scale_factors  all the scale factors found that match subsets of "seis2" to subsets "seis1"

test1=abs(seis1.traces(:));
test2=abs(seis2.traces(:));

%       Ged rid of zeros
test1=test1(test1 > 0);
test2=test2(test2 > 0);

nsamp1=length(test1);
nsamp2=length(test2);

%[nsamp1,ntr1]=size(seis1.traces);
%[nsamp2,ntr2]=size(seis2.traces);

%       Defaults for input arguments
param.alpha=0.25;
% param.sample_size=max(nsamp1,ntr1);
param.scalers=500;
temp=min([nsamp1,nsamp2,5000000/param.scalers]);
%temp=round(min(temp,max(nsamp1*ntr1,nsamp2*ntr2)/sqrt(param.scalers)));
param.sample_size=temp;
param.seed=1111;

%       Get input arguments
param=assign_input(param,varargin);

if ~iscell(param.alpha)
   alpha=param.alpha*ones(1,2);
else
   alpha=cell2mat(param.alpha);
end

disp(['sample size: ',num2str(param.sample_size)])

if isempty(param.sample_size)
%   param.sample_size=min(temp1,temp2/sqrt(param.scalers));
end

%test1=abs(seis1.traces(:));
%test2=abs(seis2.traces(:));

n1=length(test1);
n2=length(test2);

rand('state',param.seed);

% scales=zeros(param.scalers,1);
idx1=ceil(n1*rand(param.sample_size,param.scalers));
idx2=ceil(n2*rand(param.sample_size,param.scalers));
if alpha(1) == 0.5 && alpha(2) == 0.5
   scale1=median(test1(idx1));
   scale2=median(test2(idx2));

elseif alpha(1) == 0 && alpha(2) == 0
   scale1=mean(test1(idx1));
   scale2=mean(test2(idx2));

else
   alpha1=max(param.sample_size*alpha(1),1);
   alpha2=param.sample_size-param.sample_size*alpha(2);
%   ialpha=param.sample_size-alpha;
   test1=test1(idx1);
   test2=test2(idx2);
   test1=sort(test1);
   test2=sort(test2);
   index=round(alpha1):1:round(alpha2);
   scale1=mean(test1(index,:));
   scale2=mean(test2(index,:));
end

scale_factors=scale1./scale2;
