function seismic=s_noise(seismic,varargin)
% Function computes zero-mean noise with the same structure as the input data set
% Depending on the option chosen the output is either the noise or the input
% data with the noise added.
%
% Written by: E. Rietsch: July 30, 2000
% Last updated: September 29, 2009: bug fix
%
%            seismic=s_noise(seismic,varargin)
% INPUT
% seismic    seismic structure; (optional if none of the "varargin arguments are 
%            given either; in this case 12 traces if 1 sec of white Gaussian 
%            noise ware generated with 4 ms sample interval).
% varargin   one or more cell arrays; the first element of each cell array is a keyword,
%            the other elements are parameters. Presently, keywords are:
%            'ratio'   ratio of the amplitude of the noise to the "amplitude" of the 
%                 seismic. Default: {'amplitude',1}
%            'amplitude'  describes the way amplitude is measured. Possible options are:
%                 'max' (maximum absolute amplitude) or 'median' (median of the absolute
%                 amplitude). Default: {'type','median'}
%            'type'    type of noise. Possible options are: 'uniform' and 'gaussian'.
%                 Default: {'type','gaussian'}
%            'ormsby'   four corner frequencies of Ormsby filter to apply to the noise
%                 prior to amplitude scaling. 
%                 Default: {'ormsby',[]} (this implies white noise) 
%            'output'  type of output. Possible values are: 'noise' and 'seismic' (i.e.
%                 noise added to seismic. 
%                 Default: {'output','noise'}
%            'seed'  initial state of the random number generator. Possible values are 
%                 non-negative integers and []. In the latter case the
%                 current state is kept.
%                 Default: {'seed',9999}        
% OUTPUT
% seismic     noise or seismic with noise
%
% EXAMPLE
%      seismic=s_data;
%      noise=s_noise(s_data,{'type','uniform'});
%      s_wplot(noise)

if ~istype(seismic,'seismic')  
   error(' First input argument must be a seismic dataset.')
end

%       Set defaults for input parameters
param.amplitude='median';
param.ormsby=[];
param.output='noise';
param.ratio=1;
param.seed=9999;
param.type='gaussian';

param.frequencies=[];    % Obsolete
param.rnstate=[];        % Obsolete

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

%	Handle legacy keywords
if ~isempty(param.rnstate)
   alert('Keyword "rnstate" is obsolete. Replace it by "seed".')
   param.seed=param.rnstate;
end
if ~isempty(param.frequencies)
   alert('Keyword "frequencies" is obsolete. Replace it by "ormsby".')
   param.ormsby=param.frequencies;
end

[nsamp,ntr]=size(seismic.traces);

switch param.type
case 'gaussian'
   if ~isempty(param.seed)
      randn('state',param.seed);
   end
   temp=randn(nsamp,ntr);
case 'uniform'
    if ~isempty(param.seed)
      rand('state',param.seed);
   end
   temp=rand(nsamp,ntr)-0.5;
otherwise
   error([' Unknown type of noise (',param.type,')'])
end

%       Apply filter if required
if ~isempty(param.ormsby)
   if iscell(param.ormsby)
      param.ormsby=cell2num(param.ormsby);
   end
   temp=s_filter(s_convert(temp,seismic.first,seismic.step), ...
        {'ormsby',param.ormsby});
   temp=temp.traces;
end

%       Determine amplitude scale
if strcmpi(param.amplitude,'median')
   amp=median(median(abs(seismic.traces)));
   ampn=median(median(abs(temp)));
elseif strcmpi(param.amplitude,'max')
   amp=max(max(abs(seismic.traces)));
   ampn=max(max(abs(temp)));
else
   error([' Unknown type of amplitude definition (',param.amplitude,')'])
end

%       Create output
htext=[param.type,' noise; amplitude ratio = ',num2str(param.ratio),' of ',param.amplitude];

if strcmpi(param.output,'seismic')
   seismic.traces=seismic.traces+(param.ratio*amp/ampn)*temp;
   seismic=s_history(seismic,'append',htext);

elseif strcmpi(param.output,'noise')
   seismic=s_convert((param.ratio*amp/ampn)*temp,seismic.first,seismic.step, ...
   htext,seismic.units);

else
   error([' Unknown type of input parameter "seismic" (',param.seismic,')'])
end
                   