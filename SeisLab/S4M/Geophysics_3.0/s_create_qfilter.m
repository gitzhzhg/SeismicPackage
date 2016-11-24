function qf=s_create_qfilter(varargin)
% Function computes a series of constant-Q absorption filters. These filters have 
% an amplitude spectrum equal to exp(-pi*f*t/Q) where f is frequency, t is travel 
% time, and Q denotes the quality factor (usually in the range of 30 to 200).
% The total number of traces output equals the product or the number of Q values 
% and time values specified. They are sorted in the order: 
% Q1T1, Q2T1, ... , QnT1, Q1T2, Q2T2, ...
%
% Written by: E. Rietsch: July 4, 2000
% Last updated: January 1, 2007: convert to single-precision if required
%
%        qfilter=s_create_qfilter(varargin)
% INPUT 
% varargin    one or more cell arrays; the first element of each cell array is a 
%             keyword, the other elements are parameters
%       'q'   sequence or vector of Q values
%             Default: {'q',140,120,100,80,60,40}    which is equivalent to
%                      {'q',[140,120,100,80,60,40]}
%       'times'  one or more travel time values
%             Default: {'time',1000}
%       'length'  Length of the absorption filters in ms. Default: {'length',1000}
%       'step'  Sample interval if the absorption filters in ms. Default: {'step',1}
% OUTPUT
% qfilter     seismic structure with the computed filters
%             Headers Q and TIME record the parameters used for each filter
% EXAMPLE     
%       qf = s_create_qfilter({'q',50,100,150},{'times',500,1000})

global S4M

run_presets_if_needed

%	Set default values for input arguments
param.q=[140,120,100,80,60,40];
param.time=[];
param.times=1000;
param.length=1000;
param.step=1;

%       Decode and assign input arguments	
param=assign_input(param,varargin);

if ~isempty(param.time)		% Handle legacy parameter
   param.times=param.time;
   disp(' s_create_qfilter: keyword "time" is deprecated; use "times" instead.')
end

if iscell(param.q)
   param.q=cat(1,param.q{:});
else
   param.q=param.q(:);
end
nq=length(param.q);

if iscell(param.times)
   param.times=cat(1,param.times{:});
end
nt=length(param.times);
nqnt=nq*nt;

nsamp=fix(param.length/param.step)+1;
qf.type='seismic';
qf.tag='wavelet';
qf.name='Q-filter';
qf.first=0;
qf.step=param.step;
qf.last=param.length;
qf.units='ms';
qf.traces=f_qfilter(param.q,param.times,nsamp,param.step);
qf.null=[];
qf.headers=zeros(2,nqnt);
qf.header_info=[{'q','n/a','Absorption parameter'}; ...
                {'time','ms','Two-way travel time used for filter'}];
qf.headers(1,:)=reshape(param.q(:,ones(nt,1)),1,nqnt);
param.times=param.times(:);
qf.headers(2,:)=reshape(param.times(:,ones(nq,1))',1,nqnt);

%     Create history field
if S4M.history
   qf=s_history(qf,'add','Q-filters');
end

%	Convert to single-precision if required
if strcmpi(S4M.precision,'single')
   qf=single(qf);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function qf=f_qfilter(q,t,nsamp,dt)
% Function computes constant-Q absorption filters
% INPUT
% q      array of Q values for which to compute the filters
% t      array of one-way time values for which to compute the filters (ms)
% nsamp  filter length in samples
% dt     sample interval (ms)
% OUTPUT
% qf     samples of absorption filter

nq=length(q);
nt=length(t);
nqnt=nq*nt;
nyquist=500/dt;

nsamp=nsamp*2;          % Compute filter for twice the requested filter length
f=(0:2:nsamp)'*(nyquist/nsamp);
qt=reshape((0.001*pi./q(:))*t(:)',1,nqnt);
amp=exp(-f*qt);
amp=[amp;flipud(amp)];

qf=zeros(nsamp,nqnt);
for ii=1:nqnt
   qf(:,ii)=minimum_phase(amp(:,ii),nsamp);
   qf(:,ii)=qf(:,ii)/sum(qf(:,ii));            % DC component should be 1
end

qf=qf(1:nsamp/2,:);     % Shorten filter to the requested filter length
