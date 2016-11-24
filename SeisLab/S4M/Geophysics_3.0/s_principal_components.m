function [pc,aux]=s_principal_components(seismic,varargin)
% Perform principal-component analysis of the input dataset. 
% The output can be:
% A. The input data represented by any combination of the principal 
%    components --- usually the first few: {'output','seismic'}
% B. One or more of the principal components in form of a seismic data set.
%    The first trace is the first requested principal component, etc.
%    {'output','pc'}
% C. The coefficients of the principal components for each trace
%    {'output','coefficients'}
%
% Written by: E. Rietsch: July 18, 2000
% Last updated: September 20, 2009: General streamlining
%
%             [pc,aux]=s_principal_components(seismic,varargin)
% INPUT
% seismic   seismic data set
% varargin   one or more cell arrays; the first element of each cell array is 
%           a keyword, the other elements are parameters. Presently, keywords are:
%      'output' Type of output. Possible values are: 
%           'pc' (principal components), 'seismic', 'coefficients'. 
%           In the first case one trace is output for each principal 
%           component requested; headers which are constant are output as well.
%           In the second case the number of output traces equals the number
%           of input traces and all headers are copied.
%           In the third case the coefficients of the principal components for 
%           each trace are output. Again the number of output traces is the
%           same as the number of input traces.
%           Default: {'output','seismic'}
%      'index' Index/indices of the principal components requested. 
%           Possible values are integers > 0.
%           The maximum value, "maximum_value", is the smaller of the number of
%           samples per trace and the number of traces. A request for principal
%           components with higher index is ignored. 
%           Default: if {'output','seismic'} then {'index',1}
%                    if {'output','pc'}      then {'index',1:maximum_value}
% OUTPUT
% pc        seismic structure; 
%           if {'output','seismic'} it is the seismic input dataset 
%           represented by the principal components requested via keyword
%           'index'.
%           if {'output','pc'} it is set of principal components requested
%           via keyword 'index'.
% aux       structure with auxiliary information
%           if output is "seismic'
%    'energy' row vector: scaled energy of the principal components of 
%           each trace; these numbers represent the fraction of the total
%           trace energy represented by that specific combination of
%           principal components.
%    'd'    row vector: cumulative sum of the squared singular values 
%           (scaled so that the last entry is 1). It is the fraction
%           of the total energy of the seismic dataset represented
%           by the principal components as the number of principal 
%           components used is increased.
%
%           if output is "pc"
%    'sing_values'  singular values associated with the requested principal
%           components (normalized so that the sum of the squares of all
%           singular values is 1)
%
% EXAMPLES
%      seismic=s_data;
%
%      [pc,aux]=s_principal_components(seismic);
%      disp(aux)
%
%      s_wplot(pc)
%      mytitle('First principal component of each trace (correctly scaled)')
%
%      %        Consistency test
%      comp=s_principal_components(seismic,{'output','pc'});
%      coeff=s_principal_components(seismic,{'output','coefficients'});
%
%      test=s_convert(comp.traces*coeff.traces,0,seismic.step);
%
%      s_compare(seismic,test);
%      mytitle('Comparison of original traces (black) and reconstituted traces (red)')

% UPDATE HISTORY
%      October 17, 2006: "components" option of keyword "output" redone;
%                         no polarity change for principal components
%                                 

if ~istype(seismic,'seismic')
   error(' First input argument must be a seismic dataset.')
end

if isnull(seismic)
   error(' Seismic data must not contain NaNs')
end

pc=seismic;

[nsamp,ntr]=size(seismic.traces);
min_nsamp_ntr=min([nsamp,ntr]);

%%       Set default values of optional input arguments
param.output='seismic';
param.index=[];

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

%       Set/check number of principal components requested.
if isempty(param.index)
   if strcmpi(param.output,'seismic')
      param.index=1;
   else
      param.index=1:min_nsamp_ntr;
   end
else
   param.index=param.index(param.index <= min_nsamp_ntr);
end


%%      Switch to the requested output option
switch param.output

case 'seismic'
   [pr_cmp,energy,d]=princ_comp_no1(seismic.traces,param.index);
   pc.traces=pr_cmp;

%       Add history field if it exists in seismic
   aux.energy=energy;
   aux.d=d';
   pc=ds_header(pc,'add_ne','energy',energy,'n/a','Fraction of total trace energy retained');

 
case 'pc'
   [pc.traces,sing_val]=princ_comp_no2(seismic.traces,param.index);

   pc.name='Principal components';

%   	Copy headers that are constant and delete others
   if isfield(seismic,'header_info')
      nh=size(seismic.headers,1);
      headers=zeros(nh,size(pc.traces,2));
      header_info=cell(nh,3);
      icount=0;
      for ii=1:nh
         if min(seismic.headers(ii,:)) == max(seismic.headers(ii,:))
            icount=icount+1;
            headers(icount,:)=seismic.headers(ii,1);
            header_info(icount,:)=seismic.header_info(ii,:);
         end
      end
      if icount > 0
         pc.headers=headers(1:icount,:);
         pc.header_info=header_info(1:icount,:);
      else
         pc=rmfield(pc,{'headers','header_info'});
      end
   end      

   if nargout > 1
      aux.sing_values=sing_val;
   end

case 'coefficients'
   [pc.traces,d]=princ_comp_no3(seismic.traces,param.index);
   if nargout > 1
      aux.sing_values=d;
   end
   pc.first=1;
   pc.last=length(param.index);
   pc.step=1;
   pc.units='Index';
   pc.tag='principal_components';
   pc.row_label=param.index(:);
   pc.name='Coefficients of the principal components';
%   aux.d=d

otherwise
   error([' Unknowm option for keyword "output": ',param.output])
		
end		% End of switch block


%	Add entry to the history field if it exists in seismic
htext=[' Principal components: ',num2str(param.index)];
pc=s_history(pc,'append',htext); 


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [pc,energy,dd]=princ_comp_no1(s,npc)
% Function computes principal components of matrix s over the range npc and 
% outputs them together with the total relative energy in each column of s.
%
%       [pc,energy,d]=princ_comp_no1(s,npc)
% INPUT
% s     input matrix
% npc   row vector with the indices of the principal components to use
%       max(npc) <= number of columns of s
% OUTPUT
% pc    principal components
% energy  fraction of total trace energy

[ns,ntr]=size(s);

%       Perform singular-value decomposition
if ns >= ntr
  [u,d,v]=svd(s,0);
else
  [v,d,u]=svd(s',0);
end

%       Store singular values in a vector and compute normalization factor
%       so that the sum of their squares is 1
d=diag(d);
if nargout > 2; 
   dd=d.^2;
   scf=1/sum(dd);
   dd=cumsum(dd)*scf; 
else
   scf=1/sum(d.^2);
end

%       Compute the coefficients of the requested principal components for 
%       each column of the matrix
ik=0;
vv=zeros(size(v,1),length(npc));
for ii=npc
   ik=ik+1;
   vv(:,ik)=v(:,ii)*d(ii);
end

%       Select the requested principal components
pc=u(:,npc)*vv';
if ik == 1
   energy=reshape((vv.^2)*scf,1,[]);
else
   energy=sum(vv.^2,2)'*scf;
end

%       Normalize energy on a trace-by-trace basis
trace_energy=sum(s.^2);
scf1=sum(trace_energy)./trace_energy;
energy=energy.*scf1;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [pc,sing_val]=princ_comp_no2(s,npc)
% Function computes principal components of s over the range of indices npc 
% and outputs them together with the total relative energy in each column of s.
%
%           [pc,sing_val]=princ_comp1(s,npc)
% INPUT
% s         input array
% npc       indices of principal components to use
% OUTPUT
% pc        principal components
% sing_val  normalized singular values (sum of squares = 1)

[u,dd]=svd(s,0);
d=diag(dd);
pc=u(:,npc);
sing_val=d(npc)./sqrt(sum(d.^2));


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [vv,dd]=princ_comp_no3(s,npc)
% Function computes coefficients of the principal components of s over
% the range of indices npc and outputs them together with the total 
% relative energy in each column of s.
%
%           [pc,sing_val]=princ_comp1(s,npc)
% INPUT
% s         input array
% npc       indices of principal components to use
% OUTPUT
% vv        Matrix with coefficients of the principal components for each column of s
%           there are as many columns as there are columns of "s"
% d         all singular values 

[dummy,d,v]=svd(s,0);  %#ok 

dd=diag(d)';
vv=zeros(length(npc),size(s,2));
ik=0;
for ii=npc
   ik=ik+1;
   vv(ik,:)=v(:,ii)'*dd(ii);
end
