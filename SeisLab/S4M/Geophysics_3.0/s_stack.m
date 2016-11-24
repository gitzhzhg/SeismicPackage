function [stack,aux]=s_stack(seismic,varargin)
% Function stacks seismic traces (sums the traces of gathers and divides 
% the sum by the number of valid samples added; normalization). 
% It a header mnemonic is specified, traces with the same value of 
% that header (gathers) are stacked; otherwise all traces of the input 
% data set are stacked. Also, a header can be specified 
% which contains weights that are to be applied to the traces prior to
% stacking. In this case the stacked traces are not divided by the 
% number of traces stacked
%
% Written by: E. Rietsch: June 20, 2001
% Last updated: September 3, 2006: Allow scaling the traces prior to stacking
%
%           [stack,aux]=s_stack(seismic,varargin)
% INPUT
% seismic   seismic structure
% varargin  one or more cell arrays; the first element of each cell array is a 
%           keyword, the other elements are parameters. Presently, keywords are:
%           'header'  header mnemonic. Traces with the same header mnemonic
%                  are stacked. 
%                  Default: {'header',''}; i.e. no header mnemonic selected:
%                                          all traces of "seismic" are stacked.
%           'weight'  mnemonic of header which contains weights to be
%                  applied to traces prior to stacking
%                  Default: {'weight',[]}  i.e. all traces have equal weight 1
% OUTPUT
% stack    seismic structure with the stacked data
%          headers are averaged as well
% aux      structure with additional data
%    'multiplicity'  seismic structure with the same number of traces as "stack". 
%          Each trace sample represents the number of samples of "seismic" 
%          that were used to form the corresponding sample of "stack".
%
% EXAMPLE
%          seismic=s_data;
%          seismic.traces(20:30,1:3)=NaN;
%          seismic.null=NaN;
%          seismic=ds_header(seismic,'add_ne', ...
%                  'weight',1:size(seismic.traces,2),'n/a','Weights');
%
%          [stack,aux]=s_stack(seismic);
%          s_wplot(aux.multiplicity,{'interpol','linear'},{'title','Multiplicity'})
%
%          [wstack,aux]=s_stack(seismic,{'weight','weight'})
%          s_wplot(aux.multiplicity,{'interpol','linear'},{'title','Weighted multiplicity'})


         
%       Set defaults for input parameters
param.header='';
param.weight='';

%       Decode and assign input arguments
param=assign_input(param,varargin); 

if isnull(seismic)
   no_null=false;
else
   no_null=true;
end

if ~isempty(param.weight)
   weights=s_gh(seismic,param.weight).';
end

%	Case of no header specified (all traces are stacked together)
if isempty(param.header)
   htext='Stack';
   if isfield(seismic,'headers')
      stack.headers=mean(seismic.headers,2);
   end
   if nargout == 1 	% Multiplicity not requested
      if isempty(param.weight)
         [stack.traces,no_null]=normal_stack(seismic.traces,no_null);
      else
         [stack.traces,no_null]=weighted_stack(seismic.traces,no_null,weights);
      end
   else			% Multiplicity requested
     if isempty(param.weight)      
        [stack.traces,no_null,multi.traces]=normal_stack(seismic.traces,no_null);
     else
        [stack.traces,no_null,multi.traces]=weighted_stack(seismic.traces,no_null,weights);
     end

%       Copy rest of fields
      if isfield(stack,'headers')
         multi.headers=stack.headers;
      end
      multi=copy_fields(seismic,multi);
   end
  
   stack=copy_fields(seismic,stack);
   if ~no_null
      stack.null=NaN;
   end

else		% Header specified
   htext='Stack';
   no_null_out=1;
   header=s_gh(seismic,param.header);
   uh=unique(header);
   ntr=length(uh);
   stack.traces=zeros(size(seismic.traces,1),ntr);
   stack.headers=zeros(size(seismic.headers,1),ntr);

   if nargout == 1 	% Auxiliary data (second output dataset) not requested
      for ii=1:ntr
         index=find(ismember(header,uh(ii)));
	 if isempty(param.weight)
            [stack.traces(:,ii),temp]=normal_stack(seismic.traces(:,index),no_null);
	 else
            [stack.traces(:,ii),temp]=weighted_stack(seismic.traces(:,index),no_null,weights);
	 end
         stack.headers(:,ii)=mean(seismic.headers(:,index),2);
         no_null_out=no_null_out*temp;
      end
   
   else			% Auxiliary data requested
      multi.curves=zeros(size(stack.traces));
      for ii=1:ntr
         index=find(ismember(header,uh(ii)));
         if isempty(param.weight)	    
            [stack.traces(:,ii),temp,multi.traces(:,ii)]= ...
                 normal_stack(seismic.traces(:,index),no_null);
         else
            [stack.traces(:,ii),temp,multi.traces(:,ii)]= ...
                 weighted_stack(seismic.traces(:,index),no_null,weights(index));
         end
         stack.headers(:,ii)=mean(seismic.headers(:,index),2);
         no_null_out=no_null_out*temp;
      end
      multi.headers=stack.headers;
      multi=copy_fields(seismic,multi);
   end

   stack=copy_fields(seismic,stack);
   if ~no_null_out
      stack.null=NaN;
   end
end
   
%      Append history field
if isfield(seismic,'history')
   stack=s_history(stack,'append',htext);
end 

if nargout > 1
   aux.multiplicity=s_history(multi,'append','Multiplicity');
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [stack,no_null,multipl]=normal_stack(traces,no_null)
% Function computes mean of elements of array traces along rows
% INPUT
% traces      matrix of seismic traces to be stacked
% no_null     logical variable (true (1) if "traces" contains no nulls)
% OUTPUT
% stack       vector of stacked traces
% no_null     logical variable (true (1) if "traces" contains no nulls)
% multipl     multiplicity (number of samples averaged); unless NaNs are
%             present, this is the number of columns of "traces".

[nsamp,ntr]=size(traces);

stack=mean(traces,2);
if nargout > 2
   multipl=ntr*ones(nsamp,1);
end

if no_null
   return
end

% 	Check for NaNs
index=find(isnan(stack));
if isempty(index)
   no_null=true;
   return
end

%	Select rows (times) with NaNs
temp=traces(index,:);
logindex=isnan(temp);
mult=ntr-sum(logindex,2);

%	Replace NaNs by zeros and stack
temp(logindex)=0;
temp=sum(temp,2);

index1=find(mult == 0);
if isempty(index1)
   stack(index)=temp./mult;
   no_null=true;
else
   stack(index)=temp./max(mult,eps);
   stack(index(index1))=NaN;
end

if nargout > 2
   multipl(index)=mult;
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [stack,no_null,multipl]=weighted_stack(traces,no_null,weights)
% Function computes mean of elements of array traces along rows
% INPUT
% traces      matrix of seismic traces to be stacked
% no_null     logical variable (true (1) if "traces" contains no nulls)
% OUTPUT
% stack       vector of stacked traces
% no_null     logical variable (true (1) if "stack" contains no nulls)
% multipl     multiplicity (number of samples averaged); unless NaNs are
%             present, this is the number of columns of "traces".

nsamp=size(traces,1);

stack=traces*weights(:);
if nargout > 2
   multipl=sum(weights)*ones(nsamp,1);
end

if no_null
   return
end

no_null=true;

% 	Check for NaNs
index=find(isnan(stack));
if isempty(index)
   return
end

%	Select rows (times) with NaNs
temp=traces(index,:);
logindex=~isnan(temp);
for ii=1:length(index)
   bool=logindex(ii,:);
   if any(bool)
      stack(index(ii))=temp(ii,bool)*weights(bool);
      multipl(index(ii))=sum(weights(bool));
   else
      stack(index(ii))=NaN;
      multipl(index(ii))=NaN;
      no_null=false;
   end
end
