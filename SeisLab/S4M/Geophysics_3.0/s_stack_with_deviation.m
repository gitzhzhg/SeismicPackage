function [stack,deviation,aux]=s_stack_with_deviation(seismic,varargin)
% Compute stack and the deviation of each input trace from the stack. 
% Thus "deviation" has as many traces as "seismic"; each trace is the 
% difference between the corresponding trace of seismic and the stacked 
% trace associated with it.
% A scaling option allows one to compute the difference 
%    "seismic.trace - a*stack.trace"
% where "a" is a scale factor chosen internally to make the difference small in the 
% least-squares sense.
%
% Written by: E. Rietsch: April 3, 2006
% Last updated:
%
%           [stack,deviation,aux]=s_stack_with_deviation(seismic,varargin)
% INPUT
% seismic   seismic dataset (for meaningful results it should have more 
%           than one trace)
% varargin  one or more cell arrays; the first element of each cell array is a 
%           keyword, the other elements are parameters. Presently, keywords are:
%    'header'  header mnemonic. Traces with the same header mnemonic
%              are stacked. 
%              Default: {'header',''}; i.e. no header mnemonic selected:
%                                      all traces of "seismic" are stacked.
%    'scaling' possible values are 'yes' and 'no';
%              if scaling is 'yes' a scaling factor is computed and applied 
%              to the stack trace prior to subtraction. This reduced the 
%              traces of "deviation" in particular if trace amplitude is a
%              a major source of the difference between a seismic trace and 
%              the associated stack.
%              Default: {'scaling','no'}
% OUTPUT
% stack    seismic structure with the stacked data
%          headers are averaged as well
% aux      structure with additional data
%    'multi'    seismic structure with the same number of traces as "stack". 
%          Each trace sample represents the number of samples of "seismic" 
%          that were used to form the corresponding sample of "stack".
%    'stdeviation' standard deviation of seismic traces from associated 
%          stack (i.e. of "seismic.trace - a*stack.trace")

%       Set defaults for input parameters
param.header='';
param.scaling='no';

%       Replace input arguments by actual ones
param=assign_input(param,varargin); 


[stack,aux]=s_stack(seismic,{'header',param.header});

deviation=seismic;

switch param.scaling

   case 'no'
      [deviation.traces,stdev]=deviation_no1(seismic,stack,param);
   case 'yes'
      [deviation.traces,stdev]=deviation_no2(seismic,stack,param);
   otherwise
      error(['Unknown scaling option: ',param.scaling])
end
if nargout > 2
   stdeviation=stack;
   stdeviation.traces=stdev;
   aux.stdeviation=stdeviation;
end

%       Create history
deviation=s_history(deviation,'append');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [traces,stdev]=deviation_no1(seismic,stack,param)
% Compute deviation without scaling of stack trace(s)

traces=zeros(size(seismic.traces));

if ~isempty(param.header)
   uh=s_gh(stack,param.header);
   headers=s_gh(seismic,param.header);
   stdev=zeros(size(stack.traces));

   for ii=1:length(uh)
      index=find(headers==uh(ii));
      for jj=1:length(index)
         traces(:,index(jj))=seismic.traces(:,index(jj))-stack.traces(:,ii);
      end
      stdev(:,ii)=std(traces(:,index),1,2);
   end
   
else
   for ii=1:size(seismic.traces,2)
      traces(:,ii)=seismic.traces(:,ii)-stack.traces;
   end
   stdev=std(traces,1,2);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [traces,stdev]=deviation_no2(seismic,stack,param)
% Compute deviation with scaling of stack trace(s)

traces=zeros(size(seismic.traces));
stdev=zeros(size(seismic.traces));
if ~isempty(param.header)
   uh=s_gh(stack,param.header);
   headers=s_gh(seismic,param.header);

   for ii=1:length(uh)
      index=find(headers==uh(ii));
      for jj=1:length(index)
         temp=seismic.traces(:,index(jj));
         bool=~isnan(temp);
         a=(temp(bool)'*stack.traces(bool,ii))/norm(stack.traces(bool,ii))^2;
         traces(:,index(jj))=seismic.traces(:,index(jj))-a*stack.traces(:,ii);
      end
      stdev(:,ii)=std(traces(:,index),1,2);
   end
   
else
   for ii=1:size(seismic.traces,2)
      temp=seismic.traces(:,ii);
      bool=~isnan(temp);
      a=(temp(bool)'*stack.traces(bool))/norm(stack.traces(bool))^2;
      traces(:,ii)=seismic.traces(:,ii)-a*stack.traces;
   end
   stdev=std(traces,1,2);
end
