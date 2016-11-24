function seismic=s_convolve(seisin1,seisin2,varargin)
% Function convolves two seismic data sets
% If one of the data sets has only one trace then it is convolved with all 
% traces of the other data set.
% The headers of the dataset with more traces are retained.
% If the datasets have the same number of traces, corresponding traces are
% convolved, and the headers of the first data set are retained.
%
% Written by: E. Rietsch: April 2000
% Last updated: April 2, 2008: Check for trace nulls in the output dataset
%
%              seismic=s_convolve(seisin1,seisin2,varargin)
% INPUT
% seisin1     first seismic input dataset
% seisin2     second seismic input dataset
% varargin    one or more cell arrays; the first element of each cell array is a keyword,
%             the other elements are parameters. Presently, keywords are:
%             Options not yet implemented 
% OUTPUT
% seismic     output dataset; convolution of the two input datasets

% UPDATE HISTORY
%             January 22, 2007: Preserve precision of input arguments


if ~istype(seisin1,'seismic')  ||  ~istype(seisin2,'seismic')
   error('The first two input arguments must be seismic datasets.')
end

[nsamp1,ntr1]=size(seisin1.traces);
[nsamp2,ntr2]=size(seisin2.traces);

%	Set defaults for input arguments
if ntr1 >= ntr2
   param.header=1;
else
   param.header=2;
end
param.type='corresponding';

%	Replace defaults by actual input arguments (if there are any)
param=assign_input(param,varargin); 


%       Error checking

if isnull(seisin1)
   disp('The first seismic input dataset has one or more traces with null values.')
   disp('This may create a large number of null values in the output data set.')
   warning(warnid,'If this is not intended remove them with "s_rm_trace_nulls".')
end

if isnull(seisin2)
   disp('The second seismic input dataset has one or more traces with null values.')
   disp('This may create a large number of null values in the output data set.')
   warning(warnid,'If this is not intended remove them with "s_rm_trace_nulls".')
end


seismic.first=seisin1.first+seisin2.first;
seismic.last=seisin1.last+seisin2.last;

if seisin1.step == seisin2.step
   seismic.step=seisin1.step;
else
   error([' The two input data sets have different sample intervals (', ...
         num2str(seisin1.step),' vs. ',num2str(seisin2.step),')'])
end

%	Handle precision requirement
if strcmp(class(seisin1.traces),'single')  ||  strcmp(class(seisin2.traces),'single') 
   seismic.traces=zeros(nsamp1+nsamp2-1,max([ntr1,ntr2]),'single');
else
   seismic.traces=zeros(nsamp1+nsamp2-1,max([ntr1,ntr2]));
end

if ntr1 == 1
   for ii=1:ntr2
      seismic.traces(:,ii)=conv(seisin1.traces,seisin2.traces(:,ii));
   end

elseif ntr2 == 1
   for ii=1:ntr1
      seismic.traces(:,ii)=conv(seisin2.traces,seisin1.traces(:,ii));
   end
   
elseif ntr1 == ntr2 
   if strcmpi(param.type,'corresponding')
      for ii=1:ntr1
         seismic.traces(:,ii)=conv(seisin2.traces(:,ii),seisin1.traces(:,ii));
      end
   end

else
   error('This option has not yet been implemented')
end

%    Append history field and copy rest of fields

if param.header == 1

%       Copy rest of fields
   seismic=copy_fields(seisin1,seismic);

   if isfield(seisin1,'history')  &&  isfield(seisin2,'history')
      seismic=s_history(seismic,'append',' ');
      seismic=s_history(seismic,'merge',seisin2.history);
   end

else

%       Copy rest of fields
   seismic=copy_fields(seisin2,seismic);
   if isfield(seisin1,'history')  &&  isfield(seisin2,'history')
      seismic=s_history(seismic,'append',' ');
      seismic=s_history(seismic,'merge',seisin1.history);
   end

end

%     Check for null values and set field "null" accordingly
if any(isnan(seismic.traces(:)))
   seismic.null=NaN;
else
   seismic.null=[];
end

    