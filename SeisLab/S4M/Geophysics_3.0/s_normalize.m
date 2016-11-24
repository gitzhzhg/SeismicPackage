function seismic=s_normalize(seismic,varargin)
% Function normalizes seismic traces
%
% Written by: E. Rietsch: December 5, 2004
% Last updated: February 1, 2006: fix bug
%
%             seismic=s_normalize(seismic,varargin)
% INPUT
% seismic     seismic data set
% varargin    one or more cell arrays; the first element of each cell array is a keyword,
%             the other elements are parameters. Presently, keywords are:
%     'attribute'   type of attribute to make equal for all the traces
%             Possible values are: 'power', 'rms'
%             Default: {'attribute','power'}
%     'value' value to which to set attribute; possible values are the string 'average' 
%             (which sets the attribute to the average of all attributes) or a numeric value
%             Default: {'value','average'}
% OUTPUT
% seismic    input data with all traces having the same attribute.


%	Set default values for input parameters
param.attribute='power';
param.value='average';

%	Replace defaults by input parameters
param=assign_input(param,varargin);

switch lower(param.attribute)


		case 'power'
temp=mean(seismic.traces.^2);
if ischar(param.value)
   scf=sqrt(mean(temp)./temp);
else
   try
      scf=sqrt(param.value./temp);
   catch
      error('Check attribute value and/or data.')
   end
end
    

	case 'rms'

temp=sqrt(mean(seismic.traces.^2));
if ischar(param.value)
   scf=mean(temp)./temp;
else
   try
      scf=param.value./temp;
   catch
      error('Check attribute value and/or data.')
   end
end
		otherwise
error('Unknown option');

end


seismic.traces=mvt(seismic.traces,scf);
