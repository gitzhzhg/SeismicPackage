function ds=uoperation(ds,operator)
% Function performs a unary operation on a dataset
%
% Written by: E. Rietsch: December 7, 2006
% Last updated:
%
%        ds=uoperation(i1,operator)
% INPUT
% i1     operand 
%        At least one of the two operands is a dataset.
% operator   string defining operation to perform. Possible values are:
%      '+', '-'
% OUTPUT
% ds    result of the operation

if ~isfield(ds,'type')
   error(['Operation "',operator,'" is not defined for this structure.'])
end

switch ds.type
case 'seismic'
   ds=uoperation4seismic(ds,operator);

case 'pdf'
   ds=uoperation4pdf(ds,operator);

otherwise
   error(['The operator "',operator,'" is not defined for this structure.'])

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ds=uoperation4seismic(ds,operator)
% Perform the operation defined by input argument "operator" to the 
% input argument
%
% Written by: E. Rietsch: July 12, 2006
% Last updated: 
%
%       ds=uoperation4pdf(ds,operator)
% INPUT
% ds    seismic dataset 
% operation  string defining operation to perform. Possible values are:
%      '+', '-'
% OUTPUT
% ds    result of the operation


switch operator
case '+'
     % Do nothing

case '-'
    ds.traces=-ds.traces;
   
otherwise
    disp([' Unknown operator "',operator,'".'])
    error(' Abnormal termination.')
	 
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ds=uoperation4pdf(ds,operator)
% Perform the operation defined by input argument "operator" to the 
% input argument
%
% Written by: E. Rietsch: July 12, 2006
% Last updated: September 16, 2006: Add handling of PDF's
%
%       ds=uoperation4pdf(i1,i2,operator)
% INPUT
% ds    PDF
% operation  string defining operation to perform. Possible values are:
%      '+', '-',
% OUTPUT
% ds    result of the operation


switch operator
case '+'
     % Do nothing

case '-'
    ds.pdf=-ds.pdf;
    try
       ds.cdf=-ds.cdf;
    catch
       % Do nothing
    end

otherwise
    disp([' Unknown operator "',operator,'".'])
    error(' Abnormal termination.')
	 
end
