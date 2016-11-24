function [nrows,ncols]=dsSize(dataset,dim)
% Find the number of rows and/or columns of the data matrix for the 
% following datasets:
%    "pseudo-wells", "tables", "seismic"
%
% Written by: E. Rietsch: July 26, 2006
% Last updated:
%
%          [nrows,ncols]=dsSize(dataset,dim)
% INPUT
% dataset  dataset name
% dim      optional dimension for which to compute length (like size(a,2))
% OUTPUT
% nrows    number of rows
% ncols    number of columns


%	Check input
try
   typ=dataset.type;
catch
   error('Dataset must be a structure with a field "type".')
end

if nargout == 2  &&  nargin == 2
   error('There can only one input argument if two output arguments are requested.')
end


if nargin == 1
   switch typ

   case 'pseudo-wells'
      [nrows,ncols]=panelsize(dataset);
       
   case 'table'
      [nrows,ncols]=tablesize(dataset);
    
   case 'seismic'
      [nrows,ncols]=size(dataset.traces);
    
   otherwise
      error(['Unsupported dataset type: ',typ])
   end

elseif nargin == 2
   switch typ

   case 'pseudo-wells'
      nrows=panelsize(dataset,dim);

   case 'table'
      nrows=tablesize(dataset,dim);
    
   case 'seismic'
      nrows=size(dataset.traces,dim);

   otherwise
      error(['Unknown dataset type: ',typ])
   
   end
   

else
   error('Wrong number of input arguments.')

end