function [tf,msg]=isvalid(x,varargin)
%ISVALID Variable Validity.
% TF = ISVALID(X,'PName1',PValue1,...) tests the variable X using the
% property name and value pairs 'PName1',PValue1, etc., and returns logical
% True if ALL specifications are met. Otherwise logical False is returned.
% [TF,MSG] = ISVALID(X,'PName1',PValue1,...) in addition returns an
% appropriate error message identifying the first property violated.
%
% PROPERTY     VALUE
% Type         Real | Complex | Single | Double | Numeric | Logical |
%                 Flint (floating point integer) | Integer | Character |
%                 Cell | Stuct
% NumEl        scalar required for numel(X)
% Length       scalar required for length(X)
% NDims        scalar required for ndims(X)
% Size         row vector required for size(X); Use NaN for don't care
%                 elements.
% Range        [Xmin Xmax] two element vector containing the inclusive
%                 lower and upper limits for all elements of X. Applies to 
%                 numeric inputs only.
%
% Examples:
% ISVALID(ones(3),'type','real','size',[3 nan]) returns True because input
% is real and is 3-by-anything
%
% ISVALID(-5:10,'type','flint','range',[0 inf]) returns False because,
% while the input contains floating point integers, some inputs values are
% outside the range [0 inf].

% D.C. Hanselman, University of Maine, Orono, ME 04469
% MasteringMatlab@yahoo.com
% Mastering MATLAB 7
% 2006-01-12

if rem(nargin,2)~=1
   error('Property Names and Values Must Appear in Pairs.')
end
for k=1:2:nargin-2 % go through PName,PValue pairs
   PName=varargin{k}(1:min(length(varargin{k}),2));
   if ~ischar(PName)
      error('Character String Property Name Expected.')
   end
   PValue=varargin{k+1};
   switch PName
   case 'ty'                                               % PName = 'Type'
      if ~ischar(PValue)
         error('Property Value Character String Expected.')
      end
      switch lower(PValue(1:min(length(PValue),2)))
      case 're'
         tf=isreal(x);
         msg='Real Values Expected.';
      case 'co'
         tf=~isreal(x);
         msg='Complex Values Expected.';
      case 'si'
         tf=isa(x,'single');
         msg='Single Values Expected.';
      case 'do'
         tf=isa(x,'double');
         msg='Double Values Expected.';
      case 'nu'
         tf=isnumeric(x);
         msg='Numeric Values Expected.';
      case 'lo'
         tf=islogical(x);
         msg='Logical Values Expected.';
      case 'ch'
         tf=ischar(x);
         msg='Character String Expected.';
      case 'ce'
         tf=iscell(x);
         msg='Cell Array Expected.';
      case 'st'
         tf=isstruct(x);
         msg='Structure Expected.';
      case 'in'
         tf=isinteger(x);
         msg='Integer Values Expected.';
      case 'fl'
         tf=isequal(x,fix(x));
         msg='Floating Point Values Expected.';
      otherwise
         error('Unknown Property Value for ''Type'' Property.')
      end
   case 'nu'                                              % PName = 'NumEl'
      tf=numel(x)==PValue(1);
      msg='Incorrect Number of Elements.';
   case 'le'                                             % PName = 'Length'
      tf=length(x)==PValue(1);
      msg='Incorrect Length.';
   case 'nd'                                              % PName = 'NDims'
      tf=ndims(x)==PValue(1);
      msg='Incorrect Number of Dimensions.';
   case 'si'                                               % PName = 'Size'
      sx=size(x);
      if length(sx)==length(PValue) % same number of dims
         nn=~isnan(PValue);
         tf=isequal(sx(nn),PValue(nn));
      else
         tf=false; % false since dims are not equal
      end
      msg='Incorrect Size.';
   case 'ra'                                              % PName = 'Range'
      if ~isnumeric(x)
         error('Numeric Variable Required for ''Range'' Property.')
      else
         tf=all(x>=min(PValue) & x<=max(PValue));
         msg='Values are Outside Range.';
      end
   otherwise
      error(['Unknown Property Name: ' varargin{k}])
   end
   if ~tf
      return
   end
end
tf=true; % default outputs if all tests are true
msg='';     

