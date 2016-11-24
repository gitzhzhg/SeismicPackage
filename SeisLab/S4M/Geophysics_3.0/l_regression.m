function [wlog,aux]=l_regression(wlog,expression,varargin)
% Function creates new log curve using the mathematical expression contained in the
% third input argument. Null values (NaNs) in the log curves are ignored when computing
% the regression parameters. Requires the Optimization Toolbox
%
% Written by E. Rietsch, March 16, 2001
% Last updated: July 8, 2006: Use dynamic field names; updated
%
%             [wlog,aux]=l_regression(wlog,expression,varargin)
% INPUT
% wlog        log structure
% expression  expression in MATLAB syntax between curves of wlog. These curves are  
%             represented by their mnemonics. No default.
%             EXAMPLE:  'rho = x1*Vp^x2' which estimates a Gardner-type relationship
%                       for the density
%             The curve to the left of the equal sign is called the target curve 
%             ('rho' in this example)
%             The parameters to be estimated are represented by "x1","x2",...,"x9".
% varargin    one or more cell arrays; the first element of each cell array is a
%             keyword, the other elements are parameters. Presently, keywords are:
%      'depths'  start and end depth of curves used for regression calculation
%             Default: {'depths',wlog.first,wlog.last}
%      'rows' string with logical expression involving one more of the curve mnemonics
%             Can be used to restrict curve values used for regression to certain lithologies
%             Default: {'rows',''} (implies all rows)
%      'lbounds'  lower limits for the values of the parameters to be 
%             estimated.
%             No defaults.
%      'ubounds'  upper limits for the values of the parameters to be 
%             estimated.
%             No defaults.
%      'description'   description of the newly created curve
%      'x'    initial values of the parameters
%             Default: {'x',0.5*(ubounds-lbounds)}
%             Error message if any of the bounds are infinite.
%      'mnem' mnemonic of the estimated curve.
%             Default: {'mnem','curve_pred'} where "curve" is the mnmonic of the 
%                      target curve in "expression"; in the example above it 
%                      would be 'rho_pred')
%      'norm' Norm to use; possible values are 'L1' (minimizes the sum of the
%             absolute values of the error; more robust) or 'L2' (sum of the 
%             squares of the error; faster). If norm is 'L1', two additional parameters
%             may be given which allow treating positve deflections different from 
%             negative ones. Not case-sensitive.
%             Default: {'norm','L1')
% OUTPUT
% wlog        input log with the additional curve appended (including an updated field 
%             "curve_info")
% aux         structure with auxiliary information. The following fields are present:
%             x1, x2, ... estimated parameters;
%             x       estimated paramters as a row vector
%             fval    final function value (error)
%             exitflag of the minimization routine
%             general info from minimization routine (important only for debugging)
%
% EXAMPLES
%             wlog=l_data;
%
%             %		Density from velocity
%             [wlog,aux]=l_regression(wlog,'rho=x1*vp^x2');
%             l_plot1(wlog,{'curves','rho','rho_pred'})
%
%             %         Depth trend of density
%             [wlog,aux]=l_regression(wlog,'rho=x1*depth^x2',{'mnem','rho_trend'});
%             l_plot1(wlog,{'curves','rho','rho_trend'})

%       Set defaults of input arguments
param.depths=[];
param.rows=[];
param.lbounds=[];
param.ubounds=[];
% param.description='';
param.x=[];
param.mnem=[];
param.norm='l1';

%       Decode and assign input arguments
param=assign_input(param,varargin);

%       Find all the words in the expression
words=extract_words(expression);

%       Check if mnemonic of output curve (first variable in expression) exists
mnem=words{1};
[index,ier]=curve_index1(wlog,mnem,0);
if ier
   if isempty(index)
      disp([char(13),' No curve with mnemonic "',mnem,'" found in log structure'])
   else
      disp([char(13),' More than one curve with mnemonic "',mnem,'" found in log structure'])
   end
   error(' Abnormal termination')
end

%       Set default name for target curve (if not defined in argument list)
if isempty(param.mnem)
   param.mnem=[mnem,'_pred'];
end

nsamp=size(wlog.curves,1);
% words=extract_words(expression);
vars=symvar(expression);

%       Find all the parameters
x1to9={'x1','x2','x3','x4','x5','x6','x7','x8','x9'};
lindex=ismember(vars,x1to9);
xi=vars(lindex);
lxi=length(xi);

%       Check initial values for parameters
if ~isempty(param.x)
   if length(param.x) ~= lxi
      disp([char(13),' Discrepancy: ',num2str(lxi),' parameters found in expression, but ', ...
             num2str(length(param.x)),' initial value(s) found'])
      error(' Abnormal termination')
   end
   if iscell(param.x)
      param.x=cat(2,param.x{:});
   end
end

%       Check bounds of parameters
if isempty(param.lbounds)
   param.lbounds=-inf*ones(1,lxi);
else
   if length(param.lbounds) ~= lxi
      disp([char(13),' Number of lower bounds (',num2str(length(param.lbounds)), ...
                ') not equal to number of parameters in expression (', ...
               num2str(lxi),')'])
      disp(' Check number of values entered via keyword "lbounds"')
      error(' Abnormal ternination')
   end
   if iscell(param.lbounds)
      param.lbounds=cat(2,param.lbounds{:});
   end
end

if isempty(param.ubounds)
   param.ubounds=inf*ones(1,lxi);
else
   if length(param.ubounds) ~= lxi
      disp([char(13),' Number of upper bounds (',num2str(length(param.ubounds)), ...
                ') not equal to number of parameters in expression (', ...
               num2str(lxi),')'])
      disp(' Check number of values entered via keyword "ubounds"')
      error(' Abnormal ternination')
   end
   if iscell(param.ubounds)
      param.ubounds=cat(2,param.ubounds{:});
   end
end

if isempty(param.x)
   param.x=0.5*(param.ubounds+param.lbounds);
   if any(isinf(param.x))
      error('There is no default for the starting values if any of the bounds are infinite.')
   end

   idx=find(isnan(param.x));
   if ~isempty(idx)     
      for ii=1:length(idx)
         if param.lbounds(idx(ii)) > -inf 
            param.x(idx(ii))=param.lbounds(idx(ii)) + 1;
         elseif param.ubounds(idx(ii)) < inf 
            param.x(idx(ii))=param.ubounds(idx(ii)) - 1;
         else
            param.x(idx(ii))=0;
         end
      end
   end
end

%       Check if the initial values of the parameters are within the bounds
%temp=find(param.x < param.lbounds);
ier=0;
if any(param.x < param.lbounds)
   ier=1;
   alert(' One or more initial parameter values are below the lower bounds.')
end
%temp=find(param.x > param.ubounds);
if any(param.x > param.ubounds)
   ier=1;
   alert(' One or more initial parameter values are above the upper bounds.')
end
if ier
   disp(' Initial parameter values:')
   disp(param.x)
   error(' Abnormal termination')
end

%       Find all the curves ...
curve_names=vars(~lindex);
lp=length(curve_names);

%       Check if "expression" is a valid MATLAB expression
for ii=1:lxi
   eval([xi{ii},'=param.x(ii);']);
end
for ii=1:lp
   eval([curve_names{ii},'=1;']);
end

try
   eval([expression,';'])
catch
   disp([char(13),' It is very likely, that "',expression,'" is not a valid MATLAB expression']);
   error(' Abnormal termination')
end
  
%       Check if there are selection criteria regarding depth range and rows
if ~isempty(param.depths) || ~isempty(param.rows)
   if ~isempty(param.depths)
      temp=l_select(wlog,{'depths',param.depths{1},param.depths{2}});
      if ~isempty(param.rows)
         temp=l_select(temp,{'rows',param.rows});
      end
   else
      temp=l_select(wlog,{'rows',param.rows});
   end

   [aux,rhs_expression]=find_parameters(temp,expression,xi,param,curve_names);

else
   [aux,rhs_expression]=find_parameters(wlog,expression,xi,param,curve_names);
end


%       Create new curve with the parameters just estimated and add it
%       to input log structure
curves=zeros(nsamp,lp);
for ii=1:lp
   if ~strcmpi(curve_names(ii),mnem)
     curves(:,ii)=l_gc(wlog,curve_names{ii});
   end
end

x=aux.x;                    %#ok Used in "eval" below
test=sum(curves,2);
lindex=find(~isnan(test));
curves=curves(lindex,:);    %#ok Used in "eval" below
temp=eval(rhs_expression);
new_curve=NaN*zeros(nsamp,1);
new_curve(lindex)=temp;

wlog=l_curve(wlog,'add_ne',param.mnem,new_curve,l_gu(wlog,mnem), ...
     [l_gd(wlog,mnem),' (predicted)']);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [aux,rhs_expression]=find_parameters(wlog,expression,xi,param,curve_names)

nsamp=size(wlog.curves,1);
lxi=length(param.x);

%       ... and copy them into matrix "curves", ...
lp=length(curve_names);
curves=zeros(nsamp,lp);

for ii=1:lp
  eval(['curves(:,ii)=l_gc(wlog,''',curve_names{ii},''');']);
end
% curves_orig=curves;

%       ... remove all null values
test=sum(curves,2);
% idx=find(~isnan(test));
curves=curves(~isnan(test),:);

% 	Check if there are enough curve samples left
if size(curves,1) < lxi
   error(' Not enough valid sampes in requested curves')
end

%       Transform "expression" into the objective function to be minimized
%       1. Modify expression to be valid for vectors
expr=strrep(expression,'*','.*');
expr=strrep(expr,'/','./');
expr=strrep(expr,'^','.^');

%       2. replace x1, x2, ... by x(1),x(2),...

for ii=1:lxi
  expr=strrep(expr,xi{ii},['x(',num2str(ii),')']);
end

%       3. replace "=" sign by "-" sign
idx=findstr(expr,'=');
if length(idx) ~= 1
  disp([char(13),' No equal sigh (=) found in expression "',expr])
  error(' Abnormal termination')
end
expr1=[expr(1:idx-1),'-(',expr(idx+1:end),')'];
rhs_expression=expr(idx+1:end);

%       4. replace header mnemonics by columns of array,...
funct=expr1;
for ii=1:lp
  funct=strrep(funct,curve_names{ii},['curves(:,',num2str(ii),')']);
  rhs_expression=strrep(rhs_expression,curve_names{ii},['curves(:,',num2str(ii),')']);
end

%       5. define objective function for the minimization condition
if ischar(param.norm)
   if strcmpi(param.norm,'L1')
      min_cond=['sum(abs(',funct,'))'];

   elseif strcmpi(param.norm,'L2')
      min_cond=['norm(',funct,')'];

   else
      disp([char(13),' Unknown norm "',param.norm,'"'])
      error(' Abnormal termination')
   end

else
  if strcmpi(param.norm{1},'L1')
    funct1=['(',expr1,')'];
    funct2=funct1;
    for ii=1:lp
      funct1=strrep(funct1,curve_names{ii},['curves(',funct,' > 0,',num2str(ii),')']);
      funct2=strrep(funct2,curve_names{ii},['curves(',funct,' < 0,',num2str(ii),')']);
    end
    min_cond=[num2str(param.norm{3}),'*','sum(abs(',funct1,')) + ', ... 
              num2str(param.norm{2}),'*','sum(abs(',funct2,'))'];
  else
     disp([char(13),' Unknown norm "',param.norm,'" or parameters missing from "L1"'])
     error(' Abnormal termination')
  end
end

%       Create inline function for the objective function
ifunct=inline(min_cond,'x','curves');

options=optimset('MaxFunEvals',5000,'LargeScale','off');

if isempty(param.lbounds) && isempty(param.ubounds)   
            % Perform unconstrained minimization
   [x,fval,exitflag,output]=fminunc(ifunct,param.x,options,curves);

else        % Perform constrained minimization
   [x,fval,exitflag,output]=fmincon(ifunct,param.x,[],[],[],[],param.lbounds,param.ubounds, ...
            [],options,curves);
end

%       Prepare output
aux=[];
for ii=1:lxi
%  aux=setfield(aux,xi{ii},x(ii));
  aux.(xi{ii})=x(ii);
end
aux.x=x;
aux.fval=fval;
aux.exitflag=exitflag;
aux.output=output;
