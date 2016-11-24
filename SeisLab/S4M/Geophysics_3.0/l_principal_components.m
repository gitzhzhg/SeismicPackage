function [wlog,aux]=l_principal_components(wlog,varargin)
% Compute principal-component representation of two or more log curves.
%
% Written by: E. Rietsch: January 30, 2008
% Last updated: February 21, 2008: Generalize the way curve mnemonics are
%                                  specified
%
%         [wlog,aux]=l_principal_components(wlog,varargin)
% INPUT
% wlog
% varargin  one or more cell arrays; the first element of each cell array 
%         is a keyword, the other elements are parameters. 
%         Presently, keywords are:
%     'curves'  mnemonics of the curves to include in PC analysis
%         Default: {'curves',[]}  This means use: use all curves excepth the depth
%     'logdomain'   Possible values are "true" (or 'yes') and "false" (or 'no').
%         if "true" the principal components are computed for the logarithm of 
%            the curves, and the final result is exponentiated.
%         Default: {'logdomain',true}
%     'npc'  number of principal componets to use for the principal-component 
%         representation of the selected log curves.
%         Default: {'npc',2}
%     'option'  Options for the computed curves; possible strings are
%         'append'   Append the new curves after the old ones; the mnemonics of
%             the new curve have the original curve mnemonics with 
%             ['_pc',num2str(npc)] appended. The string [' (PC ',num2str(npc),')']
%             is appended to the curve description. 
%         'replace'  Replace the old curves by the new curves; curve mnemonics
%             and description are not changed.
%         Default: {'option','append'}
%     'scaling'  Possible values are "true" (or 'yes') and "false" (or 'no').
%         if "scaling"s true then the curves are scaled to have norm 1 before 
%         computing the principal components. This is important if the curves
%         have different scales (such as density and velocity)
%         Default: {'scaling',true}
% OUTPUT
% wlog    Input log with the new curves repacing the old curves or appended
%         to the existing curves
% aux     structure with auxiliary output data
%
% EXAMPLE
%         wlog=l_data;
%         [wlog1,aux]=l_principal_components(wlog,{'curves','vp','vs','rho'});
%
%         l_curve(wlog1)
%
%         lfigure
%         subplot(1,3,1)
%            h1=l_plot1(wlog1,{'curves','rho','rho_pc2'},{'depths',9000,9100},{'figure','old'});
%         subplot(1,3,2)
%            h2=l_plot1(wlog1,{'curves','Vp','Vp_pc2'},{'depths',9000,9100},{'figure','old'});
%         subplot(1,3,3)
%            h3=l_plot1(wlog1,{'curves','Vs','Vs_pc2'},{'depths',9000,9100},{'figure','old'});
%         linkaxes([h1.axis_handle,h2.axis_handle,h3.axis_handle],'y')


%        Defaults of input arguments
param.npc=2;
param.curves=[];
param.logdomain=true;
param.option='append';
param.scaling=true;
   
%        Replace defaults with actual input arguments
param=assign_input(param,varargin);

%        Handle different options of providing the curves to use for Principal
%        Component analysis
if isempty(param.curves)
   param.curves=wlog.curve_info(2:end)';
elseif ischar(param.curves)
   param.curves=tokens(param.curves);
elseif iscell(param.curves{1})
   param.curves=param.curves{1};
else
   % Do nothing
end

index=curve_indices(wlog,param.curves);

ncurves=length(param.curves);
if length(index) ~= ncurves
   disp(' Not all requested curves have been found.')
   disp(' Curves requested:')
   disp(cell2str(param.curves,','))
   disp(' Existing curves:')
   disp(cell2str(wlog.curve_info(:,1),','))
   error('Abnormal termination.')
end

mat=wlog.curves(:,index);

%       Check for and remove NaNs
bool=all(~isnan(mat),2);
index1=find(bool);
if isempty(index1)
   mywarning('No rows without NaNs found.')
   return
end

if length(index) == 1
   mywarning('Only one row without NaNs found; no action taken.')
end

mat1=mat(index1,:);

%       Take the logarithm of the curves if requested
if isyes(param.logdomain)
   mat1=log(mat1);
end

%       Scale the matrix columns so that they have unity norm
if isyes(param.scaling)
   scf=vnorm(mat1);
   mat1=bsxfun(@times,mat1,1./scf);
end

%       Compute principal components
[pc,d,coeff]=principal_components(mat1);

%       Remove effect of previously applied scaling by scaling the coefficients
if isyes(param.scaling)
   coeff=bsxfun(@times,scf,coeff);
end

%       Compute the first "param.npc" sums of the principal components
mat1=pc(:,1:param.npc)*coeff(1:param.npc,:);

if nargout > 1
   aux.coeff=coeff;
   aux.d=d;
   aux.pc=pc;
   aux.curves=wlog.curve_info(index,1)';
end

%       Exponentiate the curves if requested
if isyes(param.logdomain)
   mat1=exp(mat1);
end

mat(:)=NaN;
mat(index1,:)=mat1;

switch param.option
case 'append'
   wlog.curves=[wlog.curves,mat];
   info=wlog.curve_info(index,:);
   append1=['_pc',num2str(param.npc)];
   append3=[' (PC ',num2str(param.npc),')'];
   for ii=1:ncurves
      info(ii,[1,3])={[info{ii,1},append1],[info{ii,3},append3]};
   end
   wlog.curve_info=[wlog.curve_info;info];

case 'replace'
   wlog.curves(:,index)=mat;

otherwise
   error(['Unknown option: ',param.option])

end
