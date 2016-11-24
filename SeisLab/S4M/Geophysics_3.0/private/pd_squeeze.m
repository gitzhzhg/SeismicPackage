function pdf=pd_squeeze(pdf)
% Eliminate variates of a p.d.f. that have only one value; store the value 
% of the variate in form of a parameter of the p.d.f.
%
% Written by: E. R.: July 8, 2003
% Last updated: March 6, 2006: bug fixes
%
%        pdf=pd_squeeze(pdf)
% INPUT
% pdf    p.d.f.
% OUTPUT
% pdf    input p.d.f. with any singleton dimension eliminated. Associated
%        variates are converted to parameters


lvariates=length(pdf.variates);
if lvariates <= 1
   return
end

dims=md_size(pdf.pdf);
ldims=length(dims);
if ldims < lvariates;
   dims=[dims,ones(1,lvariates-ldims)];
end

bool=(dims == 1);
if ~any(bool)
   return
end

for ii=1:lvariates
   if dims(ii) == 1
      pdf=add_parameter(pdf,pdf.variates{ii},pdf.variate_info(ii,:));
      try
         pdf.edges(ii)=[];                        % Remove edges (if they exist)
      catch
      end
      if ii <= pdf.dimensions(1)
         pdf.dimensions(1)=pdf.dimensions(1)-1;   % Reduce the number of "variates" by 1
      else
         pdf.dimensions(2)=pdf.dimensions(2)-1;   % Reduce the number of "givens" by 1
      end
   end
end

pdf.variate_info(bool,:)=[];   % Remove the variate_info
pdf.variates(bool)=[];         % Remove the variate
pdf.pdf=mysqueeze(pdf.pdf);

if isfield(pdf,'cdf')
   pdf.cdf=mysqueeze(pdf.cdf);
   pdf.cdfdirs=pdf.cdfdirs(~bool);
end

pdf=pd_history(pdf);
