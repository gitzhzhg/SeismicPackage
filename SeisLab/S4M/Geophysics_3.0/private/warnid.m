function wid=warnid
% Create an identifier for warnings based on the name of the calling function
%
% Written by: E. Rietsch: July 6, 2006
% Last updated:
%
%       wid=warnid
% OUTPUT
% wid   identifier for "warning" (first input argument)
%
% EXAMPLE
%       warning(warnid,'This is a warning')

%       Find the name of the calling program and append it to "SeisLab:"
ll=dbstack';
nll=length(ll);

if nll > 0
   wid=cell(nll,1);
   wid{1}=' ';
   for ii=2:nll
      wid{ii}=['SeisLab:',ll(ii).name,', line ',num2str(ll(2).line)];
   end
   wid=wid{end};
else
   wid='';
end
