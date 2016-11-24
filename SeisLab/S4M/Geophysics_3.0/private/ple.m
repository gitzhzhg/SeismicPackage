function ple(s)
%PLE "Print Last Error"
%
% ple
% ple(s)
%
% If an error structure is supplied, it is used instead of lasterror

if nargin < 1
   s = lasterror;
end

if isempty(s.message)
   fprintf(1,'No error message stored\n');
   return;
end

fprintf(1,'Last Error: %s (%s)\n',s.message,s.identifier);
for i=1:numel(s.stack)
   e = s.stack(i);
   ff = which(e.file);
   [ignore_dir,command] = fileparts(ff);
   n = e.name;
   href = sprintf('matlab:opentoline(''%s'',%d)',ff,e.line);
   if strcmp(command,n)
       % main function in this file
       fprintf(1,'    <a href="%s">%s,%d</a>\n',href,ff,e.line);
   else
       % subfunction in this file
       fprintf(1,'    <a href="%s">%s >%s,%d</a>\n',href,ff,n,e.line);
   end
end
fprintf(1,'\n');
