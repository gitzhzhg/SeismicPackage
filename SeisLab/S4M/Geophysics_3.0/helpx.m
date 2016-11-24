function warnings=helpx(func)
% Run the example in the help section of a function
% The help section starts with the line "% EXAMPLE"


string=blanks(80);
string=strrep(string,' ','=');

helptext = help(func);
if isempty(helptext)
   warnings=['Function "',func,'" has no help section'];
   return
end

%linesep = [1 regexp(helptext,'\n')]
htext=tokens_no2(helptext,'\n');
idx=strmatch('EXAMPLE',strtrim(htext));

if ~isempty(idx)
   if length(idx) > 1
      warnings=['Function "',func,'" has more than one EXAMPLE'];
   else
      label=['Example for "',func,'"  '];
      disp([label,string(1:80-length(label))])
      hhh=htext(idx+1:end);
      hhh=splice_continued_lines_no1(hhh);
      try
         for ii=1:length(hhh)
            temp=[strtrim(hhh{ii}),' '];  % Add a blank so that empty lines are displayed
            disp(temp)
            pause(0)     % Make sure that the preceeding "display" command is 
                         % executed befor the result of "eval" is displayed
%            if ~isempty(temp) 
               eval(temp,',');
%            end
         end
      catch    
%         ple
%         keyboard
      end
      if nargout > 0
         warnings=' ';
      end
   end
else
   warnings=['Function "',func,'" has no EXAMPLE'];
end      


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function htext=splice_continued_lines_no1(htext)
%	Splice command lines that end in ...

for ii=length(htext)-1:-1:1
   temp=deblank(htext{ii});
   if length(temp) > 3
      while strcmp(temp(end-2:end),'...')
         temp=[temp(1:end-3),htext{ii+1}];
         htext(ii+1)=[];
      end
   end
   htext{ii}=temp;
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function cstr=tokens_no2(str,sep)
linesep = [0 regexp(str,sep)];
ntokens=length(linesep)-1;
if ntokens < 1
   cstr=[];
   return
end

cstr=cell(length(linesep),1);

ik=0;
for ii=1:ntokens
   temp=str(linesep(ii)+1:linesep(ii+1)-1);
   if 1        %~isempty(temp)
      ik=ik+1;
      cstr{ik}=temp;
   end
end
cstr(ik+1:end)=[];
