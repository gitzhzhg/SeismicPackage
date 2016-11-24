function words=extract_words(string)
% Function extracts character groups from input character string "string" and stores
% them in a cell array; character groups must start with a letter and can contain
% numbers and underscores.
%
% Written by: E. Rietsch: Date March 4, 2000;
% Last updated: December 7, 2000: words must satisfy the same rules as MATLAB variables
%
%            words=extract_words(string)
% INPUT 
% string     String to be analyzed
% OUTPUT
% words      Cell array containing all contiguous groups of letters including
%            underscores

idx=(isletter(string)  |  string == '_'  |  (double(string) > 47 & double(string) < 58));
didx=diff([0,idx,0]);
index=find(didx ~= 0);
nw=length(index);

if nw == 0
   words=cell(0);
else
   words=cell(fix((nw+1)/2),1);
   for ii=1:2:nw
      words((ii+1)/2)={string(index(ii):index(ii+1)-1)};
   end
end

%       Eliminate words that start with a number
count=0;
for ii=1:length(words);
   if isletter(words{ii}(1))
      count=count+1;
      words(count)=words(ii);
   end
end
words(count+1:length(words))=[]; 
