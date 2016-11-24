function index=ismember_ordered(set1,set2)
% Function operates like index=find(ismember(set1,set2)), except that the output
% is ordered in the sense that index(1) is the index of the element of set1 that
% matches the first element of set2 (if it exists), etc.
%
% Written by: E. Rietsch: November 9, 2005
% Last updated:
%
%           index=ismember_ordered(set1,set2)
% INPUT
% set1  numeric vector
% set2  numeric vector
% OUTPUT
% index
%
% EXAMPLE
%     index=ismember_ordered([11,22],[3,4,22,11])    
%     index1=find(ismember([11,22],[3,4,22,11]))

if ischar(set2)
   index=find(ismember(set1,set2));
   return
else
   ll=length(set2);
   index=zeros(size(set1));
   kk=1;
   for ii=1:ll
      temp=find(ismember(set1,set2(ii)));
      lt=length(temp);
      if lt > 0
        index(kk:kk+lt-1)=temp;
        kk=kk+lt;
      end
   end
end
index(kk:end)=[];
