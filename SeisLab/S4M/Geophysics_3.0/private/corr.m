function c = corr(a,b)
%   Correlation of vectors "a" and "b". The resulting
%   vector is length length(a)+length(b)-1
%
na = length(a);
nb = length(b);

if na ~= numel(a) || nb ~= numel(b)
   error('"a" and "b" must be vectors.');
end

% "filter" is substantially faster if the first argument to filter
%  the shorter of the two.
if na > nb
    if nb > 1
        a(na+nb-1)=0;
    end
    c=filter(b(end:-1:1),1,a);
else
    if na > 1
        b=b(end:-1:1);
        b(na+nb-1)=0;
    end
    c=filter(a,1,b);
end

