function [lag,cc]=max_corr(ref,b,min_shift,max_shift)
% Function finds best match between vectors "ref" and "b" within the range
% min_shift" and "max_shift"
%         [lag,cc]=max_corr(ref,b,min_shift,max_shift)
% INPUT
% ref        reference trace
% b          trace to be shifted
% min_shift  minimum shift (generally negative)
% max_shift  maximum shift (generally positive)
% OUTPUT
% lag        Lag for best correlation (0 if aligned, negative if b "comes later"
%            than ref)
% cc         correlation coefficient 

% nref=length(ref);
[nb,mb]=size(b);
lag=zeros(mb,1);
if nargout > 1
  cc=zeros(mb,1);
end

nref=norm(ref);

for ii=1:mb
  ccc=corr(ref(:),b(:,ii));
  ncc=length(ccc);
  
% 	Zero lag is at sample nb
  first=min([max([1,nb+min_shift]),ncc]);
  [dummy,lagi]=max(ccc(first:min([max([1,nb+max_shift]),ncc])));
  lag(ii)=lagi-1+first-nb;
  if nargout > 1
    cc(ii)=dummy/(nref*norm(b(:,ii))+eps);
  end
end

