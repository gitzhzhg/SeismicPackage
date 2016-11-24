function corr=xcorr_all(a,b,up,down,normalize)
% Function computes the crosscorrelation of each column of matrix "a" with each
% column of matrix "b".
% The function does no input error checking!
%           corr=xcorr_all(a,b,up,down,normalize)
% INPUT
% a     matrix
% b     matrix
% up    number of samples "b" must be shifted up with regard to "a" for the
%       computation of the first lag of the crosscorrelation
% down  number of samples "b" must be shifted down with regard to "a" for the
%       last sample of the crosscorrelation
% normalize  Type of normalization to apply prior to computing the crosscorrelation
%       Possible values are:    'no'    ==> no normalization
%                               'traces' ==> columns of "a" and "b" are normalized to have
%                                   L2 norm 1
%                               'segment' ==>
%                     in this case the condition "up >= 0 && down <= abs(na-nb)" must be satisfied
%                     where "na" and "nb" denote the number of rows of "a" and "b".
% OUTPUT
% corr  correlation coefficients; 
% if "which == 'all'" then the first ma colums of "corr" contain the correlations of "a"
% with the the first column of "b", the second "ma" columns of "corr" contain the correlations
% of "a" with the second column of "b", etc. ("ma" and "mb" denote the number of columns of "a"
% and "b", respectively) and "corr" has dimension (down-up) x (ma*mb) 
% Otherweise, "corr" has dimension (down-up) x ma.

[na,ma]=size(a);
[nb,mb]=size(b);

if strcmpi(normalize,'segment') && (up < 0 || down > abs(na-nb))
   disp([char(13),' For segment normalization the correlation shifts (lags) must'])
   disp(' not move one trace beyond the beginning or the end of the other')
   error(' Abnormal termination')
end

mm=ma*mb;
corr=zeros(down-up+1,ma*mb);

if strcmpi(normalize,'traces')
   for ii=1:ma
      a(:,ii)=a(:,ii)/(norm(a(:,ii))+eps);
   end
      
   for ii=1:mb
      b(:,ii)=b(:,ii)/(norm(b(:,ii))+eps);
   end
end

counter=0;

if na >= nb
   if strcmpi(normalize,'segment')
      for ii=1:mb
         b(:,ii)=b(:,ii)/(norm(b(:,ii))+eps);
      end
      
      for ii=up:down
         counter=counter+1;
         if nb+ii < 1
            % Do nothing
         elseif ii < 0
            temp=a(1:nb+ii,:);
            for jj=1:ma
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
            end
            corr(counter,:)=reshape(temp'*b(1-ii:nb,:),1,mm);
         elseif ii <= na-nb
            temp=a(1+ii:nb+ii,:);
            for jj=1:ma
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
            end
            corr(counter,:)=reshape(temp'*b,1,mm);
         elseif ii < na
            temp=a(1+ii:na,:);
            for jj=1:ma
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
            end
            corr(counter,:)=reshape(temp'*b(1:na-ii,:),1,mm);
         end
      end
      
   else  
      for ii=up:down
         counter=counter+1;
         if nb+ii < 1
            % Do nothing
         elseif ii < 0
           corr(counter,:)=reshape(a(1:nb+ii,:)'*b(1-ii:nb,:),1,mm);
         elseif ii <= na-nb
           corr(counter,:)=reshape(a(1+ii:nb+ii,:)'*b,1,mm);
         elseif ii < na
           corr(counter,:)=reshape(a(1+ii:na,:)'*b(1:na-ii,:),1,mm);
         end
      end
   end
   
else                        % nb > na
   if strcmpi(normalize,'segment')
      for ii=1:ma
         a(:,ii)=a(:,ii)/(norm(a(:,ii))+eps);
      end
    
      for ii=up:down
         counter=counter+1;
         if nb+ii < 1
           % Do nothing
         elseif nb+ii <= na
            temp=b(1-ii:nb,:);
            for jj=1:mb
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
            end
            corr(counter,:)=reshape(a(1:nb+ii,:)'*temp,1,mm);
         elseif ii > na-nb && ii <= 0
            temp=b(1-ii:na-ii,:);
            for jj=1:mb
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
            end
            corr(counter,:)=reshape(a'*temp,1,mm);
         elseif ii < na
            temp=b(1:na-ii,:);
            for jj=1:mb
               temp(:,jj)=temp(:,jj)/(norm(temp(:,jj))+eps);
            end
            corr(counter,:)=reshape(a(1+ii:na,:)'*temp,1,mm);
         end
      end
     
   else  
      for ii=up:down
        counter=counter+1;
        if nb+ii < 1
             % Do nothing
        elseif nb+ii <= na
            corr(counter,:)=reshape(a(1:nb+ii,:)'*b(1-ii:nb,:),1,mm);
        elseif ii > na-nb && ii <= 0
            corr(counter,:)=reshape(a'*b(1-ii:na-ii,:),1,mm);
        elseif ii < na
            corr(counter,:)=reshape(a(1+ii:na,:)'*b(1:na-ii,:),1,mm);
        end
      end
   end
end

