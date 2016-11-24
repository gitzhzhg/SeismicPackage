function [td1]=timeanal(fbtime,fbcoord,shotcoord,nshots,diffmat,cvpavg,offsetpt)
% For all possible shot pairs, calculation of the delay time using Plus 
% time analysis at each receiver inside a window define by the right cross 
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

% over point of the left shot and the left cross over point of the right shot 
[tmp nfbcoords] = size(fbcoord); 
fprintf(1,'Starting time analysis loop...\n');
k = 0;     % k is a counter for indexing the td1 matrix
for i=1:nshots-1
    for j=i+1:nshots 
      if shotcoord(i)>shotcoord(j)
         tmp = i;
         i = j;
         j = tmp;
       end
       fprintf(1,'Trying shot pair %d %d\n',i,j);
       if(isnan(cvpavg(i,2))==0 & isnan(cvpavg(j,1))==0)
          validti = find(~isnan(fbtime(i,:)));
          validtj = find(~isnan(fbtime(j,:)));	
	if (length(validti)>0 & length(validtj)>0)	
          if( (shotcoord(i)>=min(fbcoord(j,validtj))) ...
               & (shotcoord(j)<=max(fbcoord(i,validti))) )
             fprintf(1,'timeanal: calculating for shots %d %d\n',i,j);
             tij=interp1(fbcoord(i,validti),fbtime(i,validti),shotcoord(j),...
             'linear'); % Find the reciprocal time difference
	     % Establish the Plus time analysis window
	     if(offsetpt ~= 0 & isnan(offsetpt) ~= 1 )
		if( cvpavg(i,2) < (shotcoord(j)-offsetpt))
		  l=(shotcoord(j)-offsetpt);
		else
		  l=cvpavg(i,2);
		end
		if( cvpavg(j,1) > (shotcoord(i)+offsetpt))
		  r=(shotcoord(i)+offsetpt);
		else
		  r=cvpavg(j,1);
		end
	     else
		l=cvpavg(i,2);
		r=cvpavg(j,1);
	     end
             validfbci=find(fbcoord(i,validti) > l & fbcoord(i,validti) < r);
             validfbcj=find(fbcoord(j,validtj) > l & fbcoord(j,validtj) < r);
             [a b]=size(validfbci);
             [c d]=size(validfbcj);
             indad1=[];
             indad2=[];
	
             if ( b>0 & d>0 )
                ad1=min(fbcoord(i,validti(validfbci)));
	        indad1=find(fbcoord(i,:)==ad1);
             ad2=max(fbcoord(i,validti(validfbci)));
	     indad2=find(fbcoord(i,:)==ad2);
	     if (indad1>indad2)
		tmp=indad1;
		indad1=indad2;
		indad2=tmp;
	     end
	    end
             k = k+1;
             td1(k,1:nfbcoords+2) = NaN .* ones(1,nfbcoords+2);
             td1(k,1) = i;
             td1(k,2) = j;
	     % Find the Plus time values at each receivers inside
	     % the Plus time analysis window and the offset limit
	     if (length(indad1:indad2)>0)
                for indad=indad1:indad2
	          indhd=find(fbcoord(j,:)==fbcoord(i,indad));
                  tad=fbtime(i,indad);
                  thd=fbtime(j,indhd);
		  if(diffmat(i,j)~=0 & isnan(diffmat(i,j))~=1)
                    tplus=tad + thd - tij - diffmat(i,j);
		  else
		    tplus=tad + thd - tij;
		  end
                  td1(k,indad+2)=tplus;
		end
             end
            end
          end
	end
       end
    end
end
% Calculate the average Plus Time at each receiver locations
plustreject=0;
standard=0;
dev=0;
% Call the Plus Time averaging function
plust = avgplustime(fbcoord,td1,standard,plustreject,dev);
refdata('set','plust',plust);
% Update menus
PMTsetmenus;