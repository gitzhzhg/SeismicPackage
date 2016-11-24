function [v2rec] = calcvel2(fbtime,fbcoord,shotcoord,nshots,cvpavg,recelev)
% Calculation of the second layer velocity using Minus Time analysis on the 
% refracted arrivals to determine a Minus Time value at each receivers in between
% the Plus-Minus window limited by the rigth cross over point of shot (i) and the
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

% left cross over point of shot (j) 
v2=NaN.*ones(nshots,nshots);
locv2=NaN.*ones(nshots,nshots);
% Calculation of the second layer velocity using the Minus time analysis
fprintf(1,'Starting velocity calculation loop...\n');
k = 0;     % k is a counter for indexing the hg matrix
for i=1:nshots-1
   for j=i+1:nshots 
     if shotcoord(i)>shotcoord(j)
         tmp = i;
         i = j;
         j = tmp;
       end
       fprintf(1,'Trying shot pair %d %d\n',i,j);
       % Use of the Cross over point average
       if(isnan(cvpavg(i,2))==0 & isnan(cvpavg(j,1))==0)
          % Find the valid arrival times
          validti = find(~isnan(fbtime(i,:)));
          validtj = find(~isnan(fbtime(j,:)));	
        % Limit the range in between the shot location
        if( (shotcoord(i)>=min(fbcoord(j,validtj))) ...
               & (shotcoord(j)<=max(fbcoord(i,validti))) )
             fprintf(1,'calcvel2: calculating for shots %d %d\n',i,j);
             % Find the time at the Plus-Time window limit
             tij=interp1(fbcoord(i,validti),fbtime(i,validti),shotcoord(j),'linear');
             % Limit the range in between the rigth cross over point of shot (i) to the left cross over point of shot (j)
             validfbci=find(fbcoord(i,validti)>cvpavg(i,2) & fbcoord(i,validti) < cvpavg(j,1));
             validfbcj=find(fbcoord(j,validtj)<cvpavg(j,1) & fbcoord(j,validtj) > cvpavg(i,2) );
             [a b]=size(validfbci);
             [c d]=size(validfbcj);
	  if (b>1 & d>1)
             ad1=min(fbcoord(i,validti(validfbci)));
	     indad1=find(fbcoord(i,:)==ad1);
             ad2=max(fbcoord(i,validti(validfbci)));
	     indad2=find(fbcoord(i,:)==ad2);
	     if (indad1>indad2)
		tmp=indad1;
		indad1=indad2;
		indad2=tmp;
	     end
   	     n=indad2-(indad1+1);
             x=NaN.*ones(1,n);
             tminus=NaN.*ones(1,n);        
           % Calculation of the Minus Time value inside the window for each receiver
           if(n>2)
            for indad=indad1+1:indad2
             x(1,indad-indad1)=fbcoord(i,indad)-fbcoord(i,indad1);
             a=fbtime(i,indad)-fbtime(i,indad1);
	     indhd=find(fbcoord(j,:)==fbcoord(i,indad));
	     indhd1=find(fbcoord(j,:)==fbcoord(i,indad1));
             b=fbtime(j,indhd1)-fbtime(j,indhd);
             tminus(1,indad-indad1)=a+b;
            end
             % Find the velocity by using a polyfit of the Minus Time values
	     goodt=find(~isnan(tminus(1,:)));
             xdist=2*x;
	     p=polyfit(xdist(1,goodt),tminus(1,goodt),1);
             v2(i,j)=abs(1/p(1,1));
             % Coordinate of the corresponding velocity
             r=(fbcoord(i,indad2)-fbcoord(i,indad1))/2;
	     locv2(i,j)=r+fbcoord(i,indad1);
           end
          end
        end
     end
   end
end
% Find all the unique location of the second layer velocity (v2)
goodloc = find(~isnan(locv2));
locv2a=reshape(locv2,1,size(locv2,1)*size(locv2,2));
locv2s=sort(locv2a(goodloc));
locv2g=locv2s(1:size(locv2s,2)-1) - locv2s(2:size(locv2s,2));
locv2i=[ 1 (find(locv2g~=0)+1)];
uniqloc=locv2s(locv2i);
% Store all the valid velocity in a matrix (shot number, unique location)
[tmp nloc]=size(uniqloc);
allv2=NaN.*ones(nshots,nloc);
for i=1:nshots-1
   for j=2:nshots
	if(~isnan(v2(i,j))==1)
         ind=find(uniqloc==locv2(i,j));
	 allv2(i,ind) = v2(i,j);
        end
   end
end
% Calculation of the average velocity at each unique location
for n=1:nloc
	validv2=find(~isnan(allv2(:,n)));
	[a b]= size(validv2);
    if( b ~=0 )
	avgv2(n)=mean(allv2(validv2,n));
	stdv2(n)=std(allv2(validv2,n));
	fold(n)=a;
    end
end
% Interpolation of the 2nd layer velocity at the receiver location 
minloc=min(uniqloc);
maxloc=max(uniqloc);
intindex=find(recelev(1,:)>minloc & recelev(1,:)<maxloc);
extfind=find(recelev(1,:)<minloc);
extlind=find(recelev(1,:)>maxloc);
v2rec=interpextrap(uniqloc,avgv2,recelev(1,:));
% Replacement of both end of the array by a constant velocity average 
v2fext=mean(v2rec(1,intindex(1:4)));
v2lext=mean(v2rec(1,intindex(length(intindex)-4:length(intindex))));
v2rec(1,extfind)=v2fext*ones(1,length(extfind));
v2rec(1,extlind)=v2lext*ones(1,length(extlind));
% Update the menus
PMTsetmenus;