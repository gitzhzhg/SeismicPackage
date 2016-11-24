function [tdiff1, tdiff2, start1indexj, start2indexj, end1indexj, end2indexj, i, j] = tsub(i, j, shotcoord, fbcoord, fbtime)
% Subtract traveltimes of shot i from j 
% i, j = shot numbers
% shotcoord = shot location vector
% fbcoord = receiver location matrix (shot, rec)
% fbtime = arrival time matrix (shot, rec)
% traveltime difference between adjacent shot records 
% at the same receiver locations;
% Shot coordinate of (i) has to be smaller than shot coordinate of (j)
% Overlapping traveltime section on the left of shot (i) (<coordinate) is 
% used to find the left traveltime difference (tdiff1) and the left Cross
% oVer Point of shot (i), the overlapping traveltime section on the right of 
% shot (j) (>coordinate) is used to find the right traveltime difference 
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

% (tdiff2) and right Cross oVer Point of shot (j). 
if shotcoord(i)>shotcoord(j)
  tmp=i;
  i=j;
  j=tmp;
end
% Find the valid traveltime and the valid coordinate
validxi = find(~isnan(fbcoord(i,:)));
validxj = find(~isnan(fbcoord(j,:)));
validti = find(~isnan(fbtime(i,:)));
validtj = find(~isnan(fbtime(j,:)));
% Find the starting coordinate of the left range section 
if( length(validti) > 0 & length(validtj) > 0)
   [startp1x startp1xi] = min(fbcoord(i,validxi));
   [startp2x startp2xi] = min(fbcoord(j,validxj));
   start1x = max(startp1x, startp2x);
   % Find the left range valid coordinate of the two shots which will 
   % be subtracted
   jsubpoints1 = find(fbcoord(j,:)<shotcoord(i) & fbcoord(j,:)>=start1x);
   isubpoints1 = find(fbcoord(i,:)<shotcoord(i) & fbcoord(i,:)>=start1x);
   % Find the valid traveltime inside the valid coordinate range 
   if (length(jsubpoints1)>0 & length(isubpoints1)>0)
     indj = find(validtj<=max(jsubpoints1)&validtj>=min(jsubpoints1));
     indi = find(validti<=max(isubpoints1)&validti>=min(isubpoints1));
     % Find the valid traveltime indices for both shot at the valid coordinate 
     if (length(indi)>2 & length(indj)>2)
       clear goodindi;
       clear goodindj;
       clear goodj;
       clear goodi;
       goodindex=[];
       goodindi=[];
       nn = 1;
       for n=indj
	goodindex=find(fbcoord(i,validti(indi))==fbcoord(j,validtj(n)));
         if(~isempty(goodindex))
            goodindi(nn)= goodindex;
            nn = nn+1;
	 end
       end
       if(~isempty(goodindi))
         goodi=validti(indi(goodindi));
         nn=1;
         for n=goodi
           goodindex = find(fbcoord(j,validtj(indj))==fbcoord(i,n));
           if(~isempty(goodindex))
              goodindj(nn)= goodindex;
              nn = nn+1;
           end
         end
         goodj=validtj(indj(goodindj));
         % Now, work forwards from the beginning of the left section to find
         % the index of the end of the left section according to shot (j).
         end1x = max(fbcoord(j,goodj));
         end1indexj = find(fbcoord(j,:) == end1x);
	 % Find the index of the beginning of the left section
         % according shot (j)
         start1x = min(fbcoord(j,goodj));
         start1indexj = find(fbcoord(j,:) == start1x);
         % Find the traveltime difference of the left section
         tdiff1 = fbtime(j,goodj) - fbtime(i,goodi);
	 % Interpolate for the nonvalid values inside the left section  
         if (start1indexj<end1indexj)
	  if (length(goodj)<length(start1indexj:end1indexj))
           tdiff1=interp1(fbcoord(j,goodj),tdiff1,fbcoord(j,start1indexj:end1indexj));
           tdiff1=tdiff1';
	  end
         else
           tmp=start1indexj;
           start1indexj=end1indexj;
           end1indexj=tmp;
	  if (length(goodj)<length(start1indexj:end1indexj))
           tdiff1=interp1(fbcoord(j,goodj),tdiff1,fbcoord(j,start1indexj:end1indexj));
           tdiff1=tdiff1';
	  end
         end
       else
         tdiff1=[];
       end
      else
       tdiff1=[];
      end
    else
       tdiff1=[];
    end
% Find the end coordinate of the right range section
   [endp1x endp1xi] = max(fbcoord(i,validxi));
   [endp2x endp2xi] = max(fbcoord(j,validxj));
   end2x = min(endp1x, endp2x);
   % Find the right range valid coordinate of the two shots which will
   % be subtracted
   jsubpoints2 = find(fbcoord(j,:)>shotcoord(j) & fbcoord(j,:)<=end2x);
   isubpoints2 = find(fbcoord(i,:)>shotcoord(j) & fbcoord(i,:)<=end2x);
   % Find the valid traveltime inside the valid coordinate range 
   if (length(jsubpoints2)>0 & length(isubpoints2)>0)
     indj = find(validtj<=max(jsubpoints2)&validtj>=min(jsubpoints2));
     indi = find(validti<=max(isubpoints2)&validti>=min(isubpoints2));
     % Find the valid traveltime indices for both shot at the valid coordinate
     if (length(indi)>2 & length(indj)>2)
       clear goodindi;
       clear goodindj;
       clear goodj;
       clear goodi;
       goodindex=[];
       goodindi=[];
       nn = 1;
       for n=indj
	goodindex=find(fbcoord(i,validti(indi))==fbcoord(j,validtj(n)));
         if(~isempty(goodindex))
            goodindi(nn)= goodindex;
            nn = nn+1;
	 end
       end
       if(~isempty(goodindi))
         goodi=validti(indi(goodindi));
         nn=1;
         for n=goodi
           goodindex = find(fbcoord(j,validtj(indj))==fbcoord(i,n));
           if(~isempty(goodindex))
              goodindj(nn)= goodindex;
              nn = nn+1;
           end
         end
         goodj=validtj(indj(goodindj));
         % Now, work backwards from the end of the right section to find the
         % index of the beginning of the right section according to shot (j).
         start2x = min(fbcoord(j,goodj));
         start2indexj = find(fbcoord(j,:) == start2x);
	 % Find the index of the end of the right section
         % according shot (j)
         end2x = max(fbcoord(j,goodj));
         end2indexj = find(fbcoord(j,:) == end2x);
         % Find the traveltime difference of the right section
         tdiff2 = fbtime(i,goodi) - fbtime(j,goodj);
	 % Interpolate for the nonvalid values inside the right section
         if (start2indexj<end2indexj)
  	  if (length(goodj)<length(start2indexj:end2indexj))
           tdiff2=interp1(fbcoord(j,goodj),tdiff2,fbcoord(j,start2indexj:end2indexj));
           tdiff2=tdiff2';
	  end
         else
           tmp=start2indexj;
           start2indexj=end2indexj;
           end2indexj=tmp;
	  if (length(goodj)<length(start2indexj:end2indexj))
           tdiff2=interp1(fbcoord(j,goodj),tdiff2,fbcoord(j,start2indexj:end2indexj));
           tdiff2=tdiff2';
	  end
         end
       else
         tdiff2=[];
       end
      else
       tdiff2=[];
      end
    else
       tdiff2=[];
    end
else
   tdiff1=[];
   tdiff2=[];
end