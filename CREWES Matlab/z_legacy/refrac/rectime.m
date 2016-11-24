function [diffmat]=rectime(rtrange,rtpair1,rtpair2,fbcoord,shotcoord,fbtime,mint,nshots)
% Function determining the reciprocal time difference between two shot
% Difference between the arrival time value of the shot (i) at the shot (j)
% location and the arrival time value of shot (j) at the shot (i) location  
% nshots = number of shots 
% nrecs = number of receivers 
% fbcoord = receiver location (nshots,nrecs)
% fbtime = refracted arrival matrix (nshots, nrecs)
% shotcoord = shot location vector (nshots)
% function diffmat = rectime(nshots, nrecs, fbcoord, fbtime, shotcoord)
% Output: diffmat = matrix of reciprocal time differences for all the 
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

% possible shot pairs (nshots x nshots)
diffmat=NaN.*ones(nshots, nshots);
% Reciprocal time difference for all the possible shot pairs
if( rtrange==0)
	for i=1:nshots-1;       % i is the first shot (t1)
          fprintf(1,'rectime: processing shot %d\n',i);
	  for j=i+1:nshots;    % j is the 2nd shot (t2)
	    t1 = NaN;
	    t2 = NaN;
            validti = find(~isnan(fbtime(i,:)));
            validtj = find(~isnan(fbtime(j,:)));	
	    if (length(validti)>0 & length(validtj)>0)
              if( (shotcoord(j)>=min(fbcoord(i,validti))) & (shotcoord(j)<=max(fbcoord(i,validti))) )
	        t1=interp1(fbcoord(i,validti),fbtime(i,validti),shotcoord(j),'linear');
	      end
              if( (shotcoord(i)>=min(fbcoord(j,validtj))) & (shotcoord(i)<=max(fbcoord(j,validtj))) )
	       t2=interp1(fbcoord(j,validtj),fbtime(j,validtj),shotcoord(i),'linear');
	      end
	    end
	    diffmat(i,j) = t2-t1;
	  end
	end
% Reciprocal time difference for a specified shot pair
else
	i=rtpair1
	j=rtpair2
	t1 = NaN;
	t2 = NaN;
        validti = find(~isnan(fbtime(i,:)));
        validtj = find(~isnan(fbtime(j,:)));
	if (length(validti)>0 & length(validtj)>0)
 	  if( (shotcoord(j)>=min(fbcoord(i,validti))) & (shotcoord(j)<=max(fbcoord(i,validti))) )
	      t1=interp1(fbcoord(i,validti),fbtime(i,validti),shotcoord(j),'linear');	
	  end
	  if( shotcoord(i) >= min(fbcoord(j,validtj)) & shotcoord(i) <= max(fbcoord(j,validtj)) )
	      t2=interp1(fbcoord(j,validtj),fbtime(j,validtj),shotcoord(i),'linear');
 	  end
	end
	diffmat(i,j) = t2-t1;
end
absdiffmat=abs(diffmat);
[s1,s2]=find(absdiffmat>mint);  %reciprocal time bigger than a minimum time(ms)
m=size(s1);
% We need to save the current figure, and restore it afterwards, so that
% refdata (which stores data under the MAIN figure) will work.
f = gcf;
% Display the reciprocal time difference for the possible shot pairs
if( rtrange==0)
	figure('menubar','none');
	plot(absdiffmat,'*');
	xlabel('shot number');
	ylabel('Reciprocal traveltime difference (ms)');
	title('Reciprocal time difference for all possible shot pairs ')
% Display of the shot pairs over a reciprocal time difference limit
	figure('menubar','none');
	plot(s2,s1,'*');
	xlabel('shot number');
	ylabel('shot number');
	titlestr = sprintf('Shot pair(s) with over %d ms of reciprocal time difference ', mint);
	title(titlestr);
else
	mint
	t2-t1
end
figure(f); set(gcf,'menubar','none');