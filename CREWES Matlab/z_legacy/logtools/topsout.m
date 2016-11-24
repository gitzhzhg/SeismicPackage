function lasheader_t = topsout(lasheader,l1,l2,...
        tops,ztops,flag)
% updates  tops in  lasheader 
% Totally opaque code. Better to rewrite than debug
%
% t.n.bishop, CCR, July 94
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
if(nargin<=5)
	flag=0;
end
%set flag to 1 if writting out in time
    
  blank='                        ';
  [nlinesold,ncharold]=size(lasheader);
  lasheader_t=ones(nlinesold,ncharold);
  ntopsold=l2-l1+1;
  ntopsnew=length(ztops);
  ntopsdiff=ntopsnew-ntopsold;
  nlinesnew=nlinesold+ntopsdiff;
  % lines 1 to l1-1 are same, l1 to l2 will be replaces
  % by the new tops, and l2+1 to nlinesold will go into
  % lines 
% check null case (no tops in lasheader)
  if(l1 > l2)   %this is true if no tops (see topsin)
		 %find ~CURVE and put tops just before
		 for k=1:nlinesold
			if( strcmp(upper(lasheader(k,1:6)),'~CURVE') )
				l1=k;
				break;
			end
		end
	  for line=1:(l1-1)
		%if(~strcmp(upper(lasheader(line,2:5)),'TOPS'))
		 lasheader_t(line,1:ncharold)=...
				  lasheader(line,1:ncharold);
		%end
	  end
	  %[mm,nn]=size(lasheader_t);
	  %l1=mm+1;
    %str=['~Tops' blank blank blank blank blank];
    str=['~Tops' blanks(ncharold-5)];
    lasheader_t(l1,1:ncharold)=str(1:ncharold);
		if(flag)
			 str=['#TOPS NAME   .         TIME:'...
				blank blank blank blank blank];
		else
			 str=['#TOPS NAME   .        DEPTH:'...
				blank blank blank blank blank];
		end
		if(length(str)<ncharold)
			str=[str blanks(ncharold-length(str))];
		end
    lasheader_t(l1+1,1:ncharold)=str(1:ncharold);
    ntopsdiff=2+ntopsnew;
    nlinesnew=nlinesold+ntopsdiff;
    l1=l1+2;
    for line=(l1+ntopsnew):nlinesnew
      lasheader_t(line,1:ncharold)=...
           lasheader((line-ntopsdiff),1:ncharold);
    end
  else          %normal case, have tops
		if(flag)
			ind=findstr(lasheader(l1-1,:),'DEPTH');
			if(~isempty(ind))
				lasheader(l1-1,ind:ind+4)=' TIME';
			end
		end
	  for line=1:(l1-1)
		 lasheader_t(line,1:ncharold)=...
				  lasheader(line,1:ncharold);
	  end
    for line=(l2+1+ntopsdiff):nlinesnew
      lasheader_t(line,1:ncharold)=...
           lasheader((line-ntopsdiff),1:ncharold);
    end
  end
%
  if(ntopsnew>0)
    for itop=1:ntopsnew
      line=l1+itop-1;
      str=strunpad(tops(itop,:));
      strtmp=sprintf(' %10.4f:',ztops(itop));
      strnew=[strtmp blank blank blank];
      str = [' ',str,'        .  ',strnew];
      if(size(lasheader_t,2)>length(str))
	str=[str setstr(ones(1,size(lasheader_t,2)-length(str)))];
      end
      lasheader_t(line,:) = str(1:ncharold);
    end
  end
% trim, if necessary
  if(ntopsdiff<0)
    lasheader_t=lasheader_t(1:nlinesnew,:);
  end
lasheader_t=char(lasheader_t);