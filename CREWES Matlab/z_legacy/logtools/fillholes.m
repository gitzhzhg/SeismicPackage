function [lout,zout]=fillholes(lin,flag,nave,z,x,hors)
% [lout,zout]=fillholes(lin,flag,nave,z,x,hors)
% [lout,zout]=fillholes(lin,flag,nave)
% [lout,zout]=fillholes(lin,flag)
%
% FILLHOLES provides a simple mechanism fo filling in the holes in logs
% computed by LOGSEC prior to synthetic seismogram computation. The 
% flag argument provides control over which of several possible mechanisms
% is invoked.
%
% lin = input log
% flag = 'constant' ... fill in holes with a constant whose value is the
%        mean value of the 'nave' samples just before and just
%        after the hole.
% flag = 'linear' ... fill in holes with a linear trend from the
%        'nave' live sample just before the hole to the 'nave'
%        live ones after.
% flag = 'mean' ... fill in holes with the mean values of the live
%	 samples on the log. (All holes then get filled with
%        the same value)
% flag = 'layermean' ... fill in holes with the mean value of the
%        live samples in the layer in which the hole occurs.
%        Empty layers will get filled with the mean value of the
%        layers above and below the layer.
% flag = 'layertrend' ... fill in holes with a linear trend fit to
%        the live samples in a particular layer. Empty layers
%        will get filled with a trend derived from the layers
%        just above and below the layer.
%        NOTE: you may also use the integers 1->5 for flag
%
% nave = averaging distance over which samples will be averaged for 
%			flag='constant' and flag='linear'
% *************** default = 10 samples  *************
% z = vertical coordinate vector for the input log
% ****required only if flag ='layermean' or flag='layertrend' *********
% x = the x coordinate (a scaler) giving the inline position of the log
% **** required only if flag ='layermean' or flag='layertrend' *********
% hors = graphics handles of the horizons forming the cross
%		section over which the logs were propagated. If supplied, care
%		must be taken to ensure	the horizons are plotted in the same
%		units as the vertical coordinate z
%		If hors is a single scalar integer, then it is assumed to
%		be the figure
%		number of a LOGSEC window displaying the horizons.
% *** required only if flag ='layermean' or flag='layertrend' *********
% lout = output log with holes filled
% zout = output z coordinate. May differ from the input z if the log
%	extends beyond the first or last horizon. Such samples are rejected.
%	(only applied for the last two cases)
%
% G.F. Margrave, March 1994
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
% set defaults
 if(nargin<4)
		z=1:length(lin);
		[m,n]=size(lin);
		if(n==1) z=z'; end
	end
	if( nargin< 3)
		nave=10;
	end
	% make sure we have a column vector
	[m,n]=size(lin);
	if( m~=1 & n~= 1)
		error('input must be a vector');
	end
	if(n~=1)
		lin=lin(:);
		if(nargin>3)
			z=z(:);
		end
	end
	
	if(~isstr(flag))
		if(flag==1) flag='constant';
		elseif(flag==2) flag='linear';
		elseif(flag==3) flag='mean';
		elseif(flag==4) flag='layermean';
		elseif(flag==5) flag='layertrend';
		else
			error('incorrect value for flag');
		end
	end
%
% determine where the holes are
%
	inan=isnan(lin);
	idead=find(inan);
	if(isempty(idead))
		lout=lin;
		zout=z;
		return;
	end
	ind=find(diff(idead)>1);
	hole_beg=[idead(1); idead(ind+1)];
	hole_end=[idead(ind); idead(length(idead))];
	nholes=length(hole_beg);
	%
	% initialize the output trace with the input
	%
		lout=lin;
		if(nargin>3)
			zout=z;
		else
			zout=[];
		end
		nsamp=length(lin);
	%
	%branch according to flag
	%
	if( strcmp(flag,'constant') | strcmp(flag,'linear') )
		if(strcmp(flag,'constant') )
			constant=1;
		else
			constant=0;
		end
		%loop over holes
		for k=1:nholes
			
			%determine average sample values just before and after the hole
			% This requires some careful logic to make sure we average over the
			% correct samples
			iave1=hole_beg(k)-nave;
			iave2=hole_end(k)+nave;
			if(iave1<1) iave1=1; end
			if(iave2>nsamp) iave2=nsamp; end
			if( k > 1)
				if( iave1 <= hole_end(k-1) )
					iave1 = hole_end(k-1)+1;
				end
			end
			if( k < nholes )
				if( iave2 >= hole_beg(k+1) )
					iave2=hole_beg(k+1)-1;
				end
			end
			if( iave1< hole_beg(k) )
				ave1= sum(lin(iave1:hole_beg(k)-1))/length(1:hole_beg(k)-iave1);
			else
				ave1=[];
			end
			if( iave2 > hole_end(k) )
				ave2= sum(lin(hole_end(k)+1:iave2))/length(1:iave2-hole_end(k));
			else
				ave2=[];
			end
			%ok, the average values are determined, now branch according to
			%constant and compute the fillvalues
			if(constant)
				
				w1=1;w2=1;
				if(isempty(ave1)) ave1=0.0; w1=0.; end
				if(isempty(ave2)) ave2=0.0; w2=0.; end
				fillvalues= (ave1+ave2)/(w1+w2)*ones(size(hole_beg(k):hole_end(k)));
			else
				% we do a linear trend from ave1 @ hole_beg to ave2 @ hole_end
				if(isempty(ave1)) ave1=ave2; end
				if(isempty(ave2)) ave2=ave1; end
				hole=1:(hole_end(k)-hole_beg(k)+1);
				holesize=length(hole);
				if( holesize > 1 )
					fillstep=(ave2-ave1)/(holesize-1);
				else
					fillstep=0;
				end
				fillvalues = ave1+ (hole-1)*fillstep;
			end
			% fill the hole
			lout(hole_beg(k):hole_end(k))= fillvalues;
		end
		%transpose back to a row vector if that was supplied
		if( n~= 1)
			lout=lout';
		end
		return;
	end
	if(strcmp(flag,'mean'))
		% determine the logs mean value
		logmean=mean(lin(~inan));
		%loop over holes
		for k=1:nholes
			lout(hole_beg(k):hole_end(k))=logmean*ones(size(hole_beg(k):hole_end(k)));
		end
		%transpose back to a row vector if that was supplied
		if( n~= 1)
			lout=lout';
		end
		return;
	end
	if(strcmp(flag,'layermean') | strcmp(flag,'layertrend') )
		if(nargin~=6)
			error('depth and horizon information required');
		end
		if(length(hors)==1)
			h=get(hors,'userdata');
			hset=get(h(16),'userdata');
			hors=objget(hset,'handlevector');
		end
		%get the layer depths
		zlayers=horizonz(hors,x);
		%sort into order
		zlayers=zlayers(~isnan(zlayers));
		zlayers=sort(zlayers);
		nlayers=length(zlayers)-1;
		%compute the start and end indicies of each layer
		dz=z(2)-z(1);
		lay_beg=ceil((zlayers(1:nlayers)-z(1))/dz + 1);
		lay_end=floor((zlayers(2:nlayers+1)-z(1))/dz +1);
		ind=find(lay_beg<=0);
		if(~isempty(ind))
			lay_beg(ind)=ones(size(ind));
		end
		ind=find(lay_beg>nsamp);
		if(~isempty(ind))
			lay_beg(ind)=nsamp*ones(size(ind));
		end
		ind=find(lay_end<=0);
		if(~isempty(ind))
			lay_end(ind)=ones(size(ind));
		end
		ind=find(lay_end>nsamp);
		if(~isempty(ind))
			lay_end(ind)=nsamp*ones(size(ind));
		end
		%loop over holes
		if( strcmp(flag,'layertrend') )
			trend=1;
		else
			trend=0;
		end
		for k=1:nholes
			%start and end of the hole
			zholebeg=(hole_beg(k)-1)*dz+z(1);
			zholend=(hole_end(k)-1)*dz+z(1);
		
			%determine which layers are involved with this hole
			ilay1=surround(zlayers,zholebeg);
			ilay2=surround(zlayers,zholend);
			if(isempty(ilay1))
				ilay=ilay2;
			elseif(isempty(ilay2))
				ilay=ilay1;
			else
				ilay=ilay1:ilay2;
			end
			%ilay1=between(hole_beg(k),hole_end(k),lay_beg,2);
			%if(ilay1==0) ilay1=[];end
			%ilay2=between(hole_beg(k),hole_end(k),lay_end,2);
			%if(ilay2==0) ilay2=[];end
			%ilay=union(ilay1,ilay2);
			%ilay=sort(ilay);
			%ok loop over the layers that are involved
			for kk=ilay
				% see if the layer has live samples
				loglay=lin(lay_beg(kk):lay_end(kk));
				zlay=z(lay_beg(kk):lay_end(kk));
				ilive= find(~isnan(loglay));
				ihole=between(zholebeg,zholend,zlay,2);
				%simple section for layer with live samples
				if(~isempty(ilive))
					
					%compute mean or trend
					if( trend )
						
						c=polyfit(zlay(ilive),loglay(ilive),1);
						fillvalues=polyval(c,zlay(ihole));
					else
						laymean=mean(loglay(ilive));
						fillvalues=laymean*ones(size(ihole));
					end
					lout(lay_beg(kk)+ihole-1)=fillvalues;
				else %if layer is dead, then we average results from the first
				%layers above and below with live samples
					%find the first layers both above and below with live samples
					%above
					layup=k-1;
					cup=[];meanup=[];
					while(layup>0)
						layerup=lin(lay_beg(layup):lay_end(layup));
						zup=z(lay_beg(layup):lay_end(layup));
						ilive=find(~isnan(layerup));
						if(~isempty(ilive))
							%compute mean or trend
							if(trend)
								cup=polyfit(zup(ilive),layerup(ilive),1);
							else
								meanup=mean(layerup(ilive));
							end
							layup=-1; %terminate the while loop
						else
							layup=layup-1;
						end
					end
					%below
					laydown=k+1;
					cdown=[];meandown=[];
					while(laydown<=nlayers)
						layerdown=lin(lay_beg(laydown):lay_end(laydown));
						zdown=z(lay_beg(laydown):lay_end(laydown));
						ilive=find(~isnan(layerdown));
						if(~isempty(ilive))
							%compute mean or trend
							if(trend)
								cdown=polyfit(zdown(ilive),layerdown(ilive),1);
							else
								meandown=mean(layerdown(ilive));
							end
							laydown=nlayers+1; %terminate the while loop
						else
							laydown=laydown+1;
						end
					end
					% average the results
					if(trend)
						c=0;count=0;
						if(~isempty(cup))
							c=cup; count=1;
						end
						if(~isempty(cdown))
							c=(c+cdown)/(count+1);
						end
						%compute fill
						fillvalues=polyval(c,zlay(ihole));
					else
						laymean=0;count=0;
						if(~isempty(meanup))
							laymean=meanup;count=1;
						end
						if(~isempty(meandown))
							laymean=(laymean+meandown)/(count+1);
						end
						%compute fill
						fillvalues=laymean*ones(size(ihole));
					end
					lout(lay_beg(kk)+ihole-1)=fillvalues;
				end 
			end %end loop over layers
		end %end loop over holes
		%reject any samples lieing outside the layers
		ind=between(zlayers(1),zlayers(nlayers),zout,2);
		zout=zout(ind);
		lout=lout(ind);
		return;
	end %end this logical section