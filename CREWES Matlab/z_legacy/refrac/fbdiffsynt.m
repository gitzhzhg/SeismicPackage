%function= fbdiffsynt(action)
%figure 
%load_data=uimenu(gcf,'Label','Load data','callback',loadsynt);
%display_fb=uimenu(gcf,'Label','Display FB_picks','callback','fbdiffsynt(''init''));
%if(nargin<1) action = 'init';end
%if(strcmp(action,'init'))
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

	%first break vs shot & rec location along the survey line
	nshots=input('What is the number of shots?  ');
	nrecs=input('What is the number of receivers?  ');
	figure('menubar','none');
	hold;
	for i=1:nshots;
	  plot(xcoord(i,:),fbpicks(i,:));
	end;
	xlabel('xcoordinate (m)');
	ylabel('traveltime (ms)');
	title('Refracted P-wave picks for shots #1-101');
%	uimenu
%end
%if(strcmp(action,'step1'))
	%check reciprocal time
	diffmat = rectimesynt(nshots, nrecs, xcoord, fbpicks, shotcoord);
	absdiffmat=abs(diffmat);
	figure('menubar','none');
	hold;
	    for i=1:nshots-1;
		plot(absdiffmat(i,:));
	    end;
	xlabel('xcoordinate');
	ylabel('Reciprocal traveltime difference (ms)');
	title('Reciprocal time check ')
%	reciprocal time bigger than 5 ms
	[s1,s2]=find(absdiffmat>5);
	m=size(s1);
	figure('menubar','none');
	for k=1:m; plot(s2,s1,'x'); end
	xlabel('shot number');
	ylabel('shot number');
	title('Reciprocal time check, shot pairs with over 5 ms of difference ')
%end
%if(strcmp(action,'step2'))
	%Traveltime difference between two shot (askthing.m...)
	j =input('What is the first shot number (smallest)?  ');
	i =input('What is the second shot number (largest)?  ');
	step = 1;%interpolated step
	[start1, end1, tdiff1, start2, end2, tdiff2] = shotsubsynt(i,j,shotcoord,xcoord,fbpicks,step);
	
	figure('menubar','none');
	hold;
	plot(xcoord(i,:),fbpicks(i,:));
	plot(xcoord(j,:),fbpicks(j,:));
	xlabel('xcoordinate (m)');
	ylabel('traveltime (ms)');
	title('Refracted P-wave picks for the two shots');
%end
%if(strcmp(action,'step3'))
	figure('menubar','none'); %Traveltime difference between two shot
	x1 = start1:step:end1;
	plot(x1,tdiff1,'y'); hold on;
	x2 = start2:step:end2;
	plot(x2, tdiff2, 'g'); hold off;
	xlabel('xcoordinate(m)');
	ylabel('traveltime difference(ms)');
	title('Traveltime difference between the two shots ');
%end
%if(strcmp(action,'step4'))
	%Median filter
	window=input('What is the median filter window length (odd numbers)?  ');
	mddiff3=medfilt1(tdiff1,window);
	mddiff4=medfilt1(tdiff2,window);
	figure('menubar','none');
	plot(x1,mddiff3,'y');hold on;
	plot(x2,mddiff4,'g');hold off;
	xlabel('xcoordinate');
	ylabel('traveltime difference median filtered(ms)');
	title('Median filter of the traveltime difference ');
%end
%if(strcmp(action,'step5'))
	%Derivative of the traveltime difference
	clear delt1
	clear dt1
%	callback(fbdiffsynt,''step4'')
	[m,n]=size(x1);
	if( size(mddiff3) ~= 0 )
	  delt1=mddiff3(1:n-2)-mddiff3(3:n);
	  avgmdx1=(x1(1:n-2)+x1(3:n))/2;
	  dt1 = clip(delt1,1);
	else
	  avgmdx1 = [];
	end
	clear delt2
	clear dt2
	[m,n]=size(x2);
	if( size(mddiff4) ~= 0 )
	  delt2=mddiff4(1:n-2)-mddiff4(3:n);
	  avgmdx2=(x2(1:n-2)+x2(3:n))/2;
	  dt2 = clip(delt2,1);
	else 
	  avgmdx2 = [];
	end
	figure('menubar','none');
	plot(avgmdx1,delt1,'y');hold on;
	plot(avgmdx2,delt2,'g');hold off;
	xlabel('xcoordinate (m)')
	ylabel('derivative of the time difference')
	title('First derivative of the median filtered time difference')
	figure('menubar','none');
	plot(avgmdx1,dt1,'y');hold on;
	plot(avgmdx2,dt2,'g');hold off;
	xlabel('xcoordinate (m)')
	ylabel('derivative of the time difference')
	title('First derivative of the median filtered time difference')
%end
%if(strcmp(action,'step6'))
	% Second derivative of the traveltime difference
%	callback(fbdiffsynt,''step5'')
	clear delts1
	clear dts2
	[m,n]=size(avgmdx1);
	if( size(mddiff3) ~= 0 )
	  delts1=delt1(1:n-2)-delt1(3:n);
	  avgmdxs1=(avgmdx1(1:n-2)+avgmdx1(3:n))/2;
%	  dts1 = clip(delts1,1);
	else
	  avgmdxs1 = [];
	end
	clear delts2
	clear dts2
	[m,n]=size(avgmdx2);
	if( size(mddiff4) ~= 0 )
	  delts2=delt2(1:n-2)-delt2(3:n);
	  avgmdxs2=(avgmdx2(1:n-2)+avgmdx2(3:n))/2;
%	  dts2 = clip(delts2,1);
	else 
	  avgmdxs2 = [];
	end
	figure('menubar','none');
	plot(avgmdxs1,delts1,'y');hold on;
	plot(avgmdxs2,delts2,'g');hold off;
	xlabel('xcoordinate (m)')
	ylabel('derivative of the time difference')
	title('Second derivative of the median filtered time difference')
%	figure;
%	plot(avgmdxs1,dts1,'y');hold on;
%	plot(avgmdxs2,dts2,'g');hold off;
%	xlabel('xcoordinate (m)')
%	ylabel('derivative of the time difference')
%	title('Second derivative of the median filtered time difference')
%end
%if(strcmp(action,'step7'))
	%Autopicking of crossoverpoint
	cvpj=zeros(nshots-1,nshots-1);
	cvpi=zeros(nshots-1,nshots-1);
	cvpj=NaN;
	cvpi=NaN;
	for j=1:nshots-1;       % j is the first shot (t1)
		for i=j+1:nshots;    % i is the 2nd shot (t2)
		%	callback(fbdiffsynt,''step5'')
			if( size(mddiff3) ~= 0 )
				a=max(delts1);
				b=find(delts1>max(delts1)-0.000001);
				cvpj(i,j)=avgmdxs1(k);
			end
			if( size(mddiff4) ~= 0 )
				c=max(delts2);
				d=find(delts2>max(delts2)-0.000001);
				cvpi(i,j)=avgmdxs1(d);
			end
		end
	end
	figure('menubar','none');
	hold;
	for i=1:nshots;
	  plot(xcoord(i,:),fbpicks(i,:));
	end;hold on;
	xlabel('xcoordinate (m)');
	ylabel('traveltime (ms)');
	title('Refracted P-wave picks for shots #1-101 with CVP');
%end
%if(strcmp(action,'step8'))
%	edit=input('Do you wish to edit the crossoverpoint(y/n)?  ','s');
%	if(strcmp(edit,'y'))
	%if edit=1
%		gcf=input('In which figure number you want to edit ?');
%		editcpoint
%		return
%	else
%		return;
%	end
%end
%if(strcmp(action,'step8'))
%	sav=input('Do you wish to save your crossoverpoints (yes=1 or no=0)?';
%	if sav=1
%		crossoverpt=editcpoint('save');
%		[u,v]=size(crossoverpt);	
%		for i=1:v
%			crossover(i)=get(crossoverpt(i),'xdata')
%		end
%	else
%		return
%end
%figure;
%hold;
%    for i=1:188;
%	plot(abstdiff(i,:));
%    end;
%xlabel('shot number');
%ylabel('traveltime difference(ms)');
%title('Traveltime difference ');
%tic
%[m,n]=size(tdiff1);
%for a=1:m-window;
%   mddiff1(a)=median(tdiff1(a:a+window));
%   mdloc(a)=median(x1(a:a+window));
%end
%[m,n]=size(tdiff2);
%for a=1:m-window;
%   mddiff2(a)=median(tdiff2(a:a+window));
%   mdlc(a)=median(x2(a:a+window));
%end
%toc
%test=sum(abs(mddiff3-mddiff1));
%disp([' Test sum =' num2str(test)])%figure;
%plot(mdloc,mddiff1,'y');hold on;
%plot(mdlc,mddiff2,'g');hold off;
end