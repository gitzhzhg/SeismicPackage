function trout=gain(trin,t,atten,tmin,tmax)
% trout=gain(trin,t,atten,tmin,tmax)
% trout=gain(trin,t,atten)
% gain(trin,t,0,tmin,tmax)
%
% GAIN applies exponential gain to the input trace, trin. 
%  If atten is negative, the exponential decay will result.
% GAIN with no return variable assigned creates an interactive
% analysis plot to allow the estimation of gain parameters. In this
% case, atten is not used.
%
% trin= input trace
% t= input time coordinate vector
% atten= exponential gain constant in db/sec
% tmax= time at which applied gain ceases to be exponential
%       and simply becomes constant
%   ********** default = t(length(t)) ***********
% tmin = start time for gain corrections
%   ********** default = 0.0 *********
% trout = output trace with gain applied
%
% by G.F. Margrave, May 1991
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
%test for dbanal curve
if(isstr(trin))
	if(strcmp(trin,'dbcurve') )
		dat=get(gcf,'userdata');
		if( isempty(dat))
			pt=get(gca,'currentpoint');
			pt=pt(1,1:2);
			set(gcf,'userdata',pt);
			title('Click again');
			return;
		end
		pt1x=dat(1); pt1y=dat(2);
		pt=get(gca,'currentpoint');
		pt=pt(1,1:2);
		m=(pt(2)-pt1y)/(pt(1)-pt1x);
		x=[pt1x pt(1)];
		y=[pt1y pt(2)];
		line('xdata',x,'ydata',y,'color','r','linestyle','-.');
		htxt=text(pt(1),pt(2),[num2str(abs(m)) ' db/sec']);
		set(htxt,'color','r');
		set(gcf,'userdata',[]);
		title('Click twice to define db/sec curve');
		xlabel([' Chosen atten = ' num2str(abs(m))])
		return;
	end
end
 
% set defaults
 if nargin<5
   tmax=t(length(t));
 end
 if nargin<4
   tmin=t(1);
 end
 if(tmin>=tmax)
		error(' tmin and tmax specified incorrectly ');
	end
 if(nargout==0) %create gain analysis plot
		trinew=padpow2(trin,0);
		env=abs(hilbm(trinew));
		env=env(1:length(trin));
		env=todb(env);
		figure;
		iz=near(t,tmin,tmax);
		plot(t,env);
		ylabel('Envelope Amplitude (db)');
		p=polyfit(t(iz),env(iz),1);
		fit=p(1)+p(2)*t(iz);
		hfit=line('xdata',t(iz),'ydata',fit,'color','r','linestyle','-.');
		htxt=text(t(iz(length(iz))),fit(length(fit)),...
			[num2str(abs(p(2))) ' db/sec']);
		set(htxt,'color','r');
		set(gcf,'windowbuttondownfcn','gain(''dbcurve'')');
		title('Click twice to define db/sec curve');
		xlabel(['Least sqs fit gives atten = ',num2str(abs(p(2)))]);
		set(gcf,'userdata',[]);
		grid
		return;
	end
% compute gain trace
 alpha=log(10)*atten/20.;
 iz=near(t,tmin,tmax);
 tgain=ones(size(t));
 tgain(iz)=exp(alpha*(t(iz)-tmin));
 tgain(iz(length(iz))+1:length(t))=tgain(iz(length(iz)))*...
		ones(size(iz(length(iz))+1:length(t)));
% apply gain
 trout=trin.*tgain;