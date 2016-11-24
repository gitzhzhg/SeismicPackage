function ss=stretchtie(s,t,tmap)
% STRETCHTIE ... stretch (or compress) a trace to achieve a better tie to a reference
% 
% ss=stretchtie(s,t,tmap)
% 
% s ... trace to be adjusted
% t ... time coordinate for trace
% tmap ... nrows by 2 columns defining the time map. Column 1 has times of
%       the imput trace and column 2 gives the corresponding desired output
%       times. Entries in both columns must be monotonically increasing
% 
% ss ... output stretched trace
% **** Run STRETCHTIE without any outputs to cause a diagnostic plot ******
%
% Example:
% part 1 make a synthetic
% dt=.001;
% tmax=1;
% [r,t]=reflec(tmax,dt,.2,5,5);
% [w,tw]=ricker(dt,20,.2);
% s=convz(r,w);
%
% tmap=[.045 .06;.24 .2;.34 .38;.53 .58; .7 .75]; %part 2 define the time map
% 
% ss=stretchtie(s,t,tmap); %part 3: stretch and plot
% figure
% plot(t,s,t,ss+.01);
% legend('before','after')
% grid
%
% The tmap is the key to this. Remember that ; is the row separator. So
% tmap in this example we have 5 rows which means we have 5 defined tie
% points. The first number in each row is the time of an event on input and
% the second number is the time that it will be on output. Study the
% example and observe how the result follows from the input given the tmap.
%
% by G.F. Margrave, 2014
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


%examine tmap
test=diff(tmap(:,1));
ind=find(test<0, 1);
if(~isempty(ind))
    error('first column of tmap is not monotonically increasing');
end
test=diff(tmap(:,2));
ind=find(test<0, 1);
if(~isempty(ind))
    error('second column of tmap is not monotonically increasing');
end
if(tmap(1,1)~=0)
    tmap=[0.0, tmap(1,2)-tmap(1,1);tmap];
end
if(tmap(end,1)<t(end))
    tmap=[tmap;t(end), tmap(end,2)+t(end)-tmap(end,1)];
end

%determine interpolation sites
t_int=interp1(tmap(:,2),tmap(:,1),t);

%sinc interpolate
ind=between(t(1),t(end),t_int,2);
ss=zeros(size(s));
ss(ind)=sinci(s,t,t_int(ind));

if(nargout==0)
    figure
    inc=max(abs(s));
    hh=plot(t,s,t,ss+inc);
    for k=1:size(tmap,1)
        i1=near(t,tmap(k,1));
        i2=near(t,tmap(k,2));
        t1=t(i1);
        t2=t(i2);
        y1=s(i1);
        y2=ss(i2)+inc;
        h=line([t1 t2],[y1 y2],'color','k','marker','*');
    end
    legend([hh; h],'input','output','tiepoints')
    title({'results of stretchtie';'t=0 and t=end tie points added if not supplied'})
end