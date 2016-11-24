function PI_limbox_transforms(arg1,arg2,arg3)
h=get(gcbf,'userdata');
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

% hmsg=h{2};
hi=h{5};
nm=get(gcbo,'label');
% httl=get(gca,'title');  
% ttl=get(httl,'string');
seis=get(hi,'cdata');
% t=get(hi,'ydata');
% x=get(hi,'xdata');

hlimbox=h{14};
limfig=get(hlimbox,'userdata');
limmat=limfig{3};
        
imxdat=get(hi,'xdata'); imydat=get(hi,'ydata');
xdat=find(imxdat>=limmat(3) & imxdat<=limmat(4));
ydat=find(imydat>=limmat(1) & imydat<=limmat(2));
if(isempty(xdat))
    return
end
% dddd=[xdat(1) xdat(end) ydat(1) ydat(end)];
seis=seis(ydat,xdat);
xdat=imxdat(xdat);
ydat=imydat(ydat);
if(isempty(seis))
    return
end
switch nm
    case 'F-K Amp Spectrum'
        [spec,f,kx]=fktran(seis,ydat,xdat);
        plotimage(abs(spec),f,kx);
        mainax=findobj(gcf,'type','axes','tag','MAINAXES');
        mainax_title=get(mainax,'Title');
        mainax_xlabel=get(mainax,'Xlabel');
        mainax_ylabel=get(mainax,'Ylabel');
        set(mainax_title,'String','FK Amplitude Spectra');
        set(mainax_xlabel,'String','Wavenumber');
        set(mainax_ylabel,'String','Wavenumber');
%         mainaxlbl=get(mainax,'Parent');
%         set(mainaxlbl,'Title','test');
        % gtfigs=findobj(0,'type','figure','tag','PLOTIMAGEFIGURE');
        
%         stringinfo1='Generating F-K Amplitude Spectrum';
%         stringinfo2='F-K Amplitude Spectrum';
%         % limiting data that will FK transformed
%         % [spec,f,kx]=fktran(seis,ydat,xdat,2^nextpow2(ydat),2^nextpow2(xdat),20);
%         [spec,f,kx]=fktran(seis,ydat,xdat);
%         %spec = spec./mean(mean(spec));
%         xlbl='Wave Number';
%         ylbl='Frequencey';
%         file=['FK Spectrum of: ' ttl];
%         PI_init_image;
%         h=get(gcf,'userdata');
%         hscale=h{6};
%         hclip=h{7};
%         hmaster=h{10};
%         for ii=1:length(h)
% 	    if(ishandle(h{ii})&ii~=5)
% 	       set(h{ii},'enable','on');
%             end
%         end
%         newim=image(kx,f,abs(spec));
%         colormap('gray');
%         imagetype='Amp Spectrum';    % multiple different image types
    case 'F-X Amp Spectrum'
        [spec,f]= fftrl(seis,ydat);
        plotimage(abs(spec),f,xdat);
        mainax=findobj(gcf,'type','axes','tag','MAINAXES');
        mainax_title=get(mainax,'Title');
        mainax_xlabel=get(mainax,'Xlabel');
        mainax_ylabel=get(mainax,'Ylabel');
        set(mainax_title,'String','FX Amplitude Spectra');
        set(mainax_xlabel,'String','Distance');
        set(mainax_ylabel,'String','Wavenumber');
%         stringinfo1='Generating F-X Amplitude Spectrum';
%         stringinfo2='F-X Amplitude Spectrum';
%         % [spec,f]= fftrl(seis,ydat,10,2);
%         [spec,f]= fftrl(seis,ydat);
%         xlbl='Distance';
%         ylbl='Frequencey';
%         file=['FX Spectrum of: ' ttl];
%         PI_init_image;
%         h=get(gcf,'userdata');
%         hscale=h{6};
%         hclip=h{7};
%         hmaster=h{10};
%         for ii=1:length(h)
% 	    if(ishandle(h{ii})&ii~=5)
% 	       set(h{ii},'enable','on');
%             end
%         end
%         spec=abs(spec);
%         ky=(1:1:size(spec,2));
%         newim=image(ky,f,spec);
%         colormap('gray');
%         imagetype='Amp Spectrum';    % multiple different image types
end
% set(hmsg,'string',stringinfo1,'backgroundcolor',[1 1 1]);
% hmsg=h{2};
% set(hmsg,'string',stringinfo2,'backgroundcolor',[1 1 1]);
% set(gca,'xaxislocation','bottom');
% gtfigs=findobj(0,'type','figure','tag','PLOTIMAGEFIGURE');
% nm=1;
% xxnm=1;
% adon='';
% for ii=1:length(gtfigs)
%     haxs=get(gtfigs(ii),'currentaxes');
%     ttl=get(haxs,'title');
%     dat=get(ttl,'userdata');
%     if(~isempty(dat))
%         xfile=dat{1};
%         xnm=dat{2};
%         if(strcmp(xfile,file));
%             nm=nm+1;
%             if(xnm>=nm)
%                 nm=xnm+1;
%             end
%             adon=['(' num2str(nm) ')'];
%         end 
%     end
% end
% title([file adon],'tag','PLOTIMAGE-TITLE','fontweight','bold',...
%     'userdata',{file nm imagetype},'interpreter','none');
% xlabel(xlbl,'horizontalalignment','right');
% ylabel(ylbl,'tag','PLOTIMAGE-YLABEL');
% h=get(gcf,'userdata');
% h{5}=newim;
% h{10}=hmaster;
% set(hmaster,'visible','off');
% mxs=full(max(max(abs(spec))));
% %determine clipping
% smean=full(mean(mean(spec)));
% stddev=full(sqrt( sum(sum((spec-smean).^2 ) )...
%     /prod(size(spec))));
% clip=4;
% smean2=smean;
% mxs2=mxs;
% stddev2=stddev;
% scaleopt=1;
% if(~isnan(clip))
%     mxsprime=min([smean2+clip*stddev2,mxs2]);
% end
% mns=-mxsprime;
% mns2=mns;
% cm=get(gcf,'colormap');
% [nkols,m]=size(cm);
% seis = (spec-mns)/(mxsprime-mns)*(nkols-1)+1;
% clear smat
% ampflag=2;
% set(h{6},'userdata',[scaleopt mxs2 mns2 smean2 stddev2]);
% set(h{10},'userdata',[mxs smean stddev]);
% set(gcf,'userdata',h);