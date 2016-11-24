%define the synthetic to be deconvolved
%A random reflectivity is generated with "reflec". The frist time you run
%this script in a session, the synthetic attenuated trace is built. All
%subsequent times, the existing trace is used. This allows the Gabor
%parameters to be studied against the same underlying reflectivity.  The
%script checks to see if the variable rq exists, which is the random
%reflectivity. If so, then a new synthetic is not made regardless of what
%you may have changed q or tmax to. To force a new synthetic to be made,
%you must clear the variable rq: >>clear rq
%If you wish to terminate your Matlab session and restart later with the
%same synthetic, just use the save command to save your workspace and then
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

%load it in the new Matlab session before running this script
dt=.002;tmax=2;q=50;

iburg=0;%set to zero for Fourier algorithm... Burg not currently recommended
iwiener=1;%set to 1 for an AGC-WIENER-AGC comparison, leave this alone or risk trouble
iopt=1;%1 means analysis windows, 2 is synthesis, 3 is both, leave this at 1
ibigfigs=0;% set to zero to turn off big figures

idemo=0;%demo forward and inverse gabor
idecon=1;%deconvolve or not
ibandlimit=1;% 0  means don't filter after decon, 1 means filter
igaborfact=0;% set to zero to turn off plotting of the factors of the input Gabor spectrum

%gabordecon parameters, type >>help gabordecon 
%for more information
twin=.2;tinc=.01;%defines the Gaussian windows
gdb=inf;%truncation factor in fgabor
pow2option=1;%make windows a power of 2 in length
%the next three parameters define the smoothing operation on the Gabor
%spectrum of the input signal
tsmo=1;%temporal smoother in seconds
fsmo=10;%frequency smoother in Hz
ihyp=1;%flag for hyperbolic smoothing. 1 gets Hyperbolic, 0 gets boxcar
%Define the p parameter that controls the tradeoff between analysis and
%synthesis windows. p=1 gets all analysis (preferred) p=0 gets all
%synthesis (bad)
p=1; %Leave this at 1
%
stabg=.00001;phase=1;%gabor decon stab factor and phase flag
order=10;%order of the Burg spectrum if iburg is 1

%wiener decon parameters
stabw=.00001;%stability constant
operator_length=100;%operator length in samples

transforms=1;
% transforms ... must be one of [1,2,3] with the meaning
%               1 : use old Gabor transforms without normalization
%               2 : use old Gabor transforms with normalization
%               3 : use new Gabor transforms without normalization
%               4 : use new Gabor transforms with normalization
% ************* default = 3 ************



%plotflags
plottraces=1; %plot time domain traces
plotfourier=0; %plot Fourier amplitude spectra of traces
gaborplot=1;% set to zero to turn off all ploting of Gabor spectra regardless of the next flags
plotgab_input=0; %plot Gabor spectrum of input (attenuated) signal
plotgab_wavelet=0; %plot Gabor spectrum of estimated wavelet
plotgab_refl=0; %plot Gabor spectrum of actual reflectivity
plotgab_refl_est=1; %plot Gabor spectrum of reflectivity estimate
plotgab_wiener=0;%plot Gabor spectrum of Wiener estimate of reflectivity

if(iburg)
    titalg=' Burg,';
else
    titalg=' Fourier,';
end
titwin='';
% if(iopt==1)
%     titwin=' analysis,';
% elseif(iopt==2)
%     titwin=' synthesis,';
% elseif(iopt==3)
%     titwin=' analysis and synthesis,';
% end

if(~exist('rq'))
    [rq,t]=reflec(tmax,dt,.2,3,3);
    [w,tw]=wavemin(dt,20,tmax/10);
    qmat=qmatrix(q,t,w,tw);
    %s=convm(r,w);
    sq=qmat*rq;
end

if(idemo) %demo forward and inverse transforms
    normflag=0;
   [sqtvs,trow,fcol]=fgabor(sq,t,twin,tinc,p,gdb,normflag);

   sqq=igabor(sqtvs,trow,fcol,twin,tinc,p,gdb,normflag);
   
   sqq=sqq(1:length(sq));%unpad
   
   figure;
   inc=.8*max(sq);

   subplot(2,1,1)
   plot(t,sq,t,sqq+inc);
   xlabel('Time in seconds')
   title('signal reconstruction after forward and inverse gabor')
   legend('Attenuated signal','After forward and inverse Gabor');
   subplot(2,1,2)
   plot(t,sq-sqq);
   title('difference')
%     nudge=-.1*inc;
%     start=.5*tmax;
%     text(start,nudge,'Attenuated signal');
%     text(start,nudge+inc,'After forward+inverse Gabor');
%     text(start,nudge+2*inc,'Difference');
%     yoff
    
    if(ibigfigs)
        bigfig;whitefig;boldlines(gca,6);bigfont;
    end
end

if(idecon)
    if(~iburg)
       [r2,tvs_op]=gabordecon(sq,t,twin,tinc,tsmo,fsmo,ihyp,stabg,phase,p,gdb,transforms);
    else
       [r2,tvs_op]=gabordeconb(sq,t,twin,tinc,tsmo,fsmo,ihyp,order,stabg,phase,p,gdb);
       %r2=gabordeconbq(sq,t,twin,tinc,tsmo,fsmo,10,stab,phase,q,iopt);
    end


    if(iwiener)
        % wiener for comparison
        izone=near(t,.3*tmax,.7*tmax);
        sqa=aec(sq,t,tmax/3);
        r3=deconw(sqa,sqa(izone),operator_length,stabw);
        r3=aec(r3,t,tmax/3);
        r3=balans(r3,rq);
    end

    fnyq=1/(2*dt);
    fmax=fnyq/2;
    fwid=fnyq/20;
    if(plottraces)
        if(ibandlimit==1)
            r2f=filtf(r2,t,[0 0],[fmax fwid]);
            r2f=balans(r2f,rq);
            rqf=filtf(rq,t,[0 0],[fmax fwid]);
            rqf=balans(rqf,rq);
            r3f=filtf(r3,t,[0 0],[fmax fwid]);
            r3f=balans(r3f,rq);
        else
            r2f=balans(r2,rq);
            rqf=rq;
            r3f=balans(r3,rq);
        end
        figure;
        %subplot(2,1,1)
        sq2=balans(sq,rq);
        inc=.2;
%         plot(t,sq2,'b',t,r3f+inc,'g',t,r2f+2*inc,'r',t,rqf+3*inc,'k')
%         xlabel('Time in seconds')
%         title([titalg titwin 'twin=' num2str(twin) ...
%                 ' tinc=' num2str(tinc) ' tsmo=' num2str(tsmo) ...
%                 ' fsmo=' num2str(fsmo) ' ihyp=' num2str(ihyp) ...
%                 ' stab=' num2str(stabg)])
        plot(sq2,t,'b',r3f+inc,t,'g',r2f+2*inc,t,'r',rqf+3*inc,t,'k');flipy
        ylabel('Time in seconds')
        nudge=.05;
        start=.5*tmax;
        text(nudge,start,'Attenuated signal','rotation',-90);
        text(nudge+inc,start,'After AGC+Wiener','rotation',-90);
        text(nudge+2*inc,start,['After Gabor ' titalg(1:end-1)],'rotation',-90);
        text(nudge+3*inc,start,'True Reflectivity','rotation',-90);
        xoff
        if(ibandlimit)
            title(['Estimates bandlimited to ' num2str(fmax) ' Hz']);
        else
            title(['Broadband estimates']);
        end

        if(ibigfigs)
                bigfig;whitefig;boldlines(gca,6);bigfont;
        end
    end
    %subplot(2,1,2)
    if(plotfourier)
        figure
        [R,f]=fftrl(rq2,t);
        R2=fftrl(r2,t);
        R3=fftrl(r3,t);
        SQ=fftrl(sq2,t);
        plot(f,abs(SQ)/max(abs(SQ)),'b',f,abs(R3)/max(abs(R3))+1,'g',f,abs(R2)/max(abs(R2))+2,'r',f,abs(R)/max(abs(R))+3,'k')
        xlabel('Frequency in Hz')
        title([titalg titwin 'twin=' num2str(twin) ...
                ' tinc=' num2str(tinc) ' tsmo=' num2str(tsmo) ...
                ' fsmo=' num2str(fsmo) ' stab=' num2str(stab)])
        nudge=.2;
        start=.6*fnyq;
        text(start,nudge,'Attenuated signal');
        text(start,nudge+1,'After AGC+Wiener+AGC');
        text(start,nudge+2,['After Gabor ' titalg(1:end-1)]);
        text(start,nudge+3,'Bandlimited reflectivity');
        yoff

        if(ibigfigs)
                bigfig;whitefig;boldlines(gca,6);bigfont;
        end
    end

    %make some gabor plots
    if(gaborplot==1)
       if(plotgab_input)

           [sqtvs,trow,fcol]=fgabor(sq,t,twin,tinc,1,gdb);
           plotimage(abs(sqtvs),trow,fcol);
           title('Attenutated signal');
           xlabel('Frequency in Hz');ylabel('Time in seconds')
           if(ibigfigs)
            bigfig;whitefig;boldlines;bigfont;
           end
       end
       if(plotgab_wavelet)
           if(~exist('trow'))
               [sqtvs,trow,fcol]=fgabor(sq,t,twin,tinc,1,gdb);
           end
           plotimage(1./abs(tvs_op),trow,fcol);
           title('Propagating wavelet');
           xlabel('Frequency in Hz');ylabel('Time in seconds')
           if(ibigfigs)
            bigfig;whitefig;boldlines;bigfont;
           end
       end

       if(plotgab_refl)

           [rqtvs,trow,fcol]=fgabor(rq,t,twin,tinc,1,gdb);
           plotimage(abs(rqtvs),trow,fcol);
           title('Reflectivity');
           xlabel('Frequency in Hz');ylabel('Time in seconds')
           if(ibigfigs)
            bigfig;whitefig;boldlines;bigfont;
           end
       end
       if(plotgab_refl_est)

           [r2tvs,trow,fcol]=fgabor(r2,t,twin,tinc,1,gdb,pow2option);
           plotimage(abs(r2tvs),trow,fcol);
           if(iburg)
            title({'After Gabor Decon (Burg)';['twin=' num2str(twin) ...
                ',tsmo=' num2str(tsmo) ',fsmo=' num2str(fsmo) ',stabg=' num2str(stabg) ...
                'ihyp=' int2str(ihyp) ]});
           else
             title({['After Gabor Decon (Fourier), tranforms=' int2str(transforms)];['twin=' num2str(twin) ...
                ',tsmo=' num2str(tsmo) ',fsmo=' num2str(fsmo) ',stabg=' num2str(stabg) ...
                ',ihyp=' int2str(ihyp)  ]});
           end
           xlabel('Frequency in Hz');ylabel('Time in seconds')
           if(ibigfigs)
            bigfig;whitefig;boldlines;bigfont;
           end
       end

       if(plotgab_wiener)

           [r3tvs,trow,fcol]=fgabor(r3,t,twin,tinc,1,gdb);
           plotimage(abs(r3tvs),trow,fcol);
           title('After AGC+Wiener Decon');
           xlabel('Frequency in Hz');ylabel('Time in seconds')
           if(ibigfigs)
            bigfig;whitefig;boldlines;bigfont;
           end
       end

     end

     if(igaborfact)
       alpha=exp(-pi*trow(:)*fcol/q);
       plotimage(alpha,trow,fcol);
       title('Constant Q surface');
       xlabel('Frequency in Hz');ylabel('Time in seconds')
       if(ibigfigs)
        bigfig;whitefig;boldlines;bigfont;
       end
       [W,fw]=fftrl(pad_trace(w,padpow2(sq)),t);
       plotimage(ones(length(trow),1)*abs(W'),trow,fw);
       title('Wavelet surface');
       xlabel('Frequency in Hz');ylabel('Time in seconds')
       if(ibigfigs)
        bigfig;whitefig;boldlines;bigfont;
       end

       plotimage((ones(length(trow),1)*abs(W')).*alpha.*abs(rqtvs),trow,fw);
       title('Gabor spectrum model');
       xlabel('Frequency in Hz');ylabel('Time in seconds')
       if(ibigfigs)
        bigfig;whitefig;boldlines;bigfont;
       end

    end
end