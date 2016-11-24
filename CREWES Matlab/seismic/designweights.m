function w=designweights(t,t0s,transpct)
%design a set of raised cosine weights to transition between a set of reference times
%
transpct=transpct/100;
nw=length(t0s);
nt=length(t);
w=zeros(nt,nw);

for k=1:nw
    if(k==1)
        ind=t<=t0s(k);
        w(ind,k)=1;
        %back end
        dt=t0s(k+1)-t0s(k);
        ta=t0s(k);
        tb=ta+.5*(1-transpct)*dt;
        tc=tb+transpct*dt;
        %td=t0s(k+1);
        ind=between(ta,tb,t,1);% (ta<=t) && (t<tb);
        if(ind~=0)
            w(ind,k)=1;
        end
        ind=between(tb,tc,t,1);%(tb<=t) && (t<tc);
        w(ind,k)=.5+.5*cos(pi*(t(ind)-tb)/(tc-tb));
    elseif(k==nw)
        ind=t>=t0s(k);
        w(ind,k)=1;
        %front end
        dt=t0s(k)-t0s(k-1);
        ta=t0s(k-1);
        tb=ta+.5*(1-transpct)*dt;
        tc=tb+transpct*dt;
        td=t0s(k);
        ind=between(tc,td,t,1);%(tc<=t) && (t<td);
        if(ind~=0)
            w(ind,k)=1;
        end
        ind=between(tb,tc,t,1);%(tb<=t) && (t<tc);
        w(ind,k)=.5+.5*cos(pi*(t(ind)-tc)/(tc-tb));
    else
        %front end
        dt=t0s(k)-t0s(k-1);
        ta=t0s(k-1);
        tb=ta+.5*(1-transpct)*dt;
        tc=tb+transpct*dt;
        td=t0s(k);
        ind=between(tc,td,t,1);%(tc<=t) && (t<td);
        if(ind~=0)
            w(ind,k)=1;
        end
        ind=between(tb,tc,t,1);%(tb<=t) && (t<tc);
        w(ind,k)=.5+.5*cos(pi*(t(ind)-tc)/(tc-tb));
        %back end
        dt=t0s(k+1)-t0s(k);
        ta=t0s(k);
        tb=ta+.5*(1-transpct)*dt;
        tc=tb+transpct*dt;
        %td=t0s(k+1);
        ind=between(ta,tb,t,1);% (ta<=t) && (t<tb);
        if(ind~=0)
            w(ind,k)=1;
        end
        ind=between(tb,tc,t,1);%(tb<=t) && (t<tc);
        w(ind,k)=.5+.5*cos(pi*(t(ind)-tb)/(tc-tb));
    end
    
    
end



end
