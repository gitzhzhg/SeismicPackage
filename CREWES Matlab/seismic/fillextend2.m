function [xp2,tp2]=fillextend2(xp,tp,xseis,choice)

tp2=tp;
xp2=xp;

if(strcmp(choice,'fill')||strcmp(choice,'fill and extend'))
    %fill interior holes
    ind=find(~isnan(tp));
    xmin=min(xp(ind));xmax=max(xp(ind));
    ind2=between(xmin,xmax,xseis,2);
    tp2=interp1(xp(ind),tp(ind),xseis(ind2),'linear','extrap');
    xp2=xseis(ind2);
end
if(strcmp(choice,'extend')||strcmp(choice,'fill and extend'));
    
    ind=find(xseis<xp(1));
    if(~isempty(ind))
        xp2=[xseis(ind);xp2];
        tp2=[tp2(1)*ones(length(ind),1);tp2];
    end
    ind=find(xseis>xp2(end));
    if(~isempty(ind))
        xp2=[xp2;xseis(ind)];
        tp2=[tp2;tp2(end)*ones(length(ind),1)];
    end
end