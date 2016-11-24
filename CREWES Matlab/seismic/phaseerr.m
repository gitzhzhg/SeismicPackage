function err=phaseerr(ph1,ph2)
% PHASEERR: compute the error between two phase angles in degrees (quadrant sensitive)
%
% err=phaseerr(ph1,ph2)
%
% ph1 ... first phase angle between -180 and 180
% ph2 ... second phase angle between -180 and 180.
%

if(ph1>=0 && ph1<90)
    Q1=1;
elseif(ph1>=90 && ph1<=180)
    Q1=2;
elseif(ph1<0 && ph1>=-90)
    Q1=4;
else
    Q1=3;
end

if(ph2>=0 && ph2<90)
    Q2=1;
elseif(ph2>=90 && ph2<=180)
    Q2=2;
elseif(ph2<0 && ph2>=-90)
    Q2=4;
else
    Q2=3;
end

if(Q1==2 && Q2==3)
    err=abs(ph1-ph2-360);
elseif(Q1==3 && Q2==2)
    err=abs(ph1+360-ph2);
elseif(Q1==1 && Q2==3)
    err=abs(ph2+360-ph1);
elseif(Q1==3 && Q2==1)
    err=abs(ph1+360-ph2);
elseif(Q1==1 && Q2==4)
    err=abs(ph1-ph2);
elseif(Q2==1 && Q1==4)
    err=abs(ph1-ph2);
else
    err=abs(ph1-ph2);
end