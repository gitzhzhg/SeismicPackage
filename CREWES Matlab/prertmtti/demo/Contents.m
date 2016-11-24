% main: input the migration parameters and apply the migration according to
% requirement

shotsfile='singleshot.sgy';

velfile='vels.bin';

delfile='deltas.bin';

epsilonfile='epsilons.bin';

phifile='thetas.bin';

migfile='ttimig.bin';

dx=20;

dz=10;

nxo=456;

nz=200;

nxshot=1;

lpad=20;

rpad=20;

fpeak=25;

ixshot_start=1;

%tti migration
premigrtmtti(shotsfile, velfile, epsilonfile, delfile, phifile, ...
              dx, dz, nxo, nz, nxshot,lpad, rpad, fpeak, ixshot_start, migfile);         

plotfile(migfile, nxo, nz, dx, dz);