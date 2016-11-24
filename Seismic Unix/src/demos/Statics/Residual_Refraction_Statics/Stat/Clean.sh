#! /bin/sh
# clean directory

rm -f refr_file.b topo_file.b blvl_file.b vel_file.b
rm -f data.su data_res.su data_res_nmo.su
rm -f data_stk.su data_stk_bulk.su
rm -f num.b tcdp.b stkpar surfmaker
rm -f cmp_11000_raw.ps cmp_11000_rrs.ps
rm -f stk.ps stk_bulk.ps INSTALL

cd MakeData; Clean.sh

exit 0
