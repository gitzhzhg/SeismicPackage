# Makefile for ...cwp/lib

include $(CWPROOT)/src/Makefile.config

D = $I/cwp.h

LIB = $L/libcwp.a

ARCH =			\
	$(LIB)(abel.o)	\
	$(LIB)(airy.o)	\
	$(LIB)(alloc.o)	\
	$(LIB)(antialias.o)	\
	$(LIB)(axb.o)	\
	$(LIB)(bigmatrix.o)	\
	$(LIB)(butterworth.o)	\
	$(LIB)(complex.o)	\
	$(LIB)(complexd.o)	\
	$(LIB)(complexf.o)	\
	$(LIB)(complexff.o)	\
	$(LIB)(conjugate_gradient.o)	\
	$(LIB)(convolution.o)	\
	$(LIB)(cubicspline.o)	\
	$(LIB)(dblas.o)	\
	$(LIB)(dge.o)	\
	$(LIB)(differentiate.o)	\
	$(LIB)(dintlin.o)	\
	$(LIB)(exit.o)	\
	$(LIB)(frannor.o)	\
	$(LIB)(franuni.o)	\
	$(LIB)(hankel.o)	\
	$(LIB)(hartley.o)	\
	$(LIB)(hilbert.o)	\
	$(LIB)(holbergd1.o)	\
	$(LIB)(intcub.o)	\
	$(LIB)(intl2b.o)	\
	$(LIB)(intlin.o)	\
	$(LIB)(intlinc.o)	\
	$(LIB)(intlirr2b.o)	\
	$(LIB)(intsinc8.o)	\
	$(LIB)(inttable8.o)	\
	$(LIB)(linfit.o)	\
	$(LIB)(linregress.o)	\
	$(LIB)(linfit.o)	\
	$(LIB)(maxmin.o)	\
	$(LIB)(mkdiff.o)	\
	$(LIB)(mkhdiff.o)	\
	$(LIB)(mksinc.o)	\
	$(LIB)(mnewt.o)	\
	$(LIB)(pfafft.o)	\
	$(LIB)(dpfafft.o)	\
	$(LIB)(orthpolynomials.o)	\
	$(LIB)(polar.o)	\
	$(LIB)(printerplot.o)	\
	$(LIB)(quest.o)	\
	$(LIB)(ressinc8.o)	\
	$(LIB)(rfwtva.o)	\
	$(LIB)(rfwtvaint.o)	\
	$(LIB)(sblas.o)	\
	$(LIB)(scaxis.o)	\
	$(LIB)(sge.o)	\
	$(LIB)(svd.o)	\
	$(LIB)(shfs8r.o)	\
	$(LIB)(sinc.o)	\
	$(LIB)(sort.o)	\
	$(LIB)(sqr.o)	\
	$(LIB)(stoep.o)	\
	$(LIB)(strstuff.o)	\
	$(LIB)(swapbyte.o)	\
	$(LIB)(symmeigen.o)	\
	$(LIB)(tridiagonal.o)	\
	$(LIB)(tempname.o)	\
	$(LIB)(unwrapphase.o)	\
	$(LIB)(vandermonde.o)	\
	$(LIB)(waveforms.o)	\
	$(LIB)(windows.o)	\
	$(LIB)(wrap.o)	\
	$(LIB)(xcor.o)	\
	$(LIB)(xindex.o)	\
	$(LIB)(yclip.o)	\
	$(LIB)(yxtoxy.o)	\
	$(LIB)(zasc.o)		\
	$(LIB)(zebc.o)

INSTALL:	$(LIB) $L
	@-rm -f INSTALL
	@touch $@

$(ARCH)	: $D

$(LIB)	:	$(ARCH)
	$(RANLIB) $(LIB)

remake	:
	@-rm -f $(LIB) INSTALL
	@$(MAKE) INSTALL

list	:
	$(AR) tv $(LIB)

clean::
	rm -f junk* core a.out

.PRECIOUS:	 $(LIB)
