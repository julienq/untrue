GHC = ghc
RM = rm -f
CFLAGS = -Weverything -Werror

all:	untrue

untruec:	untruec.hs
	$(GHC) $<
	@$(RM) untruec.hi untruec.o

%.uc:	%.u all
	./untruec < $< > $@

.PHONY:	clean realclean
clean:
	-$(RM) untruec runtrue untrue

realclean: clean
	-$(RM) *.uc
