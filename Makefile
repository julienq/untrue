GHC = ghc
RM = rm -f
CFLAGS = -Weverything -Werror

all:	untruec untrue

untruec:	untruec.hs
	$(GHC) $<
	@$(RM) untruec.hi untruec.o

untrue:	untrue.c
	$(CC) $< -o $@

%.uc:	%.u all
	./untruec < $< > $@

.PHONY:	clean realclean
clean:
	-$(RM) untruec untrue

realclean: clean
	-$(RM) *.uc
