GHC = ghc
RM = rm -f
CFLAGS = -Wall

all:	untruec untrue

untruec:	untruec.hs
	$(GHC) $<
	@$(RM) untruec.hi untruec.o

untrue:	untrue.c
	$(CC) $< -o $@

.PHONY:	clean
clean:
	-$(RM) untruec untrue
