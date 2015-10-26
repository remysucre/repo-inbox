# Makefile rules for CPSA

# Suggested CPSA flags include a memory use limit:
# CPSAFLAGS = +RTS -M512m -RTS

# Analyze protocols for shapes
%.txt:		%.scm
	$(CPSATIME) cpsa $(CPSAFLAGS) -o $@ $<

# Analyze protocols for shapes, but don't fail when CPSA does
%.txt:		%.lsp
	-$(CPSATIME) cpsa $(CPSAFLAGS) -o $@ $<

# Analyze protocols for shapes using Diffie-Hellman algebra
%.txt:		%.sch
	$(CPSATIME) cpsa -a diffie-hellman $(CPSAFLAGS) -o $@ $<

# Extract shapes
%_shapes.txt:	%.txt
	cpsashapes -o $@ $<

# Annotate shapes
%_annotations.txt:	%_shapes.txt
	cpsaannotations -o $@ $<

# Visualize output using the expanded format (default)
%.xml:		%.txt
	cpsagraph -o $@ $<

# Visualize output using the compact format
%.svg:		%.txt
	cpsagraph -c -o $@ $<

# Visualize output using the LaTeX format
%.tex:		%.txt
	cpsagraph -l -m 62 -o $@ $<

.PRECIOUS:	%.txt %_shapes.txt %_annotations.txt
