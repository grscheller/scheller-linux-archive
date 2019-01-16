PATH_STDS := src/standards
PROGS_STDS := sysLimits

PROGS_STDS_FULL := $(addprefix $(PATH_STDS)/,$(PROGS_STDS))

standards: $(PROGS_STDS_FULL)

$(PATH_STDS)/sysLimits: $(PATH_STDS)/sysLimits.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATH_STDS)/sysLimits.c: $(PATH_STDS)/genSysLimits.awk \
	                      $(PATH_STDS)/sysConf.sym \
                          $(PATH_STDS)/pathConf.sym
	cd $(PATH_STDS); $(AWK) -f $(notdir $<) > $(notdir $@)

cleanstandards:
	rm -f $(PROGS_STDS_FULL) $(PATH_STDS)/sysLimits.c
