VERSION:=0.0.1
PACKAGE_NAME:=blanket-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	cp -r ../blanket/* $@
	sed -re "s/VERSION/$(VERSION)/" $@/blanket-package-template.el > $@/blanket-pkg.el

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)
