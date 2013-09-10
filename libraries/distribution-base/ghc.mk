libraries/distribution-base_PACKAGE = distribution-base
libraries/distribution-base_dist-install_GROUP = libraries
$(if $(filter distribution-base,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/distribution-base,dist-boot,0)))
$(if $(filter distribution-base,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/distribution-base,dist-install,1)))
$(if $(filter distribution-base,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/distribution-base,dist-install,2)))
