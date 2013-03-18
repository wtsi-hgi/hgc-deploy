#!/bin/bash

# Cleans up an overlay in order to allow it to be used in the building
# of a new image. We remove various files which may have been left
# behind by the build process.

clean[1]="rootfs/etc/passwd"
clean[2]="rootfs/etc/shadow"
clean[3]="rootfs/etc/systemd/system/console-getty.service"
clean[4]="config"
clean[5]="fstab"
clean[6]="rootfs/var/cache"
clean[7]="rootfs/home/*"

root=$1

if [ -e $1 ]; then
	for file in "${clean[@]}"; do
		echo "Cleaning $root/$file"
		rm -rf $root/$file
	done
else
	echo "Root dir $root does not exist"
fi