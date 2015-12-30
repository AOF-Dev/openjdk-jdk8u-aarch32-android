#!/bin/sh

set -v

CFLAGS="--sysroot=$RASPI_ROOT"  \
CXXFLAGS="--sysroot=$RASPI_ROOT"  \
LDFLAGS="--sysroot=$RASPI_ROOT"  \
BUILD_LD=gcc \
make \
    OBJCOPY=/opt/raspi/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/bin/arm-linux-gnueabihf-objcopy \
    STRIP=/opt/raspi/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/bin/arm-linux-gnueabihf-strip \
    POST_STRIP_CMD="/opt/raspi/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/bin/arm-linux-gnueabihf-strip -g" \
	BUILD_LD=gcc \
	"$@"
