#!/bin/sh

set -v

LDFLAGS="--sysroot=$RASPI_ROOT"  \
CFLAGS="--sysroot=$RASPI_ROOT"  \
CXXFLAGS="--sysroot=$RASPI_ROOT"  \
PKG_CONFIG_PATH=$PKG_CONFIG_PATH:"$RASPI_ROOT/usr/lib/arm-linux-gnueabihf/pkgconfig"  \
PKG_CONFIG=$PWD/cross-pkg-config \
SYSROOT=$RASPI_ROOT \
bash configure  \
	CC=arm-linux-gnueabihf-gcc  \
	CXX=arm-linux-gnueabihf-g++  \
	BUILD_CC=gcc  \
	BUILD_LD=gcc  \
	--with-sys-root=$RASPI_ROOT \
	--with-freetype-include=$RASPI_ROOT/usr/include/freetype2/  \
	--with-freetype-lib=$RASPI_ROOT/usr/lib/arm-linux-gnueabihf  \
	--x-includes=$RASPI_ROOT/usr/include/  \
	--x-libraries=$RASPI_ROOT/usr/lib  \
	--disable-precompiled-headers \
	--with-extra-cflags=--sysroot=$RASPI_ROOT \
	--with-extra-cxxflags=--sysroot=$RASPI_ROOT \
	--with-extra-ldflags=--sysroot=$RASPI_ROOT \
	--openjdk-target=aarch32-linux-gnueabihf \
	"$@"

