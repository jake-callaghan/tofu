# Dynamic config
HOST := $(shell uname -s)-$(shell uname -m)

ifeq ($(HOST),Linux-i686)
  _CC = gcc
endif

ifeq ($(HOST),Linux-x86_64)
  _CC = gcc -m32
endif

ifeq ($(HOST),Darwin-i386)
  _CC = gcc -m32
endif

ifeq ($(HOST),Darwin-x86_64)
  _CC = gcc -m32
endif

ifeq ($(HOST),Linux-armv6l)
  _CC = gcc
endif

ifeq ($(HOST),Linux-armv7l)
  _CC = gcc
endif

ifndef _CC
$(error Can't configure for host type $(HOST))
endif

CC = $(_CC)
