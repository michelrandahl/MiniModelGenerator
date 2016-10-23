#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net
  mkdir .paket
  cd .paket
  wget https://github.com/fsprojects/Paket/releases/download/3.19.8/paket.bootstrapper.exe
  cd ..

  .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  packages/FAKE/tools/FAKE.exe $@ --fsiargs build.fsx
else
  # use mono
  mkdir .paket
  cd .paket
  wget https://github.com/fsprojects/Paket/releases/download/3.19.8/paket.bootstrapper.exe
  cd ..

  mono .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  mono .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi
  mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
fi
