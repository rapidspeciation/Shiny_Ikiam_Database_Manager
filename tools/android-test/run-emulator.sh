#!/usr/bin/env bash
# Starts the test phone (Android 15, Pixel 7) without a window. Used by the mobile tests.
cd "$(dirname "$0")"
export JAVA_HOME=$PWD/jdk ANDROID_HOME=$PWD/sdk ANDROID_AVD_HOME=$PWD/avd
exec sdk/emulator/emulator -avd phone -no-window -no-audio -no-boot-anim -gpu swiftshader_indirect -memory 2048 -no-snapshot-save "$@"
