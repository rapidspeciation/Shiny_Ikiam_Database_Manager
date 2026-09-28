#!/usr/bin/env bash
# One-time setup of the Android test phone (Linux with KVM): a Java runtime, the
# Android command-line tools, the emulator, and an Android 15 image with Chrome and
# Gboard, all under ~/.local/share/android-test (about 3 GB). Then a Pixel 7 without
# a hardware keyboard, so the on-screen keyboard appears as on a phone.
set -euo pipefail
DIR=~/.local/share/android-test
mkdir -p "$DIR" && cd "$DIR"
if [ ! -x jdk/bin/java ]; then
  curl -sL -o jdk.tar.gz "https://api.adoptium.net/v3/binary/latest/17/ga/linux/x64/jdk/hotspot/normal/eclipse"
  mkdir -p jdk && tar -xzf jdk.tar.gz -C jdk --strip-components=1 && rm jdk.tar.gz
fi
export JAVA_HOME=$DIR/jdk ANDROID_HOME=$DIR/sdk ANDROID_AVD_HOME=$DIR/avd
if [ ! -x sdk/cmdline-tools/latest/bin/sdkmanager ]; then
  zip=$(curl -s https://dl.google.com/android/repository/repository2-3.xml | grep -o 'commandlinetools-linux-[0-9]*_latest.zip' | sort -t- -k3 -n | tail -1)
  curl -sL -o clt.zip "https://dl.google.com/android/repository/$zip"
  mkdir -p sdk/cmdline-tools && python3 -c "import zipfile;zipfile.ZipFile('clt.zip').extractall('sdk/cmdline-tools')"
  mv sdk/cmdline-tools/cmdline-tools sdk/cmdline-tools/latest && chmod +x sdk/cmdline-tools/latest/bin/* && rm clt.zip
fi
yes | sdk/cmdline-tools/latest/bin/sdkmanager --licenses >/dev/null 2>&1 || true
yes | sdk/cmdline-tools/latest/bin/sdkmanager --install platform-tools emulator "system-images;android-35;google_apis_playstore;x86_64"
mkdir -p avd
echo no | sdk/cmdline-tools/latest/bin/avdmanager create avd -n phone -k "system-images;android-35;google_apis_playstore;x86_64" -d pixel_7 --force
sed -i 's/^hw.keyboard=.*/hw.keyboard=no/; s/^hw.ramSize=.*/hw.ramSize=2048M/' avd/phone.avd/config.ini
cp "$(dirname "$0")/run-emulator.sh" "$(dirname "$0")/tap-text.sh" "$DIR/" 2>/dev/null || true
echo "Ready. Start the phone with: $DIR/run-emulator.sh &"
