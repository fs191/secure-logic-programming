# Build sharemind API
cd /root
apt-get update && apt-get install -y \
  bison cmake doxygen flex g++ gcc git libboost-dev libboost-filesystem-dev \
  libboost-iostreams-dev libboost-program-options-dev libboost-system-dev \
  libbz2-dev libcrypto++-dev libgmp-dev libgnutls28-dev libhdf5-dev xz-utils \
  libhiredis-dev libmpfr-dev libssl-dev m4 make nettle-dev patch pkg-config \
  z3 python-numpy curl
apt-get install -y --no-install-recommends doxygen
LD_LIBRARY_PATH=/root/build-sdk/prefix/lib
git clone https://github.com/sharemind-sdk/build-sdk.git
git checkout 972aac87d772b67122ebb20bac0c315e5bf3ac83
cd build-sdk
echo 'INCLUDE("${CMAKE_CURRENT_SOURCE_DIR}/profiles/DebianStretch.cmake" REQUIRED)' > config.local
mkdir b
cd b
cmake ..
make -j 8

# Install sharemind API
cd /root
install build-sdk/prefix/bin/* /usr/local/bin
install build-sdk/prefix/lib/* /usr/local/lib
install build-sdk/prefix/include /usr/local/include
install build-sdk/prefix/sharemind /usr/local/share
rm -rf build-sdk
