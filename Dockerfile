# Set up the development environment
FROM fpco/stack-build:lts-16.1 AS environment

# Install Sharemind API
WORKDIR /root
RUN apt-get update && apt-get install -y \
      bison cmake doxygen flex g++ gcc git libboost-dev libboost-filesystem-dev \
      libboost-iostreams-dev libboost-program-options-dev libboost-system-dev \
      libbz2-dev libcrypto++-dev libgmp-dev libgnutls28-dev libhdf5-dev xz-utils \
      libhiredis-dev libmpfr-dev libssl-dev m4 make nettle-dev patch pkg-config \
      z3 python-numpy curl
RUN apt-get install -y --no-install-recommends doxygen
#RUN LD_LIBRARY_PATH=/root/build-sdk/prefix/lib
ENV SHAREMIND_STDLIB_PATH /usr/lib/sharemind/stdlib
RUN git clone https://github.com/sharemind-sdk/build-sdk.git
WORKDIR build-sdk
RUN git checkout 972aac87d772b67122ebb20bac0c315e5bf3ac83
RUN echo 'INCLUDE("${CMAKE_CURRENT_SOURCE_DIR}/profiles/DebianStretch.cmake" REQUIRED)' > config.local
RUN mkdir b
WORKDIR b
RUN cmake ..
RUN make -j 8

# Install sharemind API
WORKDIR /root
ADD docker/scripts/* /usr/bin/
ADD docker/config/* /root/.config/sharemind/
RUN cp -r build-sdk/prefix/bin/* /usr/bin
RUN cp -r build-sdk/prefix/lib/* /usr/lib
RUN cp -r build-sdk/prefix/include/* /usr/include
RUN cp -r build-sdk/prefix/share/* /usr/share
RUN rm -rf build-sdk
ADD SecreC/lp_essentials.sc /usr/lib/sharemind/stdlib

# Install SWI-Prolog
#ADD docker/install-swipl.sh install-swipl.sh
#RUN chmod +x install-swipl.sh && ./install-swipl.sh
RUN add-apt-repository ppa:swi-prolog/stable -y
RUN sudo apt-get update
RUN sudo apt-get install swi-prolog -y

# Build lpsec
FROM environment AS build
WORKDIR /opt/build
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only
WORKDIR /root/lpsec
ADD . .
RUN stack install
CMD stack test
