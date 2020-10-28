# Set up the development environment
FROM fpco/stack-build:lts-16.1 AS environment

# Install Sharemind API
ADD docker/install-sharemind.sh install-sharemind.sh
RUN chmod +x install-sharemind.sh && ./install-sharemind.sh
ADD docker/scripts/* /root/sharemind/bin/
ADD docker/config/* /root/.config/sharemind/

# Install SWI-Prolog
ADD docker/install-swipl.sh install-swipl.sh
RUN chmod +x install-swipl.sh && ./install-swipl.sh

# Build lpsec
FROM environment AS build
ADD . lpsec
WORKDIR lpsec
RUN stack build --copy-bins --test
