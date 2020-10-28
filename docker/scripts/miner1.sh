#!/bin/sh
# readlink on OS X does not behave as on Linux
# http://stackoverflow.com/questions/1055671/how-can-i-get-the-behavior-of-gnus-readlink-f-on-a-mac
if [ `uname -s` = "Darwin" ]; then
  CWD=`pwd`
  TARGET_FILE=$0

  cd "`dirname "$TARGET_FILE"`"
  TARGET_FILE=`basename "$TARGET_FILE"`

  # Iterate down a (possible) chain of symlinks
  while [ -L "$TARGET_FILE" ]
  do
    TARGET_FILE=`readlink "$TARGET_FILE"`
    cd "`dirname "$TARGET_FILE"`"
    TARGET_FILE=`basename "$TARGET_FILE"`
  done
  unset -v TARGET_FILE

  # Compute the canonicalized name by finding the physical path
  # for the directory we're in and appending the target file.
  ABSSP=`pwd -P`
  cd $CWD
  unset -v CWD
else
  ABSS=`readlink -f $0`
  ABSSP=`dirname $ABSS`
  unset -v ABSS
fi

L=$(find "${ABSSP}/../" -name *.so -print0 | xargs -0 -n1 dirname | sort -u)
L=$(for d in $L; do echo -n "$(cd "$d"; pwd):"; done)
L="${L%:}"
if [ `uname -s` = "Darwin" ]; then
  DYLD_LIBRARY_PATH=${L}${DYLD_LIBRARY_PATH:+:}${DYLD_LIBRARY_PATH}
else
  LD_LIBRARY_PATH=${L}${LD_LIBRARY_PATH:+:}${LD_LIBRARY_PATH}
fi
unset -v L

RUNCMD="${ABSSP}/sharemind-server --conf ${ABSSP}/miner1.cfg $*"
if [ `uname -s` = "Darwin" ]; then
  echo "Running: DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH} ${RUNCMD}"; echo
  DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH} ${RUNCMD}
else
  echo "Running: LD_LIBRARY_PATH=${LD_LIBRARY_PATH} ${RUNCMD}"; echo
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH} ${RUNCMD}
fi
echo "Miner process exited with status $?"
